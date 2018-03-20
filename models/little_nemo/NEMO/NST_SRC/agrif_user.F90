#if defined key_agrif
   !!----------------------------------------------------------------------
   !! NEMO/NST 3.3 , NEMO Consortium (2010)
   !! $Id: agrif_user.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   SUBROUTINE agrif_before_regridding
   END SUBROUTINE

   SUBROUTINE Agrif_InitWorkspace
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitWorkspace ***
      !!----------------------------------------------------------------------
      USE par_oce
      USE dom_oce
      USE Agrif_Util
      USE nemogcm
      !
      IMPLICIT NONE
      !!----------------------------------------------------------------------
      !
      IF( .NOT. Agrif_Root() ) THEN
         jpni = Agrif_Parent(jpni)
         jpnj = Agrif_Parent(jpnj)
         jpnij = Agrif_Parent(jpnij)
         jpiglo  = nbcellsx + 2 + 2*nbghostcells
         jpjglo  = nbcellsy + 2 + 2*nbghostcells
         jpi     = ( jpiglo-2*jpreci + (jpni-1+0) ) / jpni + 2*jpreci
         jpj     = ( jpjglo-2*jprecj + (jpnj-1+0) ) / jpnj + 2*jprecj
         jpk     = jpkdta
         jpim1   = jpi-1
         jpjm1   = jpj-1
         jpkm1   = jpk-1                                        
         jpij    = jpi*jpj
         jpidta  = jpiglo
         jpjdta  = jpjglo
         jpizoom = 1
         jpjzoom = 1
         nperio  = 0
         jperio  = 0
      ENDIF
      !
   END SUBROUTINE Agrif_InitWorkspace


   SUBROUTINE Agrif_InitValues
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues ***
      !!
      !! ** Purpose :: Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
      USE Agrif_Util
      USE oce 
      USE dom_oce
      USE nemogcm
      USE tradmp
      USE obc_par
      USE bdy_par

      IMPLICIT NONE
      !!----------------------------------------------------------------------

      ! 0. Initializations
      !-------------------
#if defined key_orca_r025 || defined key_orca_r05 || defined key_orca_r2 || defined key_orca_r4
      jp_cfg = -1    ! set special value for jp_cfg on fine grids
      cp_cfg = "default"
#endif

      ! Specific fine grid Initializations
      ! no tracer damping on fine grids
      ln_tradmp = .FALSE.
      ! no open boundary on fine grids
      lk_obc = .FALSE.
      lk_bdy = .FALSE.

      CALL nemo_init  ! Initializations of each fine grid
      CALL agrif_nemo_init
# if ! defined key_offline
      CALL Agrif_InitValues_cont
# endif       
# if defined key_top
      CALL Agrif_InitValues_cont_top
# endif      
   END SUBROUTINE Agrif_initvalues

# if ! defined key_offline

   SUBROUTINE Agrif_InitValues_cont
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues_cont ***
      !!
      !! ** Purpose ::   Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
      USE Agrif_Util
      USE oce 
      USE dom_oce
      USE nemogcm
      USE sol_oce
      USE in_out_manager
      USE agrif_opa_update
      USE agrif_opa_interp
      USE agrif_opa_sponge
      !
      IMPLICIT NONE
      !
#  include "domzgr_substitute.h90"  
      !
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE :: tabtstemp
      REAL(wp), DIMENSION(:,:,:  ), ALLOCATABLE :: tabuvtemp
      LOGICAL :: check_namelist
      INTEGER :: ji,jj,jk
      !!----------------------------------------------------------------------

      ALLOCATE( tabtstemp(jpi, jpj, jpk, jpts) )
      ALLOCATE( tabuvtemp(jpi, jpj, jpk)       )


      ! 1. Declaration of the type of variable which have to be interpolated
      !---------------------------------------------------------------------
      CALL agrif_declare_var

      ! 2. First interpolations of potentially non zero fields
      !-------------------------------------------------------
      Agrif_SpecialValue=0.
      Agrif_UseSpecialValue = .TRUE.
      Call Agrif_Bc_variable(tabtstemp,tsn_id,calledweight=1.,procname=interptsn)
      Call Agrif_Bc_variable(tabtstemp,tsa_id,calledweight=1.,procname=interptsn)

      Call Agrif_Bc_variable(tabuvtemp,un_id,calledweight=1.,procname=interpu)
      Call Agrif_Bc_variable(tabuvtemp,vn_id,calledweight=1.,procname=interpv)
      Call Agrif_Bc_variable(tabuvtemp,ua_id,calledweight=1.,procname=interpun)
      Call Agrif_Bc_variable(tabuvtemp,va_id,calledweight=1.,procname=interpvn)
      Agrif_UseSpecialValue = .FALSE.

      ! 3. Some controls
      !-----------------
      check_namelist = .true.
            
      IF( check_namelist ) THEN
     
         ! Check time steps           
         IF( NINT(Agrif_Rhot()) * nint(rdt) /= Agrif_Parent(rdt) ) THEN
            WRITE(*,*) 'incompatible time step between grids'
            WRITE(*,*) 'parent grid value : ',Agrif_Parent(rdt)
            WRITE(*,*) 'child  grid value : ',nint(rdt)
            WRITE(*,*) 'value on parent grid should be : ',rdt*Agrif_Rhot()
            STOP
         ENDIF
         
         ! Check run length
         IF( Agrif_IRhot() * (Agrif_Parent(nitend)- &
            Agrif_Parent(nit000)+1) .ne. (nitend-nit000+1) ) THEN
            WRITE(*,*) 'incompatible run length between grids'
            WRITE(*,*) 'parent grid value : ',(Agrif_Parent(nitend)- &
               Agrif_Parent(nit000)+1),' time step'
            WRITE(*,*) 'child  grid value : ', &
               (nitend-nit000+1),' time step'
            WRITE(*,*) 'value on child grid should be : ', &
               Agrif_IRhot() * (Agrif_Parent(nitend)- &
               Agrif_Parent(nit000)+1)
            STOP
         ENDIF
         
         ! Check coordinates
         IF( ln_zps ) THEN
            ! check parameters for partial steps 
            IF( Agrif_Parent(e3zps_min) .ne. e3zps_min ) THEN
               WRITE(*,*) 'incompatible e3zps_min between grids'
               WRITE(*,*) 'parent grid :',Agrif_Parent(e3zps_min)
               WRITE(*,*) 'child grid  :',e3zps_min
               WRITE(*,*) 'those values should be identical'
               STOP
            ENDIF          
            IF( Agrif_Parent(e3zps_rat) /= e3zps_rat ) THEN
               WRITE(*,*) 'incompatible e3zps_rat between grids'
               WRITE(*,*) 'parent grid :',Agrif_Parent(e3zps_rat)
               WRITE(*,*) 'child grid  :',e3zps_rat
               WRITE(*,*) 'those values should be identical'                  
               STOP
            ENDIF
         ENDIF
      ENDIF
       
      CALL Agrif_Update_tra(0)
      CALL Agrif_Update_dyn(0)

      nbcline = 0
      !
      DEALLOCATE(tabtstemp)
      DEALLOCATE(tabuvtemp)
      !
   END SUBROUTINE Agrif_InitValues_cont


   SUBROUTINE agrif_declare_var
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_declarE_var ***
      !!
      !! ** Purpose :: Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
      USE agrif_util
      USE par_oce       !   ONLY : jpts
      USE oce
      IMPLICIT NONE
      !!----------------------------------------------------------------------
   
      ! 1. Declaration of the type of variable which have to be interpolated
      !---------------------------------------------------------------------
      CALL agrif_declare_variable((/2,2,0,0/),(/3,3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jpts/),tsn_id)
      CALL agrif_declare_variable((/2,2,0,0/),(/3,3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jpts/),tsa_id)
      CALL agrif_declare_variable((/2,2,0,0/),(/3,3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jpts/),tsb_id)

      CALL agrif_declare_variable((/2,2,0/),(/3,3,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/),avt_id)
      CALL agrif_declare_variable((/2,2,0/),(/3,3,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/),avm_id)
      CALL agrif_declare_variable((/1,2,0/),(/2,3,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/),avmu_id)
      CALL agrif_declare_variable((/2,1,0/),(/3,2,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/),avmv_id)

      CALL agrif_declare_variable((/1,2,0/),(/2,3,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/),un_id)
      CALL agrif_declare_variable((/2,1,0/),(/3,2,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/),vn_id)
      CALL agrif_declare_variable((/1,2,0/),(/2,3,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/),ua_id)
      CALL agrif_declare_variable((/2,1,0/),(/3,2,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/),va_id)

      CALL agrif_declare_variable((/2,2,0/),(/3,3,0/),(/'x','y','N'/),(/1,1,1/),(/jpi,jpj,jpk/),e3t_id)
   
      CALL agrif_declare_variable((/1,2/),(/2,3/),(/'x','y'/),(/1,1/),(/jpi,jpj/),e1u_id)
      CALL agrif_declare_variable((/2,1/),(/3,2/),(/'x','y'/),(/1,1/),(/jpi,jpj/),e2v_id)

      CALL agrif_declare_variable((/2,2/),(/3,3/),(/'x','y'/),(/1,1/),(/jpi,jpj/),sshn_id)
      CALL agrif_declare_variable((/2,2/),(/3,3/),(/'x','y'/),(/1,1/),(/jpi,jpj/),gcb_id)
       
      CALL agrif_declare_variable((/2,2/),(/3,3/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),glamt_id)
      CALL agrif_declare_variable((/2,2/),(/3,3/),(/'x','y'/),(/1,1/),(/nlci,nlcj/),gphit_id)

      ! 2. Type of interpolation
      !-------------------------
      CALL Agrif_Set_bcinterp(tsn_id,interp=AGRIF_linear)
      CALL Agrif_Set_bcinterp(tsa_id,interp=AGRIF_linear)

      CALL Agrif_Set_bcinterp(avt_id,interp=AGRIF_linear)
      CALL Agrif_Set_bcinterp(avm_id,interp=AGRIF_linear)
      CALL Agrif_Set_bcinterp(avmu_id,interp=AGRIF_linear)
      CALL Agrif_Set_bcinterp(avmv_id,interp=AGRIF_linear)

      Call Agrif_Set_bcinterp(un_id,interp1=Agrif_linear,interp2=AGRIF_ppm)
      Call Agrif_Set_bcinterp(vn_id,interp1=AGRIF_ppm,interp2=Agrif_linear)

      Call Agrif_Set_bcinterp(ua_id,interp1=Agrif_linear,interp2=AGRIF_ppm)
      Call Agrif_Set_bcinterp(va_id,interp1=AGRIF_ppm,interp2=Agrif_linear)

      Call Agrif_Set_bcinterp(e3t_id,interp=AGRIF_constant)

      Call Agrif_Set_bcinterp(e1u_id,interp1=Agrif_linear,interp2=AGRIF_ppm)
      Call Agrif_Set_bcinterp(e2v_id,interp1=AGRIF_ppm,interp2=Agrif_linear)

      ! 3. Location of interpolation
      !-----------------------------
      Call Agrif_Set_bc(un_id,(/0,1/)) ! if west: column 1 and 2 
      Call Agrif_Set_bc(vn_id,(/0,1/))

      Call Agrif_Set_bc(e3t_id,(/-3*Agrif_irhox(),0/))   ! if west and rhox=3: column 2 to 11

      Call Agrif_Set_bc(e1u_id,(/0,0/))
      Call Agrif_Set_bc(e2v_id,(/0,0/))

      Call Agrif_Set_bc(tsn_id,(/0,1/)) ! if west: column 1 and 2 
      Call Agrif_Set_bc(tsa_id,(/-3*Agrif_irhox(),0/))

      Call Agrif_Set_bc(avt_id,(/0,1/))
      Call Agrif_Set_bc(avm_id,(/0,1/))
      Call Agrif_Set_bc(avmu_id,(/0,1/))
      Call Agrif_Set_bc(avmv_id,(/0,1/))

      Call Agrif_Set_bc(ua_id,(/-2*Agrif_irhox(),0/))
      Call Agrif_Set_bc(va_id,(/-2*Agrif_irhox(),0/))

      ! 5. Update type
      !--------------- 
      Call Agrif_Set_Updatetype(tsn_id, update = AGRIF_Update_Average)
      Call Agrif_Set_Updatetype(tsb_id, update = AGRIF_Update_Average)

      Call Agrif_Set_Updatetype(sshn_id, update = AGRIF_Update_Average)
      Call Agrif_Set_Updatetype(gcb_id, update = AGRIF_Update_Average)

      Call Agrif_Set_Updatetype(un_id,update1 = Agrif_Update_Copy, update2 = Agrif_Update_Average)
      Call Agrif_Set_Updatetype(vn_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy)

      Call Agrif_Set_Updatetype(e1u_id,update1 = Agrif_Update_Copy, update2=Agrif_Update_Average)
      Call Agrif_Set_Updatetype(e2v_id,update1 = Agrif_Update_Average, update2=Agrif_Update_Copy)

      CALL Agrif_Set_Updatetype(glamt_id, update = AGRIF_Update_Average)
      CALL Agrif_Set_Updatetype(gphit_id, update = AGRIF_Update_Average)

      CALL Agrif_Set_Updatetype(avt_id, update = AGRIF_Update_Average)
      CALL Agrif_Set_Updatetype(avm_id, update = AGRIF_Update_Average)
      CALL Agrif_Set_Updatetype(avmu_id, update = AGRIF_Update_Average)
      CALL Agrif_Set_Updatetype(avmv_id, update = AGRIF_Update_Average)

   END SUBROUTINE agrif_declare_var
# endif
   
# if defined key_top
   SUBROUTINE Agrif_InitValues_cont_top
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues_cont_top ***
      !!
      !! ** Purpose :: Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
      USE Agrif_Util
      USE oce 
      USE dom_oce
      USE nemogcm
      USE trc
      USE in_out_manager
      USE agrif_top_update
      USE agrif_top_interp
      USE agrif_top_sponge
      !
      IMPLICIT NONE
      !
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE :: tabtrtemp
      LOGICAL :: check_namelist
      !!----------------------------------------------------------------------

      ALLOCATE( tabtrtemp(jpi,jpj,jpk,jptra) )
      
      
      ! 1. Declaration of the type of variable which have to be interpolated
      !---------------------------------------------------------------------
      CALL agrif_declare_var_top

      ! 2. First interpolations of potentially non zero fields
      !-------------------------------------------------------
      Agrif_SpecialValue=0.
      Agrif_UseSpecialValue = .TRUE.
      Call Agrif_Bc_variable(tabtrtemp,trn_id,calledweight=1.)
      Call Agrif_Bc_variable(tabtrtemp,tra_id,calledweight=1.,procname=interptrn)
      Agrif_UseSpecialValue = .FALSE.

      ! 3. Some controls
      !-----------------
      check_namelist = .true.
            
      IF( check_namelist ) THEN
#  if defined offline     
         ! Check time steps
         IF( nint(Agrif_Rhot()) * nint(rdt) .ne. Agrif_Parent(rdt) ) THEN
            WRITE(*,*) 'incompatible time step between grids'
            WRITE(*,*) 'parent grid value : ',Agrif_Parent(rdt)
            WRITE(*,*) 'child  grid value : ',nint(rdt)
            WRITE(*,*) 'value on parent grid should be : ',rdt*Agrif_Rhot()
            STOP
         ENDIF

         ! Check run length
         IF( Agrif_IRhot() * (Agrif_Parent(nitend)- &
            Agrif_Parent(nit000)+1) .ne. (nitend-nit000+1) ) THEN
            WRITE(*,*) 'incompatible run length between grids'
            WRITE(*,*) 'parent grid value : ',(Agrif_Parent(nitend)- &
               Agrif_Parent(nit000)+1),' time step'
            WRITE(*,*) 'child  grid value : ', &
               (nitend-nit000+1),' time step'
            WRITE(*,*) 'value on child grid should be : ', &
               Agrif_IRhot() * (Agrif_Parent(nitend)- &
               Agrif_Parent(nit000)+1)
            STOP
         ENDIF
         
         ! Check coordinates
         IF( ln_zps ) THEN
            ! check parameters for partial steps 
            IF( Agrif_Parent(e3zps_min) .ne. e3zps_min ) THEN
               WRITE(*,*) 'incompatible e3zps_min between grids'
               WRITE(*,*) 'parent grid :',Agrif_Parent(e3zps_min)
               WRITE(*,*) 'child grid  :',e3zps_min
               WRITE(*,*) 'those values should be identical'
               STOP
            ENDIF          
            IF( Agrif_Parent(e3zps_rat) .ne. e3zps_rat ) THEN
               WRITE(*,*) 'incompatible e3zps_rat between grids'
               WRITE(*,*) 'parent grid :',Agrif_Parent(e3zps_rat)
               WRITE(*,*) 'child grid  :',e3zps_rat
               WRITE(*,*) 'those values should be identical'                  
               STOP
            ENDIF
         ENDIF
#  endif         
        ! Check passive tracer cell
        IF( nn_dttrc .ne. 1 ) THEN
           WRITE(*,*) 'nn_dttrc should be equal to 1'
        ENDIF
      ENDIF
       
      CALL Agrif_Update_trc(0)
      nbcline_trc = 0
      !
      DEALLOCATE(tabtrtemp)
      !
   END SUBROUTINE Agrif_InitValues_cont_top


   SUBROUTINE agrif_declare_var_top
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE agrif_declare_var_top ***
      !!
      !! ** Purpose :: Declaration of TOP variables to be interpolated
      !!----------------------------------------------------------------------
      USE agrif_util
      USE dom_oce
      USE trc
      
      IMPLICIT NONE
   
      ! 1. Declaration of the type of variable which have to be interpolated
      !---------------------------------------------------------------------
      CALL agrif_declare_variable((/2,2,0,0/),(/3,3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jptra/),trn_id)
      CALL agrif_declare_variable((/2,2,0,0/),(/3,3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jptra/),trb_id)
      CALL agrif_declare_variable((/2,2,0,0/),(/3,3,0,0/),(/'x','y','N','N'/),(/1,1,1,1/),(/jpi,jpj,jpk,jptra/),tra_id)
#  if defined key_offline
      CALL agrif_declare_variable((/1,2/),(/2,3/),(/'x','y'/),(/1,1/),(/jpi,jpj/),e1u_id)
      CALL agrif_declare_variable((/2,1/),(/3,2/),(/'x','y'/),(/1,1/),(/jpi,jpj/),e2v_id)
#  endif
       
      ! 2. Type of interpolation
      !-------------------------
      CALL Agrif_Set_bcinterp(trn_id,interp=AGRIF_linear)
      CALL Agrif_Set_bcinterp(tra_id,interp=AGRIF_linear)
   
#  if defined key_offline
      Call Agrif_Set_bcinterp(e1u_id,interp1=Agrif_linear,interp2=AGRIF_ppm)
      Call Agrif_Set_bcinterp(e2v_id,interp1=AGRIF_ppm,interp2=Agrif_linear)
#  endif

      ! 3. Location of interpolation
      !-----------------------------
#  if defined key_offline
      Call Agrif_Set_bc(e1u_id,(/0,0/))
      Call Agrif_Set_bc(e2v_id,(/0,0/))
#  endif
      Call Agrif_Set_bc(trn_id,(/0,1/))
      Call Agrif_Set_bc(tra_id,(/-3*Agrif_irhox(),0/))

      ! 5. Update type
      !--------------- 
      Call Agrif_Set_Updatetype(trn_id, update = AGRIF_Update_Average)
      Call Agrif_Set_Updatetype(trb_id, update = AGRIF_Update_Average)

#  if defined key_offline
      Call Agrif_Set_Updatetype(e1u_id,update1 = Agrif_Update_Copy, update2=Agrif_Update_Average)
      Call Agrif_Set_Updatetype(e2v_id,update1 = Agrif_Update_Average, update2=Agrif_Update_Copy)
#  endif

   END SUBROUTINE agrif_declare_var_top
# endif
   
   SUBROUTINE Agrif_detect( kg, ksizex )
      !!----------------------------------------------------------------------
      !!   *** ROUTINE Agrif_detect ***
      !!----------------------------------------------------------------------
      USE Agrif_Types
      !
      INTEGER, DIMENSION(2) :: ksizex
      INTEGER, DIMENSION(ksizex(1),ksizex(2)) :: kg 
      !!----------------------------------------------------------------------
      !
      RETURN
      !
   END SUBROUTINE Agrif_detect


   SUBROUTINE agrif_nemo_init
      !!----------------------------------------------------------------------
      !!                     *** ROUTINE agrif_init ***
      !!----------------------------------------------------------------------
      USE agrif_oce 
      USE in_out_manager
      USE lib_mpp
      IMPLICIT NONE
      !
      NAMELIST/namagrif/ nn_cln_update, rn_sponge_tra, rn_sponge_dyn, ln_spc_dyn
      !!----------------------------------------------------------------------
      !
      REWIND( numnam )                ! Read namagrif namelist
      READ  ( numnam, namagrif )
      !
      IF(lwp) THEN                    ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'agrif_nemo_init : AGRIF parameters'
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namagrif : set AGRIF parameters'
         WRITE(numout,*) '      baroclinic update frequency       nn_cln_update = ', nn_cln_update
         WRITE(numout,*) '      sponge coefficient for tracers    rn_sponge_tra = ', rn_sponge_tra, ' s'
         WRITE(numout,*) '      sponge coefficient for dynamics   rn_sponge_tra = ', rn_sponge_dyn, ' s'
         WRITE(numout,*) '      use special values for dynamics   ln_spc_dyn    = ', ln_spc_dyn
         WRITE(numout,*) 
      ENDIF
      !
      ! convert DOCTOR namelist name into OLD names
      nbclineupdate = nn_cln_update
      visc_tra      = rn_sponge_tra
      visc_dyn      = rn_sponge_dyn
      !
      IF( agrif_oce_alloc()  > 0 )   CALL ctl_warn('agrif sol_oce_alloc: allocation of arrays failed')
      !
    END SUBROUTINE agrif_nemo_init

# if defined key_mpp_mpi

   SUBROUTINE Agrif_InvLoc( indloc, nprocloc, i, indglob )
      !!----------------------------------------------------------------------
      !!                     *** ROUTINE Agrif_detect ***
      !!----------------------------------------------------------------------
      USE dom_oce
      IMPLICIT NONE
      !
      INTEGER :: indglob, indloc, nprocloc, i
      !!----------------------------------------------------------------------
      !
      SELECT CASE( i )
      CASE(1)   ;   indglob = indloc + nimppt(nprocloc+1) - 1
      CASE(2)   ;   indglob = indloc + njmppt(nprocloc+1) - 1 
      CASE(3)   ;   indglob = indloc
      CASE(4)   ;   indglob = indloc
      END SELECT
      !
   END SUBROUTINE Agrif_InvLoc

# endif

#else
   SUBROUTINE Subcalledbyagrif
      !!----------------------------------------------------------------------
      !!                   *** ROUTINE Subcalledbyagrif ***
      !!----------------------------------------------------------------------
      WRITE(*,*) 'Impossible to be here'
   END SUBROUTINE Subcalledbyagrif
#endif
