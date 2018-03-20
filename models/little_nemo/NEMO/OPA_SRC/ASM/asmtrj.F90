MODULE asmtrj
   !!======================================================================
   !!                       ***  MODULE asmtrj  ***
   !! Assimilation trajectory interface: Write to file the background state and the model state trajectory
   !!======================================================================
   !! History :       ! 2007-03 (M. Martin)  Met. Office version
   !!                 ! 2007-04 (A. Weaver)  asm_trj_wri, original code
   !!                 ! 2007-03 (K. Mogensen)  Adapt to NEMOVAR and use IOM instead of IOIPSL
   !!                 ! 2007-04 (A. Weaver)  Name change (formally asmbkg.F90). Distinguish
   !!                                        background states in Jb term and at analysis time.
   !!                                        Include state trajectory routine (currently empty)
   !!                 ! 2007-07 (A. Weaver)  Add tke_rst and flt_rst for case nitbkg=0 
   !!                 ! 2009-03 (F. Vigilant)  Add hmlp (zdfmxl) for no tracer nmldp=2 
   !!                 ! 2009-06 (F. Vigilant) asm_trj_wri: special case when kt=nit000-1
   !!                 ! 2009-07 (F. Vigilant) asm_trj_wri: add computation of eiv at restart
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   'key_asminc' : Switch on the assimilation increment interface
   !!----------------------------------------------------------------------
   !!   asm_bkg_wri  : Write out the background state
   !!   asm_trj_wri  : Write out the model state trajectory (used with 4D-Var)
   !!----------------------------------------------------------------------
   USE oce                ! Dynamics and active tracers defined in memory
   USE sbc_oce            ! Ocean surface boundary conditions
   USE zdf_oce            ! Vertical mixing variables
   USE zdfddm             ! Double diffusion mixing parameterization
   USE ldftra_oce         ! Lateral tracer mixing coefficient defined in memory
   USE ldfslp             ! Slopes of neutral surfaces
   USE tradmp             ! Tracer damping
#if defined key_zdftke
   USE zdftke             ! TKE vertical physics
#endif
   USE eosbn2             ! Equation of state (eos_bn2 routine)
   USE zdfmxl             ! Mixed layer depth
   USE dom_oce, ONLY :   ndastp
   USE sol_oce, ONLY :   gcx   ! Solver variables defined in memory
   USE in_out_manager     ! I/O manager
   USE iom                ! I/O module
   USE asmpar             ! Parameters for the assmilation interface
   USE zdfmxl             ! mixed layer depth
#if defined key_traldf_c2d
   USE ldfeiv             ! eddy induced velocity coef.      (ldf_eiv routine)
#endif

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC   asm_bkg_wri   !: Write out the background state
   PUBLIC   asm_trj_wri   !: Write out the background state

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: asmtrj.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE asm_bkg_wri( kt )
      !!-----------------------------------------------------------------------
      !!                  ***  ROUTINE asm_bkg_wri ***
      !!
      !! ** Purpose : Write to file the background state for later use in the
      !!              inner loop of data assimilation or for direct initialization
      !!              in the outer loop.
      !!
      !! ** Method  : Write out the background state for use in the Jb term
      !!              in the cost function and for use with direct initialization
      !!              at analysis time.
      !!-----------------------------------------------------------------------
      INTEGER, INTENT( IN ) :: kt               ! Current time-step
      !
      CHARACTER (LEN=50) :: cl_asmbkg
      CHARACTER (LEN=50) :: cl_asmdin
      LOGICAL :: llok          ! Check if file exists
      INTEGER :: inum          ! File unit number
      REAL(wp) :: zdate        ! Date
      !!-----------------------------------------------------------------------

      !                                !-------------------------------------------
      IF( kt == nitbkg_r ) THEN        ! Write out background at time step nitbkg_r
         !                             !-----------------------------------========
         !
         WRITE(cl_asmbkg, FMT='(A,".nc")' ) TRIM( c_asmbkg )
         cl_asmbkg = TRIM( cl_asmbkg )
         INQUIRE( FILE = cl_asmbkg, EXIST = llok )
         !
         IF( .NOT. llok ) THEN
            IF(lwp) WRITE(numout,*) ' Setting up assimilation background file '// TRIM( c_asmbkg )
            !
            !                                      ! Define the output file        
            CALL iom_open( c_asmbkg, inum, ldwrt = .TRUE., kiolib = jprstlib)
            !
            IF( nitbkg_r == nit000 - 1 ) THEN      ! Treat special case when nitbkg = 0
               zdate = REAL( ndastp )
#if defined key_zdftke
               ! lk_zdftke=T :   Read turbulent kinetic energy ( en )
               IF(lwp) WRITE(numout,*) ' Reading TKE (en) from restart...'
               CALL tke_rst( nit000, 'READ' )               ! lk_zdftke=T :   Read turbulent kinetic energy ( en )

#endif
            ELSE
               zdate = REAL( ndastp )
            ENDIF
            !
            !                                      ! Write the information
            CALL iom_rstput( kt, nitbkg_r, inum, 'rdastp' , zdate             )
            CALL iom_rstput( kt, nitbkg_r, inum, 'un'     , un                )
            CALL iom_rstput( kt, nitbkg_r, inum, 'vn'     , vn                )
            CALL iom_rstput( kt, nitbkg_r, inum, 'tn'     , tsn(:,:,:,jp_tem) )
            CALL iom_rstput( kt, nitbkg_r, inum, 'sn'     , tsn(:,:,:,jp_sal) )
            CALL iom_rstput( kt, nitbkg_r, inum, 'sshn'   , sshn              )
#if defined key_zdftke
            CALL iom_rstput( kt, nitbkg_r, inum, 'en'     , en                )
#endif
            CALL iom_rstput( kt, nitbkg_r, inum, 'gcx'    , gcx               )
            !
            CALL iom_close( inum )
         ENDIF
         !
      ENDIF

      !                                !-------------------------------------------
      IF( kt == nitdin_r ) THEN        ! Write out background at time step nitdin_r
         !                             !-----------------------------------========
         !
         WRITE(cl_asmdin, FMT='(A,".nc")' ) TRIM( c_asmdin )
         cl_asmdin = TRIM( cl_asmdin )
         INQUIRE( FILE = cl_asmdin, EXIST = llok )
         !
         IF( .NOT. llok ) THEN
            IF(lwp) WRITE(numout,*) ' Setting up assimilation background file '// TRIM( c_asmdin )
            !
            !                                      ! Define the output file        
            CALL iom_open( c_asmdin, inum, ldwrt = .TRUE., kiolib = jprstlib)
            !
            IF( nitdin_r == nit000 - 1 ) THEN      ! Treat special case when nitbkg = 0

               zdate = REAL( ndastp )
            ELSE
               zdate = REAL( ndastp )
            ENDIF
            !
            !                                      ! Write the information
            CALL iom_rstput( kt, nitdin_r, inum, 'rdastp' , zdate             )
            CALL iom_rstput( kt, nitdin_r, inum, 'un'     , un                )
            CALL iom_rstput( kt, nitdin_r, inum, 'vn'     , vn                )
            CALL iom_rstput( kt, nitdin_r, inum, 'tn'     , tsn(:,:,:,jp_tem) )
            CALL iom_rstput( kt, nitdin_r, inum, 'sn'     , tsn(:,:,:,jp_sal) )
            CALL iom_rstput( kt, nitdin_r, inum, 'sshn'   , sshn              )
            !
            CALL iom_close( inum )
         ENDIF
         !
      ENDIF
      !                    
   END SUBROUTINE asm_bkg_wri


   SUBROUTINE asm_trj_wri( kt )
      !!-----------------------------------------------------------------------
      !!                  ***  ROUTINE asm_trj_wri ***
      !!
      !! ** Purpose :   Write to file the model state trajectory for use with 4D-Var.
      !!-----------------------------------------------------------------------
      INTEGER, INTENT( IN ) :: kt             ! Current time-step
      !
      INTEGER :: inum                  ! File unit number
      INTEGER :: it
      CHARACTER (LEN=50) :: cl_asmtrj
      REAL(wp) :: zdate            ! Date
      !!-----------------------------------------------------------------------

      !------------------------------------------------------------------------
      ! Write a single file for each trajectory time step
      !------------------------------------------------------------------------
      IF( ( MOD( kt - nit000 + 1, nittrjfrq ) == 0 ) .OR. ( kt == nitend ) ) THEN
         
         IF( kt == nit000 - 1 ) THEN         ! Treat special case when kt = nit000-1
            !
#if defined key_zdftke
            IF(lwp) WRITE(numout,*) ' Computing  zdf_tke coeff. form restart...'
            ! Compute the vertical eddy viscosity and diffusivity coefficients
            CALL zdf_tke( nit000 )
#endif
#if defined key_zdfddm
            IF(lwp) WRITE(numout,*) ' Computing zdf_ddm coeff. from restart...'
            ! Compute the vertical eddy viscosity and diffusivity coefficients (salt effect)
            CALL zdf_ddm( nit000 )
#endif
            IF(lwp) WRITE(numout,*) ' Computing zdf_mxl coeff. from restart...'
            ! Compute the turbocline depth and the mixed layer depth
            CALL zdf_mxl( nit000 ) 
#if defined key_ldfslp
            IF(lwp) WRITE(numout,*) ' Compute the slopes of neutral surface...'
            CALL bn2( tsb, rn2 ) 
            CALL ldf_slp( nit000, rhd, rn2 )
#endif
#if defined key_traldf_c2d
            IF(lwp) WRITE(numout,*) ' Computing ldf_eiv coeff. from restart...'
            ! Compute eddy induced velocity coefficient
            IF( lk_traldf_eiv )   CALL ldf_eiv( nit000 )
#endif
         ENDIF
         !
         it = kt - nit000 + 1
         !
         !                                   ! Define the output file        
         WRITE(cl_asmtrj, FMT='(A,A,I5.5)' ) TRIM( c_asmtrj ), '_', it
         cl_asmtrj = TRIM( cl_asmtrj )
         CALL iom_open( cl_asmtrj, inum, ldwrt = .TRUE., kiolib = jprstlib)
         !
         !                                   ! Output trajectory fields
         CALL iom_rstput( it, it, inum, 'emp'   , emp    )
         CALL iom_rstput( it, it, inum, 'emps'  , emps   )
         CALL iom_rstput( it, it, inum, 'un'    , un     )
         CALL iom_rstput( it, it, inum, 'vn'    , vn     )
         CALL iom_rstput( it, it, inum, 'tn'    , tsn(:,:,:,jp_tem) )
         CALL iom_rstput( it, it, inum, 'sn'    , tsn(:,:,:,jp_sal) )
         CALL iom_rstput( it, it, inum, 'avmu'  , avmu   )
         CALL iom_rstput( it, it, inum, 'avmv'  , avmv   )
         CALL iom_rstput( it, it, inum, 'avt'   , avt    )
#if defined key_ldfslp
         CALL iom_rstput( it, it, inum, 'uslp'  , uslp   )
         CALL iom_rstput( it, it, inum, 'vslp'  , vslp   )
         CALL iom_rstput( it, it, inum, 'wslpi' , wslpi  )
         CALL iom_rstput( it, it, inum, 'wslpj' , wslpj  )
#endif
#if defined key_zdfddm
         CALL iom_rstput( it, it, inum, 'avs'   , avs    )
#endif
         CALL iom_rstput( it, it, inum, 'ta'    , tsa(:,:,:,jp_tem) )
         CALL iom_rstput( it, it, inum, 'sa'    , tsa(:,:,:,jp_sal) )
         CALL iom_rstput( it, it, inum, 'tb'    , tsb(:,:,:,jp_tem) )
         CALL iom_rstput( it, it, inum, 'sb'    , tsb(:,:,:,jp_sal) )
         IF( ln_tradmp ) THEN
            CALL iom_rstput( it, it, inum, 'strdmp', strdmp )
            CALL iom_rstput( it, it, inum, 'hmlp'  , hmlp   )
         END IF
         CALL iom_rstput( it, it, inum, 'aeiu'  , aeiu   )
         CALL iom_rstput( it, it, inum, 'aeiv'  , aeiv   )
         CALL iom_rstput( it, it, inum, 'aeiw'  , aeiw   )
         !
         CALL iom_close( inum )
      ENDIF
      !
   END SUBROUTINE asm_trj_wri

   !!======================================================================
END MODULE asmtrj
