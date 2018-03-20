MODULE zdfddm
   !!======================================================================
   !!                       ***  MODULE  zdfddm  ***
   !! Ocean physics : double diffusion mixing parameterization
   !!======================================================================
   !! History :  OPA  ! 2000-08  (G. Madec)  double diffusive mixing
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  F90: Free form and module
   !!            3.3  !  2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase
   !!----------------------------------------------------------------------
#if defined key_zdfddm   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_zdfddm' :                                     double diffusion
   !!----------------------------------------------------------------------
   !!   zdf_ddm       : compute the Ks for salinity
   !!   zdf_ddm_init  : read namelist and control the parameters
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables 
   USE zdf_oce         ! ocean vertical physics variables
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! Print control
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_ddm       ! called by step.F90
   PUBLIC   zdf_ddm_init  ! called by opa.F90
   PUBLIC   zdf_ddm_alloc ! called by nemogcm.F90

   LOGICAL , PUBLIC, PARAMETER ::   lk_zdfddm = .TRUE.  !: double diffusive mixing flag

   REAL(wp), PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:,:,:) ::   avs    !: salinity vertical diffusivity coeff. at w-point
   REAL(wp), PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:,:,:) ::   rrau   !: heat/salt buoyancy flux ratio

   !                                  !!* Namelist namzdf_ddm : double diffusive mixing *
   REAL(wp) ::   rn_avts  = 1.e-4_wp   ! maximum value of avs for salt fingering
   REAL(wp) ::   rn_hsbfr = 1.6_wp     ! heat/salt buoyancy flux ratio

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: zdfddm.F90 3348 2012-04-11 08:25:58Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_ddm_alloc()
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE zdf_ddm_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( avs(jpi,jpj,jpk), rrau(jpi,jpj,jpk), STAT= zdf_ddm_alloc )
      !
      IF( lk_mpp             )   CALL mpp_sum ( zdf_ddm_alloc )
      IF( zdf_ddm_alloc /= 0 )   CALL ctl_warn('zdf_ddm_alloc: failed to allocate arrays')
   END FUNCTION zdf_ddm_alloc


   SUBROUTINE zdf_ddm( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_ddm  ***
      !!                    
      !! ** Purpose :   Add to the vertical eddy diffusivity coefficient the 
      !!              effect of salt fingering and diffusive convection. 
      !!
      !! ** Method  :   Diapycnal mixing is increased in case of double
      !!      diffusive mixing (i.e. salt fingering and diffusive layering)
      !!      following Merryfield et al. (1999). The rate of double diffusive 
      !!      mixing depend on the buoyancy ratio: Rrau=alpha/beta dk[T]/dk[S]
      !!      which is computed in rn2.F
      !!         * salt fingering (Schmitt 1981):
      !!      for Rrau > 1 and rn2 > 0 : zavfs = rn_avts / ( 1 + (Rrau/rn_hsbfr)^6 )
      !!      for Rrau > 1 and rn2 > 0 : zavfs = O
      !!      otherwise                : zavft = 0.7 zavs / Rrau
      !!         * diffusive layering (Federov 1988):
      !!      for 0< Rrau < 1 and rn2 > 0 : zavdt = 1.3635e-6  
      !!                                 * exp( 4.6 exp(-0.54 (1/Rrau-1) ) )
      !!      otherwise                   : zavdt = 0 
      !!      for .5 < Rrau < 1 and rn2 > 0 : zavds = zavdt (1.885 Rrau -0.85)
      !!      for  0 < Rrau <.5 and rn2 > 0 : zavds = zavdt 0.15 Rrau      
      !!      otherwise                     : zavds = 0 
      !!         * update the eddy diffusivity:
      !!      avt = avt + zavft + zavdt
      !!      avs = avs + zavfs + zavds
      !!      avmu, avmv are required to remain at least above avt and avs.
      !!      
      !! ** Action  :   avt, avs : updated vertical eddy diffusivity coef. for T & S
      !!
      !! References :   Merryfield et al., JPO, 29, 1124-1142, 1999.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step indexocean time step
      !
      INTEGER  ::   ji, jj , jk     ! dummy loop indices
      REAL(wp) ::   zinr, zrr       ! temporary scalars
      REAL(wp) ::   zavft, zavfs    !    -         -
      REAL(wp) ::   zavdt, zavds    !    -         -
      REAL(wp), POINTER, DIMENSION(:,:) ::   zmsks, zmskf, zmskd1, zmskd2, zmskd3
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zdf_ddm')
      !
      CALL wrk_alloc( jpi,jpj, zmsks, zmskf, zmskd1, zmskd2, zmskd3 )

      !                                                ! ===============
      DO jk = 2, jpkm1                                 ! Horizontal slab
         !                                             ! ===============
         ! Define the mask 
         ! ---------------
         rrau(:,:,jk) = MAX( 1.e-20, rrau(:,:,jk) )         ! only retains positive value of rrau

         DO jj = 1, jpj                                     ! indicators:
            DO ji = 1, jpi
               ! stability indicator: msks=1 if rn2>0; 0 elsewhere
               IF( rn2(ji,jj,jk) + 1.e-12  <= 0. ) THEN   ;   zmsks(ji,jj) = 0._wp
               ELSE                                       ;   zmsks(ji,jj) = 1._wp
               ENDIF
               ! salt fingering indicator: msksf=1 if rrau>1; 0 elsewhere            
               IF( rrau(ji,jj,jk) <= 1.          ) THEN   ;   zmskf(ji,jj) = 0._wp
               ELSE                                       ;   zmskf(ji,jj) = 1._wp
               ENDIF
               ! diffusive layering indicators: 
               !     ! mskdl1=1 if 0<rrau<1; 0 elsewhere
               IF( rrau(ji,jj,jk) >= 1.          ) THEN   ;   zmskd1(ji,jj) = 0._wp
               ELSE                                       ;   zmskd1(ji,jj) = 1._wp
               ENDIF
               !     ! mskdl2=1 if 0<rrau<0.5; 0 elsewhere
               IF( rrau(ji,jj,jk) >= 0.5         ) THEN   ;   zmskd2(ji,jj) = 0._wp
               ELSE                                       ;   zmskd2(ji,jj) = 1._wp
               ENDIF
               !   mskdl3=1 if 0.5<rrau<1; 0 elsewhere
               IF( rrau(ji,jj,jk) <= 0.5 .OR. rrau(ji,jj,jk) >= 1. ) THEN   ;   zmskd3(ji,jj) = 0._wp
               ELSE                                                         ;   zmskd3(ji,jj) = 1._wp
               ENDIF
            END DO
         END DO
         ! mask zmsk in order to have avt and avs masked
         zmsks(:,:) = zmsks(:,:) * tmask(:,:,jk)


         ! Update avt and avs
         ! ------------------
         ! Constant eddy coefficient: reset to the background value
!CDIR NOVERRCHK
         DO jj = 1, jpj
!CDIR NOVERRCHK
            DO ji = 1, jpi
               zinr = 1./rrau(ji,jj,jk)
               ! salt fingering
               zrr = rrau(ji,jj,jk)/rn_hsbfr
               zrr = zrr * zrr
               zavfs = rn_avts / ( 1 + zrr*zrr*zrr ) * zmsks(ji,jj) * zmskf(ji,jj)
               zavft = 0.7 * zavfs * zinr
               ! diffusive layering
               zavdt = 1.3635e-6 * EXP(  4.6 * EXP( -0.54*(zinr-1.) )  ) * zmsks(ji,jj) * zmskd1(ji,jj)
               zavds = zavdt * zmsks(ji,jj) * (  (1.85 * rrau(ji,jj,jk) - 0.85 ) * zmskd3(ji,jj)   &
                  &                            +  0.15 * rrau(ji,jj,jk)          * zmskd2(ji,jj)  )
               ! add to the eddy viscosity coef. previously computed
               avs (ji,jj,jk) = avt(ji,jj,jk) + zavfs + zavds
               avt (ji,jj,jk) = avt(ji,jj,jk) + zavft + zavdt
               avm (ji,jj,jk) = avm(ji,jj,jk) + MAX( zavft + zavdt, zavfs + zavds )
            END DO
         END DO


         ! Increase avmu, avmv if necessary
         ! --------------------------------
!!gm to be changed following the definition of avm.
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               avmu(ji,jj,jk) = MAX( avmu(ji,jj,jk),    &
                  &                  avt(ji,jj,jk), avt(ji+1,jj,jk),   &
                  &                  avs(ji,jj,jk), avs(ji+1,jj,jk) )  * umask(ji,jj,jk)
               avmv(ji,jj,jk) = MAX( avmv(ji,jj,jk),    &
                  &                  avt(ji,jj,jk), avt(ji,jj+1,jk),   &
                  &                  avs(ji,jj,jk), avs(ji,jj+1,jk) )  * vmask(ji,jj,jk)
            END DO
         END DO
         !                                                ! ===============
      END DO                                              !   End of slab
      !                                                   ! ===============
      !
      CALL lbc_lnk( avt , 'W', 1._wp )     ! Lateral boundary conditions   (unchanged sign)
      CALL lbc_lnk( avs , 'W', 1._wp )
      CALL lbc_lnk( avm , 'W', 1._wp )
      CALL lbc_lnk( avmu, 'U', 1._wp ) 
      CALL lbc_lnk( avmv, 'V', 1._wp )

      IF(ln_ctl) THEN
         CALL prt_ctl(tab3d_1=avt , clinfo1=' ddm  - t: ', tab3d_2=avs , clinfo2=' s: ', ovlap=1, kdim=jpk)
         CALL prt_ctl(tab3d_1=avmu, clinfo1=' ddm  - u: ', mask1=umask, &
            &         tab3d_2=avmv, clinfo2=       ' v: ', mask2=vmask, ovlap=1, kdim=jpk)
      ENDIF
      !
      CALL wrk_dealloc( jpi,jpj, zmsks, zmskf, zmskd1, zmskd2, zmskd3 )
      !
      IF( nn_timing == 1 )  CALL timing_stop('zdf_ddm')
      !
   END SUBROUTINE zdf_ddm
   
   
   SUBROUTINE zdf_ddm_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_ddm_init  ***
      !!
      !! ** Purpose :   Initialization of double diffusion mixing scheme
      !!
      !! ** Method  :   Read the namzdf_ddm namelist and check the parameter values
      !!              called by zdf_ddm at the first timestep (nit000)
      !!----------------------------------------------------------------------
      NAMELIST/namzdf_ddm/ rn_avts, rn_hsbfr
      !!----------------------------------------------------------------------
      !
      REWIND( numnam )                ! Read Namelist namzdf_ddm : double diffusion mixing scheme
      READ  ( numnam, namzdf_ddm )
      !
      IF(lwp) THEN                    ! Parameter print
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_ddm : double diffusive mixing'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '   Namelist namzdf_ddm : set dd mixing parameter'
         WRITE(numout,*) '      maximum avs for dd mixing      rn_avts   = ', rn_avts
         WRITE(numout,*) '      heat/salt buoyancy flux ratio  rn_hsbfr  = ', rn_hsbfr
      ENDIF
      !
      !                               ! allocate zdfddm arrays
      IF( zdf_ddm_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_ddm_init : unable to allocate arrays' )
      !                               ! initialization to masked Kz
      avs(:,:,:) = rn_avt0 * tmask(:,:,:) 
      !
   END SUBROUTINE zdf_ddm_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :          Dummy module          No double diffusion
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_zdfddm = .FALSE.   !: double diffusion flag
CONTAINS
   SUBROUTINE zdf_ddm( kt )           ! Dummy routine
      WRITE(*,*) 'zdf_ddm: You should not have seen this print! error?', kt
   END SUBROUTINE zdf_ddm
   SUBROUTINE zdf_ddm_init            ! Dummy routine
   END SUBROUTINE zdf_ddm_init
#endif

   !!======================================================================
END MODULE zdfddm
