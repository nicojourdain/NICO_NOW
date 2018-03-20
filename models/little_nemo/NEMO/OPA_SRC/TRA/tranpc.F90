MODULE tranpc
   !!==============================================================================
   !!                       ***  MODULE  tranpc  ***
   !! Ocean active tracers:  non penetrative convection scheme
   !!==============================================================================
   !! History :  1.0  ! 1990-09  (G. Madec)  Original code
   !!                 ! 1996-01  (G. Madec)  statement function for e3
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  free form F90
   !!            3.0  ! 2008-06  (G. Madec)  applied on ta, sa and called before tranxt in step.F90
   !!            3.3  ! 2010-05  (C. Ethe, G. Madec)  merge TRC-TRA
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_npc : apply the non penetrative convection scheme
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and active tracers 
   USE dom_oce         ! ocean space and time domain
   USE zdf_oce         ! ocean vertical physics
   USE trdmod_oce      ! ocean active tracer trends
   USE trdtra          ! ocean active tracer trends
   USE eosbn2          ! equation of state (eos routine) 
   USE lbclnk          ! lateral boundary conditions (or mpp link)
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_npc       ! routine called by step.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: tranpc.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_npc( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tranpc  ***
      !!
      !! ** Purpose :   Non penetrative convective adjustment scheme. solve 
      !!      the static instability of the water column on after fields
      !!      while conserving heat and salt contents.
      !!
      !! ** Method  :   The algorithm used converges in a maximium of jpk 
      !!      iterations. instabilities are treated when the vertical density
      !!      gradient is less than 1.e-5.
      !!      l_trdtra=T: the trend associated with this algorithm is saved.
      !!
      !! ** Action  : - (ta,sa) after the application od the npc scheme
      !!              - save the associated trends (ttrd,strd) ('key_trdtra')
      !!
      !! References : Madec, et al., 1991, JPO, 21, 9, 1349-1371.
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   inpcc        ! number of statically instable water column
      INTEGER  ::   inpci        ! number of iteration for npc scheme
      INTEGER  ::   jiter, jkdown, jkp        ! ???
      INTEGER  ::   ikbot, ik, ikup, ikdown   ! ???
      REAL(wp) ::   ze3tot, zta, zsa, zraua, ze3dwn
      REAL(wp), POINTER, DIMENSION(:,:  ) :: zwx, zwy, zwz
      REAL(wp), POINTER, DIMENSION(:,:,:) :: ztrdt, ztrds, zrhop
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('tra_npc')
      !
      CALL wrk_alloc(jpi, jpj, jpk, zrhop )
      CALL wrk_alloc(jpi, jpk, zwx, zwy, zwz )
      !
      IF( MOD( kt, nn_npc ) == 0 ) THEN

         inpcc = 0
         inpci = 0

         CALL eos( tsa, rhd, zrhop )         ! Potential density

         IF( l_trdtra )   THEN                    !* Save ta and sa trends
            CALL wrk_alloc( jpi, jpj, jpk, ztrdt, ztrds )
            ztrdt(:,:,:) = tsa(:,:,:,jp_tem) 
            ztrds(:,:,:) = tsa(:,:,:,jp_sal)
         ENDIF

         !                                                ! ===============
         DO jj = 1, jpj                                   !  Vertical slab
            !                                             ! ===============
            !  Static instability pointer 
            ! ----------------------------
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  zwx(ji,jk) = ( zrhop(ji,jj,jk) - zrhop(ji,jj,jk+1) ) * tmask(ji,jj,jk+1)
               END DO
            END DO

            ! 1.1 do not consider the boundary points

            ! even if east-west cyclic b. c. do not considere ji=1 or jpi
            DO jk = 1, jpkm1
               zwx( 1 ,jk) = 0.e0
               zwx(jpi,jk) = 0.e0
            END DO
            ! even if south-symmetric b. c. used, do not considere jj=1
            IF( jj == 1 )   zwx(:,:) = 0.e0

            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  zwx(ji,jk) = 1.
                  IF( zwx(ji,jk) < 1.e-5 ) zwx(ji,jk) = 0.e0
               END DO
            END DO

            zwy(:,1) = 0.e0
            DO ji = 1, jpi
               DO jk = 1, jpkm1
                  zwy(ji,1) = zwy(ji,1) + zwx(ji,jk)
               END DO
            END DO

            zwz(1,1) = 0.e0
            DO ji = 1, jpi
               zwz(1,1) = zwz(1,1) + zwy(ji,1)
            END DO

            inpcc = inpcc + NINT( zwz(1,1) )


            ! 2. Vertical mixing for each instable portion of the density profil
            ! ------------------------------------------------------------------

            IF( zwz(1,1) /= 0.e0 ) THEN         ! -->> the density profil is statically instable :
               DO ji = 1, jpi
                  IF( zwy(ji,1) /= 0.e0 ) THEN
                     !
                     ikbot = mbkt(ji,jj)        ! ikbot: ocean bottom T-level
                     !
                     DO jiter = 1, jpk          ! vertical iteration
                        !
                        ! search of ikup : the first static instability from the sea surface
                        !
                        ik = 0
220                     CONTINUE
                        ik = ik + 1
                        IF( ik >= ikbot ) GO TO 200
                        zwx(ji,ik) = zrhop(ji,jj,ik) - zrhop(ji,jj,ik+1)
                        IF( zwx(ji,ik) <= 0.e0 ) GO TO 220
                        ikup = ik
                        ! the density profil is instable below ikup
                        ! ikdown : bottom of the instable portion of the density profil
                        ! search of ikdown and vertical mixing from ikup to ikdown
                        !
                        ze3tot= fse3t(ji,jj,ikup)
                        zta   = tsa  (ji,jj,ikup,jp_tem)
                        zsa   = tsa  (ji,jj,ikup,jp_sal)
                        zraua = zrhop(ji,jj,ikup)
                        !
                        DO jkdown = ikup+1, ikbot-1
                           IF( zraua <= zrhop(ji,jj,jkdown) ) THEN
                              ikdown = jkdown
                              GO TO 240
                           ENDIF
                           ze3dwn =  fse3t(ji,jj,jkdown)
                           ze3tot =  ze3tot + ze3dwn
                           zta   = ( zta*(ze3tot-ze3dwn) + tsa(ji,jj,jkdown,jp_tem)*ze3dwn )/ze3tot
                           zsa   = ( zsa*(ze3tot-ze3dwn) + tsa(ji,jj,jkdown,jp_sal)*ze3dwn )/ze3tot
                           zraua = ( zraua*(ze3tot-ze3dwn) + zrhop(ji,jj,jkdown)*ze3dwn )/ze3tot
                           inpci = inpci+1
                        END DO
                        ikdown = ikbot-1
240                     CONTINUE
                        !
                        DO jkp = ikup, ikdown-1
                           tsa  (ji,jj,jkp,jp_tem) = zta
                           tsa  (ji,jj,jkp,jp_sal) = zsa
                           zrhop(ji,jj,jkp       ) = zraua
                        END DO
                        IF (ikdown == ikbot-1 .AND. zraua >= zrhop(ji,jj,ikdown) ) THEN
                           tsa  (ji,jj,jkp,jp_tem) = zta
                           tsa  (ji,jj,jkp,jp_sal) = zsa
                           zrhop(ji,jj,ikdown    ) = zraua
                        ENDIF
                     END DO
                  ENDIF
200               CONTINUE
               END DO
               ! <<-- no more static instability on slab jj
            ENDIF
            !                                             ! ===============
         END DO                                           !   End of slab
         !                                                ! ===============
         ! 
         IF( l_trdtra )   THEN         ! save the Non penetrative mixing trends for diagnostic
            ztrdt(:,:,:) = tsa(:,:,:,jp_tem) - ztrdt(:,:,:)
            ztrds(:,:,:) = tsa(:,:,:,jp_sal) - ztrds(:,:,:)
            CALL trd_tra( kt, 'TRA', jp_tem, jptra_trd_npc, ztrdt )
            CALL trd_tra( kt, 'TRA', jp_sal, jptra_trd_npc, ztrds )
            CALL wrk_dealloc( jpi, jpj, jpk, ztrdt, ztrds )
         ENDIF
      
         ! Lateral boundary conditions on ( ta, sa )   ( Unchanged sign)
         ! ------------------------------============
         CALL lbc_lnk( tsa(:,:,:,jp_tem), 'T', 1. )   ;   CALL lbc_lnk( tsa(:,:,:,jp_sal), 'T', 1. )
      

         !  2. non penetrative convective scheme statistics
         !  -----------------------------------------------
         IF( nn_npcp /= 0 .AND. MOD( kt, nn_npcp ) == 0 ) THEN
            IF(lwp) WRITE(numout,*)' kt=',kt, ' number of statically instable',   &
               &                   ' water column : ',inpcc, ' number of iteration : ',inpci
         ENDIF
         !
      ENDIF
      !
      CALL wrk_dealloc(jpi, jpj, jpk, zrhop )
      CALL wrk_dealloc(jpi, jpk, zwx, zwy, zwz )
      !
      IF( nn_timing == 1 )  CALL timing_stop('tra_npc')
      !
   END SUBROUTINE tra_npc

   !!======================================================================
END MODULE tranpc
