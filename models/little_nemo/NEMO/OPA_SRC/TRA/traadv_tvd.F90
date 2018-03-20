MODULE traadv_tvd
   !!==============================================================================
   !!                       ***  MODULE  traadv_tvd  ***
   !! Ocean  tracers:  horizontal & vertical advective trend
   !!==============================================================================
   !! History :  OPA  !  1995-12  (L. Mortier)  Original code
   !!                 !  2000-01  (H. Loukos)  adapted to ORCA 
   !!                 !  2000-10  (MA Foujols E.Kestenare)  include file not routine
   !!                 !  2000-12  (E. Kestenare M. Levy)  fix bug in trtrd indexes
   !!                 !  2001-07  (E. Durand G. Madec)  adaptation to ORCA config
   !!            8.5  !  2002-06  (G. Madec)  F90: Free form and module
   !!    NEMO    1.0  !  2004-01  (A. de Miranda, G. Madec, J.M. Molines ): advective bbl
   !!            2.0  !  2008-04  (S. Cravatte) add the i-, j- & k- trends computation
   !!             -   !  2009-11  (V. Garnier) Surface pressure gradient organization
   !!            3.3  !  2010-05  (C. Ethe, G. Madec)  merge TRC-TRA + switch from velocity to transport
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_adv_tvd  : update the tracer trend with the horizontal
   !!                  and vertical advection trends using a TVD scheme
   !!   nonosc       : compute monotonic tracer fluxes by a nonoscillatory
   !!                  algorithm 
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and active tracers
   USE dom_oce         ! ocean space and time domain
   USE trdmod_oce      ! tracers trends
   USE trdtra          ! tracers trends
   USE in_out_manager  ! I/O manager
   USE dynspg_oce      ! choice/control of key cpp for surface pressure gradient
   USE lib_mpp         ! MPP library
   USE lbclnk          ! ocean lateral boundary condition (or mpp link) 
   USE diaptr          ! poleward transport diagnostics
   USE trc_oce         ! share passive tracers/Ocean variables
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_adv_tvd    ! routine called by step.F90

   LOGICAL ::   l_trd   ! flag to compute trends

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: traadv_tvd.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_adv_tvd ( kt, kit000, cdtype, p2dt, pun, pvn, pwn,      &
      &                                       ptb, ptn, pta, kjpt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_tvd  ***
      !! 
      !! **  Purpose :   Compute the now trend due to total advection of 
      !!       tracers and add it to the general trend of tracer equations
      !!
      !! **  Method  :   TVD scheme, i.e. 2nd order centered scheme with
      !!       corrected flux (monotonic correction)
      !!       note: - this advection scheme needs a leap-frog time scheme
      !!
      !! ** Action : - update (pta) with the now advective tracer trends
      !!             - save the trends 
      !!----------------------------------------------------------------------
      USE oce     , ONLY:   zwx => ua        , zwy => va          ! (ua,va) used as workspace
      !
      INTEGER                              , INTENT(in   ) ::   kt              ! ocean time-step index
      INTEGER                              , INTENT(in   ) ::   kit000          ! first time step index
      CHARACTER(len=3)                     , INTENT(in   ) ::   cdtype          ! =TRA or TRC (tracer indicator)
      INTEGER                              , INTENT(in   ) ::   kjpt            ! number of tracers
      REAL(wp), DIMENSION(        jpk     ), INTENT(in   ) ::   p2dt            ! vertical profile of tracer time-step
      REAL(wp), DIMENSION(jpi,jpj,jpk     ), INTENT(in   ) ::   pun, pvn, pwn   ! 3 ocean velocity components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(in   ) ::   ptb, ptn        ! before and now tracer fields
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(inout) ::   pta             ! tracer trend 
      !
      INTEGER  ::   ji, jj, jk, jn           ! dummy loop indices  
      REAL(wp) ::   z2dtt, zbtr, ztra        ! local scalar
      REAL(wp) ::   zfp_ui, zfp_vj, zfp_wk   !   -      -
      REAL(wp) ::   zfm_ui, zfm_vj, zfm_wk   !   -      -
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zwi, zwz
      REAL(wp), POINTER, DIMENSION(:,:,:) :: ztrdx, ztrdy, ztrdz
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('tra_adv_tvd')
      !
      CALL wrk_alloc( jpi, jpj, jpk, zwi, zwz )
      !
      IF( kt == kit000 )  THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tra_adv_tvd : TVD advection scheme on ', cdtype
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
         !
         l_trd = .FALSE.
         IF( ( cdtype == 'TRA' .AND. l_trdtra ) .OR. ( cdtype == 'TRC' .AND. l_trdtrc ) ) l_trd = .TRUE.
      ENDIF
      !
      IF( l_trd )  THEN
         CALL wrk_alloc( jpi, jpj, jpk, ztrdx, ztrdy, ztrdz )
         ztrdx(:,:,:) = 0.e0   ;    ztrdy(:,:,:) = 0.e0   ;   ztrdz(:,:,:) = 0.e0
      ENDIF
      !
      zwi(:,:,:) = 0.e0
      !
      !                                                          ! ===========
      DO jn = 1, kjpt                                            ! tracer loop
         !                                                       ! ===========
         ! 1. Bottom value : flux set to zero
         ! ----------------------------------
         zwx(:,:,jpk) = 0.e0    ;    zwz(:,:,jpk) = 0.e0
         zwy(:,:,jpk) = 0.e0    ;    zwi(:,:,jpk) = 0.e0

         ! 2. upstream advection with initial mass fluxes & intermediate update
         ! --------------------------------------------------------------------
         ! upstream tracer flux in the i and j direction
         DO jk = 1, jpkm1
            DO jj = 1, jpjm1
               DO ji = 1, fs_jpim1   ! vector opt.
                  ! upstream scheme
                  zfp_ui = pun(ji,jj,jk) + ABS( pun(ji,jj,jk) )
                  zfm_ui = pun(ji,jj,jk) - ABS( pun(ji,jj,jk) )
                  zfp_vj = pvn(ji,jj,jk) + ABS( pvn(ji,jj,jk) )
                  zfm_vj = pvn(ji,jj,jk) - ABS( pvn(ji,jj,jk) )
                  zwx(ji,jj,jk) = 0.5 * ( zfp_ui * ptb(ji,jj,jk,jn) + zfm_ui * ptb(ji+1,jj  ,jk,jn) )
                  zwy(ji,jj,jk) = 0.5 * ( zfp_vj * ptb(ji,jj,jk,jn) + zfm_vj * ptb(ji  ,jj+1,jk,jn) )
               END DO
            END DO
         END DO

         ! upstream tracer flux in the k direction
         ! Surface value
         IF( lk_vvl ) THEN   ;   zwz(:,:, 1 ) = 0.e0                         ! volume variable
         ELSE                ;   zwz(:,:, 1 ) = pwn(:,:,1) * ptb(:,:,1,jn)   ! linear free surface 
         ENDIF
         ! Interior value
         DO jk = 2, jpkm1
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zfp_wk = pwn(ji,jj,jk) + ABS( pwn(ji,jj,jk) )
                  zfm_wk = pwn(ji,jj,jk) - ABS( pwn(ji,jj,jk) )
                  zwz(ji,jj,jk) = 0.5 * ( zfp_wk * ptb(ji,jj,jk,jn) + zfm_wk * ptb(ji,jj,jk-1,jn) )
               END DO
            END DO
         END DO

         ! total advective trend
         DO jk = 1, jpkm1
            z2dtt = p2dt(jk)
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zbtr = 1. / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
                  ! total intermediate advective trends
                  ztra = - zbtr * (  zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )   &
                     &             + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  )   &
                     &             + zwz(ji,jj,jk) - zwz(ji  ,jj  ,jk+1) )
                  ! update and guess with monotonic sheme
                  pta(ji,jj,jk,jn) =   pta(ji,jj,jk,jn)         + ztra
                  zwi(ji,jj,jk)    = ( ptb(ji,jj,jk,jn) + z2dtt * ztra ) * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !                             ! Lateral boundary conditions on zwi  (unchanged sign)
         CALL lbc_lnk( zwi, 'T', 1. )  

         !                                 ! trend diagnostics (contribution of upstream fluxes)
         IF( l_trd )  THEN 
            ! store intermediate advective trends
            ztrdx(:,:,:) = zwx(:,:,:)   ;    ztrdy(:,:,:) = zwy(:,:,:)  ;   ztrdz(:,:,:) = zwz(:,:,:)
         END IF
         !                                 ! "Poleward" heat and salt transports (contribution of upstream fluxes)
         IF( cdtype == 'TRA' .AND. ln_diaptr .AND. ( MOD( kt, nn_fptr ) == 0 ) ) THEN  
           IF( jn == jp_tem )  htr_adv(:) = ptr_vj( zwy(:,:,:) )
           IF( jn == jp_sal )  str_adv(:) = ptr_vj( zwy(:,:,:) )
         ENDIF

         ! 3. antidiffusive flux : high order minus low order
         ! --------------------------------------------------
         ! antidiffusive flux on i and j
         DO jk = 1, jpkm1
            DO jj = 1, jpjm1
               DO ji = 1, fs_jpim1   ! vector opt.
                  zwx(ji,jj,jk) = 0.5 * pun(ji,jj,jk) * ( ptn(ji,jj,jk,jn) + ptn(ji+1,jj,jk,jn) ) - zwx(ji,jj,jk)
                  zwy(ji,jj,jk) = 0.5 * pvn(ji,jj,jk) * ( ptn(ji,jj,jk,jn) + ptn(ji,jj+1,jk,jn) ) - zwy(ji,jj,jk)
               END DO
            END DO
         END DO
      
         ! antidiffusive flux on k
         zwz(:,:,1) = 0.e0         ! Surface value
         !
         DO jk = 2, jpkm1          ! Interior value
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zwz(ji,jj,jk) = 0.5 * pwn(ji,jj,jk) * ( ptn(ji,jj,jk,jn) + ptn(ji,jj,jk-1,jn) ) - zwz(ji,jj,jk)
               END DO
            END DO
         END DO
         CALL lbc_lnk( zwx, 'U', -1. )   ;   CALL lbc_lnk( zwy, 'V', -1. )         ! Lateral bondary conditions
         CALL lbc_lnk( zwz, 'W',  1. )

         ! 4. monotonicity algorithm
         ! -------------------------
         CALL nonosc( ptb(:,:,:,jn), zwx, zwy, zwz, zwi, p2dt )


         ! 5. final trend with corrected fluxes
         ! ------------------------------------
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.  
                  zbtr = 1. / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
                  ! total advective trends
                  ztra = - zbtr * (  zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )   &
                     &             + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  )   &
                     &             + zwz(ji,jj,jk) - zwz(ji  ,jj  ,jk+1) )
                  ! add them to the general tracer trends
                  pta(ji,jj,jk,jn) = pta(ji,jj,jk,jn) + ztra
               END DO
            END DO
         END DO

         !                                 ! trend diagnostics (contribution of upstream fluxes)
         IF( l_trd )  THEN 
            ztrdx(:,:,:) = ztrdx(:,:,:) + zwx(:,:,:)  ! <<< Add to previously computed
            ztrdy(:,:,:) = ztrdy(:,:,:) + zwy(:,:,:)  ! <<< Add to previously computed
            ztrdz(:,:,:) = ztrdz(:,:,:) + zwz(:,:,:)  ! <<< Add to previously computed
            
            CALL trd_tra( kt, cdtype, jn, jptra_trd_xad, ztrdx, pun, ptn(:,:,:,jn) )   
            CALL trd_tra( kt, cdtype, jn, jptra_trd_yad, ztrdy, pvn, ptn(:,:,:,jn) )  
            CALL trd_tra( kt, cdtype, jn, jptra_trd_zad, ztrdz, pwn, ptn(:,:,:,jn) ) 
         END IF
         !                                 ! "Poleward" heat and salt transports (contribution of upstream fluxes)
         IF( cdtype == 'TRA' .AND. ln_diaptr .AND. ( MOD( kt, nn_fptr ) == 0 ) ) THEN  
           IF( jn == jp_tem )  htr_adv(:) = ptr_vj( zwy(:,:,:) ) + htr_adv(:)
           IF( jn == jp_sal )  str_adv(:) = ptr_vj( zwy(:,:,:) ) + str_adv(:)
         ENDIF
         !
      END DO
      !
                   CALL wrk_dealloc( jpi, jpj, jpk, zwi, zwz )
      IF( l_trd )  CALL wrk_dealloc( jpi, jpj, jpk, ztrdx, ztrdy, ztrdz )
      !
      IF( nn_timing == 1 )  CALL timing_stop('tra_adv_tvd')
      !
   END SUBROUTINE tra_adv_tvd


   SUBROUTINE nonosc( pbef, paa, pbb, pcc, paft, p2dt )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE nonosc  ***
      !!     
      !! **  Purpose :   compute monotonic tracer fluxes from the upstream 
      !!       scheme and the before field by a nonoscillatory algorithm 
      !!
      !! **  Method  :   ... ???
      !!       warning : pbef and paft must be masked, but the boundaries
      !!       conditions on the fluxes are not necessary zalezak (1979)
      !!       drange (1995) multi-dimensional forward-in-time and upstream-
      !!       in-space based differencing for fluid
      !!----------------------------------------------------------------------
      !
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpk)         , INTENT(in   ) ::   p2dt            ! vertical profile of tracer time-step
      REAL(wp), DIMENSION (jpi,jpj,jpk), INTENT(in   ) ::   pbef, paft      ! before & after field
      REAL(wp), DIMENSION (jpi,jpj,jpk), INTENT(inout) ::   paa, pbb, pcc   ! monotonic fluxes in the 3 directions
      !
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      INTEGER ::   ikm1         ! local integer
      REAL(wp) ::   zpos, zneg, zbt, za, zb, zc, zbig, zrtrn, z2dtt   ! local scalars
      REAL(wp) ::   zau, zbu, zcu, zav, zbv, zcv, zup, zdo            !   -      -
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zbetup, zbetdo, zbup, zbdo
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('nonosc')
      !
      CALL wrk_alloc( jpi, jpj, jpk, zbetup, zbetdo, zbup, zbdo )
      !

      zbig  = 1.e+40_wp
      zrtrn = 1.e-15_wp
      zbetup(:,:,jpk) = 0._wp   ;   zbetdo(:,:,jpk) = 0._wp


      ! Search local extrema
      ! --------------------
      ! max/min of pbef & paft with large negative/positive value (-/+zbig) inside land
      zbup = MAX( pbef * tmask - zbig * ( 1.e0 - tmask ),   &
         &        paft * tmask - zbig * ( 1.e0 - tmask )  )
      zbdo = MIN( pbef * tmask + zbig * ( 1.e0 - tmask ),   &
         &        paft * tmask + zbig * ( 1.e0 - tmask )  )

      DO jk = 1, jpkm1
         ikm1 = MAX(jk-1,1)
         z2dtt = p2dt(jk)
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.

               ! search maximum in neighbourhood
               zup = MAX(  zbup(ji  ,jj  ,jk  ),   &
                  &        zbup(ji-1,jj  ,jk  ), zbup(ji+1,jj  ,jk  ),   &
                  &        zbup(ji  ,jj-1,jk  ), zbup(ji  ,jj+1,jk  ),   &
                  &        zbup(ji  ,jj  ,ikm1), zbup(ji  ,jj  ,jk+1)  )

               ! search minimum in neighbourhood
               zdo = MIN(  zbdo(ji  ,jj  ,jk  ),   &
                  &        zbdo(ji-1,jj  ,jk  ), zbdo(ji+1,jj  ,jk  ),   &
                  &        zbdo(ji  ,jj-1,jk  ), zbdo(ji  ,jj+1,jk  ),   &
                  &        zbdo(ji  ,jj  ,ikm1), zbdo(ji  ,jj  ,jk+1)  )

               ! positive part of the flux
               zpos = MAX( 0., paa(ji-1,jj  ,jk  ) ) - MIN( 0., paa(ji  ,jj  ,jk  ) )   &
                  & + MAX( 0., pbb(ji  ,jj-1,jk  ) ) - MIN( 0., pbb(ji  ,jj  ,jk  ) )   &
                  & + MAX( 0., pcc(ji  ,jj  ,jk+1) ) - MIN( 0., pcc(ji  ,jj  ,jk  ) )

               ! negative part of the flux
               zneg = MAX( 0., paa(ji  ,jj  ,jk  ) ) - MIN( 0., paa(ji-1,jj  ,jk  ) )   &
                  & + MAX( 0., pbb(ji  ,jj  ,jk  ) ) - MIN( 0., pbb(ji  ,jj-1,jk  ) )   &
                  & + MAX( 0., pcc(ji  ,jj  ,jk  ) ) - MIN( 0., pcc(ji  ,jj  ,jk+1) )

               ! up & down beta terms
               zbt = e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) / z2dtt
               zbetup(ji,jj,jk) = ( zup            - paft(ji,jj,jk) ) / ( zpos + zrtrn ) * zbt
               zbetdo(ji,jj,jk) = ( paft(ji,jj,jk) - zdo            ) / ( zneg + zrtrn ) * zbt
            END DO
         END DO
      END DO
      CALL lbc_lnk( zbetup, 'T', 1. )   ;   CALL lbc_lnk( zbetdo, 'T', 1. )   ! lateral boundary cond. (unchanged sign)

      ! 3. monotonic flux in the i & j direction (paa & pbb)
      ! ----------------------------------------
      DO jk = 1, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zau = MIN( 1.e0, zbetdo(ji,jj,jk), zbetup(ji+1,jj,jk) )
               zbu = MIN( 1.e0, zbetup(ji,jj,jk), zbetdo(ji+1,jj,jk) )
               zcu =       ( 0.5  + SIGN( 0.5 , paa(ji,jj,jk) ) )
               paa(ji,jj,jk) = paa(ji,jj,jk) * ( zcu * zau + ( 1.e0 - zcu) * zbu )

               zav = MIN( 1.e0, zbetdo(ji,jj,jk), zbetup(ji,jj+1,jk) )
               zbv = MIN( 1.e0, zbetup(ji,jj,jk), zbetdo(ji,jj+1,jk) )
               zcv =       ( 0.5  + SIGN( 0.5 , pbb(ji,jj,jk) ) )
               pbb(ji,jj,jk) = pbb(ji,jj,jk) * ( zcv * zav + ( 1.e0 - zcv) * zbv )

      ! monotonic flux in the k direction, i.e. pcc
      ! -------------------------------------------
               za = MIN( 1., zbetdo(ji,jj,jk+1), zbetup(ji,jj,jk) )
               zb = MIN( 1., zbetup(ji,jj,jk+1), zbetdo(ji,jj,jk) )
               zc =       ( 0.5  + SIGN( 0.5 , pcc(ji,jj,jk+1) ) )
               pcc(ji,jj,jk+1) = pcc(ji,jj,jk+1) * ( zc * za + ( 1.e0 - zc) * zb )
            END DO
         END DO
      END DO
      CALL lbc_lnk( paa, 'U', -1. )   ;   CALL lbc_lnk( pbb, 'V', -1. )   ! lateral boundary condition (changed sign)
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zbetup, zbetdo, zbup, zbdo )
      !
      IF( nn_timing == 1 )  CALL timing_stop('nonosc')
      !
   END SUBROUTINE nonosc

   !!======================================================================
END MODULE traadv_tvd
