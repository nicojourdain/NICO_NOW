MODULE traadv_eiv
   !!======================================================================
   !!                    ***  MODULE  traadv_eiv  ***
   !! Ocean tracers:  advection trend - eddy induced velocity
   !!======================================================================
   !! History :  1.0  !  2005-11 (G. Madec)  Original code, from traldf and zdf _iso
   !!            3.3  !  2010-05 (C. Ethe, G. Madec)  merge TRC-TRA 
   !!----------------------------------------------------------------------
#if defined key_traldf_eiv   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_traldf_eiv'                  rotation of the lateral mixing tensor
   !!----------------------------------------------------------------------
   !!   tra_ldf_iso : update the tracer trend with the horizontal component
   !!                 of iso neutral laplacian operator or horizontal 
   !!                 laplacian operator in s-coordinate
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE ldftra_oce      ! ocean active tracers: lateral physics
   USE ldfslp          ! iso-neutral slopes
   USE in_out_manager  ! I/O manager
   USE iom
   USE trc_oce         ! share passive tracers/Ocean variables
# if defined key_diaeiv
   USE phycst          ! physical constants
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE diaar5, ONLY:   lk_diaar5
# endif  
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_adv_eiv   ! routine called by step.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "ldftra_substitute.h90"
#  include "ldfeiv_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: traadv_eiv.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_adv_eiv( kt, kit000, pun, pvn, pwn, cdtype )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_eiv  ***
      !! 
      !! ** Purpose :   Compute the before horizontal tracer (t & s) diffusive 
      !!      trend and add it to the general trend of tracer equation.
      !!
      !! ** Method  :   The eddy induced advection is computed from the slope
      !!      of iso-neutral surfaces computed in routine ldf_slp as follows:
      !!         zu_eiv =  1/(e2u e3u)   dk[ aeiu e2u mi(wslpi) ]
      !!         zv_eiv =  1/(e1v e3v)   dk[ aeiv e1v mj(wslpj)
      !!         zw_eiv = -1/(e1t e2t) { di[ aeiu e2u mi(wslpi) ]
      !!                               + dj[ aeiv e1v mj(wslpj) ] }
      !!      add the eiv component to the model velocity:
      !!         p.n = p.n + z._eiv
      !!
      !! ** Action  : - add to p.n the eiv component
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   kt       ! ocean time-step index
      INTEGER                         , INTENT(in   ) ::   kit000   ! first time step index
      CHARACTER(len=3)                , INTENT(in   ) ::   cdtype   ! =TRA or TRC (tracer indicator)
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   pun      ! in : 3 ocean velocity components 
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   pvn      ! out: 3 ocean velocity components
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   pwn      ! increased by the eiv
      !!
      INTEGER  ::   ji, jj, jk                 ! dummy loop indices
      REAL(wp) ::   zuwk, zuwk1, zuwi, zuwi1   ! local scalars
      REAL(wp) ::   zvwk, zvwk1, zvwj, zvwj1   !   -      -
# if defined key_diaeiv 
      REAL(wp) ::   zztmp                      ! local scalar
# endif  
      REAL(wp), POINTER, DIMENSION(:,:) :: zu_eiv, zv_eiv, zw_eiv, z2d
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start( 'tra_adv_eiv')
      !
# if defined key_diaeiv 
      CALL wrk_alloc( jpi, jpj, zu_eiv, zv_eiv, zw_eiv, z2d )
# else
      CALL wrk_alloc( jpi, jpj, zu_eiv, zv_eiv, zw_eiv )
# endif

      IF( kt == kit000 )  THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tra_adv_eiv : eddy induced advection on ', cdtype,' :'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   add to velocity fields the eiv component'
# if defined key_diaeiv 
         IF( cdtype == 'TRA') THEN
            u_eiv(:,:,:) = 0.e0
            v_eiv(:,:,:) = 0.e0
            w_eiv(:,:,:) = 0.e0
         END IF
# endif
      ENDIF

      zu_eiv(:,:) = 0.e0   ;   zv_eiv(:,:) = 0.e0   ;    zw_eiv(:,:) = 0.e0  
      
                                                    ! =================
      DO jk = 1, jpkm1                              !  Horizontal slab
         !                                          ! =================
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               zuwk = ( wslpi(ji,jj,jk  ) + wslpi(ji+1,jj,jk  ) ) * fsaeiu(ji,jj,jk  ) * umask(ji,jj,jk  )
               zuwk1= ( wslpi(ji,jj,jk+1) + wslpi(ji+1,jj,jk+1) ) * fsaeiu(ji,jj,jk+1) * umask(ji,jj,jk+1)
               zvwk = ( wslpj(ji,jj,jk  ) + wslpj(ji,jj+1,jk  ) ) * fsaeiv(ji,jj,jk  ) * vmask(ji,jj,jk  )
               zvwk1= ( wslpj(ji,jj,jk+1) + wslpj(ji,jj+1,jk+1) ) * fsaeiv(ji,jj,jk+1) * vmask(ji,jj,jk+1)

               zu_eiv(ji,jj) = 0.5 * umask(ji,jj,jk) * ( zuwk - zuwk1 ) 
               zv_eiv(ji,jj) = 0.5 * vmask(ji,jj,jk) * ( zvwk - zvwk1 ) 
   
               pun(ji,jj,jk) = pun(ji,jj,jk) + e2u(ji,jj) * zu_eiv(ji,jj)
               pvn(ji,jj,jk) = pvn(ji,jj,jk) + e1v(ji,jj) * zv_eiv(ji,jj)
            END DO
         END DO
# if defined key_diaeiv 
         IF( cdtype == 'TRA') THEN
            u_eiv(:,:,jk) = zu_eiv(:,:) / fse3u(:,:,jk)
            v_eiv(:,:,jk) = zv_eiv(:,:) / fse3v(:,:,jk)
         END IF
# endif
         IF( jk >=2 ) THEN                             ! jk=1 zw_eiv=0, not computed
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
# if defined key_traldf_c2d || defined key_traldf_c3d
                  zuwi  = ( wslpi(ji,jj,jk)+wslpi(ji-1,jj,jk) ) * fsaeiu(ji-1,jj,jk) * e2u(ji-1,jj) * umask(ji-1,jj,jk)
                  zuwi1 = ( wslpi(ji,jj,jk)+wslpi(ji+1,jj,jk) ) * fsaeiu(ji  ,jj,jk) * e2u(ji  ,jj) * umask(ji  ,jj,jk)
                  zvwj  = ( wslpj(ji,jj,jk)+wslpj(ji,jj-1,jk) ) * fsaeiv(ji,jj-1,jk) * e1v(ji,jj-1) * vmask(ji,jj-1,jk)
                  zvwj1 = ( wslpj(ji,jj,jk)+wslpj(ji,jj+1,jk) ) * fsaeiv(ji,jj  ,jk) * e1v(ji  ,jj) * vmask(ji  ,jj,jk)
  
                  zw_eiv(ji,jj) = - 0.5 * tmask(ji,jj,jk) * ( zuwi1 - zuwi + zvwj1 - zvwj ) 
# else
                  zuwi  = ( wslpi(ji,jj,jk) + wslpi(ji-1,jj,jk) ) * e2u(ji-1,jj) * umask(ji-1,jj,jk)
                  zuwi1 = ( wslpi(ji,jj,jk) + wslpi(ji+1,jj,jk) ) * e2u(ji  ,jj) * umask(ji  ,jj,jk)
                  zvwj  = ( wslpj(ji,jj,jk) + wslpj(ji,jj-1,jk) ) * e1v(ji,jj-1) * vmask(ji,jj-1,jk)
                  zvwj1 = ( wslpj(ji,jj,jk) + wslpj(ji,jj+1,jk) ) * e1v(ji  ,jj) * vmask(ji  ,jj,jk)

                  zw_eiv(ji,jj) = - 0.5 * tmask(ji,jj,jk) * fsaeiw(ji,jj,jk) * ( zuwi1 - zuwi + zvwj1 - zvwj )
# endif
                  pwn(ji,jj,jk) = pwn(ji,jj,jk) + zw_eiv(ji,jj)
               END DO
            END DO
# if defined key_diaeiv 
            IF( cdtype == 'TRA')  w_eiv(:,:,jk) = zw_eiv(:,:) / ( e1t(:,:) * e2t(:,:) )
# endif
         ENDIF
         !                                          ! =================
      END DO                                        !    End of slab  
      !                                             ! =================

# if defined key_diaeiv 
      IF( cdtype == 'TRA') THEN
         CALL iom_put( "uoce_eiv", u_eiv )    ! i-eiv current
         CALL iom_put( "voce_eiv", v_eiv )    ! j-eiv current
         CALL iom_put( "woce_eiv", w_eiv )    ! vert. eiv current
         IF( lk_diaar5 ) THEN
            zztmp = 0.5 * rau0 * rcp 
            z2d(:,:) = 0.e0 
            DO jk = 1, jpkm1
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     z2d(ji,jj) = z2d(ji,jj) + zztmp * u_eiv(ji,jj,jk) &
                       &         * (tsn(ji,jj,jk,jp_tem)+tsn(ji+1,jj,jk,jp_tem)) * e2u(ji,jj) * fse3u(ji,jj,jk) 
                  END DO
               END DO
            END DO
            CALL lbc_lnk( z2d, 'U', -1. )
            CALL iom_put( "ueiv_heattr", z2d )                  ! heat transport in i-direction
            z2d(:,:) = 0.e0 
            DO jk = 1, jpkm1
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     z2d(ji,jj) = z2d(ji,jj) + zztmp * v_eiv(ji,jj,jk) &
                     &           * (tsn(ji,jj,jk,jp_tem)+tsn(ji,jj+1,jk,jp_tem)) * e1v(ji,jj) * fse3v(ji,jj,jk) 
                  END DO
               END DO
            END DO
            CALL lbc_lnk( z2d, 'V', -1. )
            CALL iom_put( "veiv_heattr", z2d )                  !  heat transport in i-direction
         ENDIF
    END IF
# endif  
      ! 
# if defined key_diaeiv 
      CALL wrk_dealloc( jpi, jpj, zu_eiv, zv_eiv, zw_eiv, z2d )
# else
      CALL wrk_dealloc( jpi, jpj, zu_eiv, zv_eiv, zw_eiv )
# endif
      !
      IF( nn_timing == 1 )  CALL timing_stop( 'tra_adv_eiv')
      !
    END SUBROUTINE tra_adv_eiv

#else
   !!----------------------------------------------------------------------
   !!   Dummy module :             No rotation of the lateral mixing tensor
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE tra_adv_eiv( kt, kit000, pun, pvn, pwn, cdtype )              ! Empty routine
      INTEGER  ::   kt    
      INTEGER  ::   kit000    
      CHARACTER(len=3) ::   cdtype
      REAL, DIMENSION(:,:,:) ::   pun, pvn, pwn
      WRITE(*,*) 'tra_adv_eiv: You should not have seen this print! error?', kt, cdtype, pun(1,1,1), pvn(1,1,1), pwn(1,1,1)
   END SUBROUTINE tra_adv_eiv
#endif

   !!==============================================================================
END MODULE traadv_eiv
