MODULE sedco3
#if defined key_sed
   !!======================================================================
   !!              ***  MODULE  sedco3  ***
   !!    Sediment : carbonate in sediment pore water
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable


   IMPLICIT NONE
   PRIVATE

   !! *  Routine accessibility
   PUBLIC sed_co3     


   !! * Module variables
   REAL(wp) :: epsmax   =  1.e-12      ! convergence limite value

   !!----------------------------------------------------------------------
   !!   OPA 9.0   !   LODYC-IPSL   (2003)
   !!----------------------------------------------------------------------

CONTAINS


   SUBROUTINE sed_co3( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_co3  ***
      !!
      !! ** Purpose :  carbonate ion and proton concentration 
      !!               in sediment pore water
      !!
      !! ** Methode :  - solving nonlinear equation for [H+] with given alkalinity
      !!               and total co2 
      !!               - one dimensional newton-raphson algorithm for [H+])
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) coupled with PISCES
      !!        !  06-04 (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT(in)  :: kt   ! time step

      !
      !---Local variables
      INTEGER  :: jiter, ji, jk, ipt  ! dummy loop indices

      INTEGER  :: itermax             ! maximum number of Newton-Raphson iterations
      INTEGER  :: itime               ! number of time to perform Newton-Raphson algorithm
      LOGICAL  :: lconv = .FALSE.     ! flag for convergence
      REAL(wp) :: brems               !  relaxation. parameter
      REAL(wp) :: zresm, zresm1, zhipor_min 
      REAL(wp) :: zalk, zbor, zsil, zpo4, zdic
      REAL(wp) :: zh_old, zh_old2, zh_old3, zh_old4
      REAL(wp) :: zuu, zvv, zduu, zdvv 
      REAL(wp) :: zup, zvp, zdup, zdvp
      REAL(wp) :: zah_old, zah_olds
      REAL(wp) :: zh_new, zh_new2, zco3

     !!----------------------------------------------------------------------

      IF( kt == nitsed000 ) THEN
         WRITE(numsed,*) ' sed_co3 : carbonate ion and proton concentration calculation  '
         WRITE(numsed,*) ' '
      ENDIF

      itermax     = 30
      brems       = 1.
      itime       = 0


      DO jk = 1, jpksed
10001    CONTINUE
         IF( itime <= 2 ) THEN
            lconv  = .FALSE.
            IF( itime > 0 ) THEN  
               ! increase max number of iterations and relaxation parameter
               itermax = 200
!!               brems   = 0.3
               IF( itime == 2 ) hipor(1:jpoce,jk) = 3.e-9 ! re-initilazation of [H] values
            ENDIF

iflag:      DO jiter = 1, itermax

                ! Store previous hi field.   
               zresm = -1.e10
               ipt = 1
               DO ji = 1, jpoce
                  ! dissociation constant are in mol/kg of solution
                  ! convert pwcp concentration [mol/l] in mol/kg for solution
                  zalk    = pwcp(ji,jk,jwalk) / densSW(ji)
                  zh_old  = hipor(ji,jk) / densSW(ji)
                  zh_old2 = zh_old  * zh_old
                  zh_old3 = zh_old2 * zh_old
                  zh_old4 = zh_old3 * zh_old
                  zbor    = borats(ji) / densSW(ji)
                  zsil    = pwcp(ji,jk,jwsil) / densSW(ji)
                  zpo4    = pwcp(ji,jk,jwpo4) / densSW(ji)
                  zdic    = pwcp(ji,jk,jwdic) / densSW(ji)               
                  ! intermediate calculation 
                  zuu     = zdic * ( ak1s(ji) / zh_old + 2.* ak12s(ji) / zh_old2 )
                  zvv     = 1. + ak1s(ji) / zh_old + ak12s(ji) / zh_old2
                  zduu    = zdic * ( -ak1s(ji) / zh_old2 - 4. * ak12s(ji) / zh_old3 )
                  zdvv    = -ak1s(ji) / zh_old2 - 2. * ak12s(ji) / zh_old3
                  zup     = zpo4 * ( ak12ps(ji) / zh_old2 + 2. * ak123ps(ji) / zh_old3 - 1.)
                  zvp     = 1. + ak1ps(ji) / zh_old + ak12ps(ji) / zh_old2 + ak123ps(ji) / zh_old3
                  zdup    = zpo4 * ( -2. * ak12ps(ji) / zh_old3 - 6. * ak123ps(ji) / zh_old4 )
                  zdvp    = -ak1ps(ji) / zh_old2 - 2.* ak12ps(ji) / zh_old3 - 3. * ak123ps(ji) / zh_old4
                  
                  zah_old  = zuu / zvv + zbor / ( 1. + zh_old / akbs(ji) ) + &
                     &      akws(ji) / zh_old - zh_old + zsil / ( 1. + zh_old / aksis(ji) ) + &
                     &      zup / zvp
                  
                  zah_olds = ( ( zduu * zvv - zdvv * zuu ) / ( zvv * zvv ) )      - &
                     &        zbor / akbs(ji) * ( 1. + zh_old / akbs(ji) )**(-2) - &
                     &        akws(ji) / zh_old2 - 1. -                            &
                     &        zsil / aksis(ji) * ( 1. + zh_old / aksis(ji) )**(-2) + &
                     &       ( ( zdup * zvp - zdvp * zup ) / ( zvp * zvp ) )
                  !
                  zh_new = zh_old - brems * ( zah_old - zalk ) / zah_olds
                  ! 
                  zresm1 = ABS( zh_new - zh_old )
                  IF( zresm1 > zresm ) THEN 
                     zresm = zresm1  
                  ENDIF
                  !
                  zh_new2  = zh_new * zh_new
                  zco3   = ( ak12s(ji) * zdic ) / ( ak12s(ji) + ak1s(ji) * zh_new + zh_new2)
                  ! again in mol/l
                  hipor (ji,jk) = zh_new * densSW(ji)
                  co3por(ji,jk) = zco3   * densSW(ji)
                  
               ENDDO  ! end loop ji
               
               ! convergence test
               IF( zresm <= epsmax ) THEN
                  lconv = .TRUE.
                  !minimum value of hipor
                  zhipor_min = MINVAL( hipor(1:jpoce,jk ) )
                  EXIT iflag
               ENDIF

            ENDDO iflag

            IF( lconv ) THEN
!               WRITE(numsed,*) ' convergence after iter =', jiter, ' iterations ;  res =',zresm  
               IF( zhipor_min < 0. ) THEN 
                  IF ( itime == 0 ) THEN
!                     WRITE(numsed,*) '    but hipor < 0 ; try one more time for jk = ', jk 
!                     WRITE(numsed,*) '    with re-initialization of initial PH field '       
                     itime = 2
                     GOTO 10001
                  ELSE
!                     WRITE(numsed,*) ' convergence after iter =', jiter, ' iterations ;  res =',zresm 
!                     WRITE(numsed,*) '    but hipor < 0, again for second time for jk = ', jk 
!                     WRITE(numsed,*) ' We stop : STOP '
                     STOP
                  ENDIF
               ELSE
!                  WRITE(numsed,*) ' successfull convergence for level jk = ',jk,&
!                     &               '  after iter =', jiter, ' iterations ;  res =',zresm  
!                  WRITE(numsed,*) ' '
                  itime = 0
               ENDIF
            ELSE
               itime = itime + 1
               WRITE(numsed,*) ' No convergence for jk = ', jk, ' after ', itime, '  try'            
               IF ( itime == 1 ) THEN
                  WRITE(numsed,*) ' try one more time with more iterations and higher relax. value'
                  GOTO 10001
               ELSE IF ( itime == 2 ) THEN
                  WRITE(numsed,*) ' try one more time for with more iterations, higher relax. value'               
                  WRITE(numsed,*) ' and with re-initialization of initial PH field ' 
               ELSE       
                  WRITE(numsed,*) ' No more... we stop '
                  STOP
               ENDIF
            ENDIF
         ENDIF
     ENDDO

   END SUBROUTINE sed_co3
#else
   !!======================================================================
   !! MODULE sedco3  :   Dummy module 
   !!======================================================================
CONTAINS
   SUBROUTINE sed_co3( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_co3: You should not have seen this print! error?', kt
   END SUBROUTINE sed_co3

   !!======================================================================

#endif

END MODULE sedco3
