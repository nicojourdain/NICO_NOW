MODULE sedrst
#if defined key_sed
   !!======================================================================
   !!                       *** MODULE sedrst ***
   !!   Read and write the restart files for sediment
   !!======================================================================

   !!----------------------------------------------------------------------
   !! * Modules used
   !! ==============
   USE sed
   USE sedarr


   !! * Accessibility
   IMPLICIT NONE
   PRIVATE

   !! * Accessibility
   PUBLIC sed_rst_read
   PUBLIC sed_rst_wri

   !! * Module variables
   INTEGER, PUBLIC ::   numrsr, numrsw   !: logical unit for sed restart (read and write)
   
   
CONTAINS


   SUBROUTINE sed_rst_read 
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_rst_read  ***
      !!
      !! ** Purpose :  Initialization of sediment module
      !!               - sets initial sediment composition
      !!                 ( only clay or reading restart file )
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  original
      !!----------------------------------------------------------------------
      !! * Modules used
      USE iom

      !! * local declarations
      INTEGER :: ji, jk, jn 
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: zdta
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:  ) :: zdta1 
      REAL(wp), ALLOCATABLE, DIMENSION(:,:    ) :: zhipor
      REAL(wp) :: zkt
      CHARACTER(len = 20) ::   cltra
      INTEGER             ::   jlibalt = jprstlib
      LOGICAL             ::   llok
      !--------------------------------------------------------------------
 

      WRITE(numsed,*) ' '      
      WRITE(numsed,*) ' Initilization of Sediment components from restart'
      WRITE(numsed,*) ' '

      ALLOCATE( zdta(jpi,jpj,jpksed,jptrased), zdta1(jpi,jpj,jpksed,2), zhipor(jpoce,jpksed) ) 

      IF ( jprstlib == jprstdimg ) THEN
        ! eventually read netcdf file (monobloc)  for restarting on different number of processors
        ! if restart_sed.nc exists, then set jlibalt to jpnf90
        INQUIRE( FILE = 'restart_sed.nc', EXIST = llok )
        IF ( llok ) THEN ; jlibalt = jpnf90  ; ELSE ; jlibalt = jprstlib ; ENDIF
      ENDIF

      CALL iom_open( 'restart_sed', numrsr, kiolib = jlibalt )     
      CALL iom_get( numrsr, 'kt'     , zkt      )   ! time-step
 
      DO jn = 1, jptrased
         cltra = sedtrcd(jn)
         CALL iom_get( numrsr, jpdom_unknown, cltra, zdta(:,:,:,jn), &
            &          kstart=(/1,1,1/), kcount=(/jpi,jpj,jpksed/) )
      ENDDO
        

      CALL pack_arr( jpoce, solcp(1:jpoce,1:jpksed,jsopal), &
         &            zdta(1:jpi,1:jpj,1:jpksed,1), iarroce(1:jpoce) )

      CALL pack_arr( jpoce, solcp(1:jpoce,1:jpksed,jsclay), &
         &             zdta(1:jpi,1:jpj,1:jpksed,2), iarroce(1:jpoce) )

      CALL pack_arr( jpoce, solcp(1:jpoce,1:jpksed,jspoc), &
         &             zdta(1:jpi,1:jpj,1:jpksed,3), iarroce(1:jpoce) )

      CALL pack_arr( jpoce, solcp(1:jpoce,1:jpksed,jscal), &
         &             zdta(1:jpi,1:jpj,1:jpksed,4), iarroce(1:jpoce) )

      CALL pack_arr( jpoce, pwcp(1:jpoce,1:jpksed,jwsil), &
         &             zdta(1:jpi,1:jpj,1:jpksed,5), iarroce(1:jpoce) )


      CALL pack_arr( jpoce, pwcp(1:jpoce,1:jpksed,jwoxy), &
         &             zdta(1:jpi,1:jpj,1:jpksed,6), iarroce(1:jpoce) )


      CALL pack_arr( jpoce, pwcp(1:jpoce,1:jpksed,jwdic), &
         &             zdta(1:jpi,1:jpj,1:jpksed,7), iarroce(1:jpoce) )

      CALL pack_arr( jpoce, pwcp(1:jpoce,1:jpksed,jwno3), &
         &             zdta(1:jpi,1:jpj,1:jpksed,8), iarroce(1:jpoce) )

      CALL pack_arr( jpoce, pwcp(1:jpoce,1:jpksed,jwpo4), &
         &             zdta(1:jpi,1:jpj,1:jpksed,9), iarroce(1:jpoce) )

      CALL pack_arr( jpoce, pwcp(1:jpoce,1:jpksed,jwalk), &
         &             zdta(1:jpi,1:jpj,1:jpksed,10), iarroce(1:jpoce) )

      CALL pack_arr( jpoce, pwcp(1:jpoce,1:jpksed,jwc13), &
         &             zdta(1:jpi,1:jpj,1:jpksed,11), iarroce(1:jpoce) )

      DO jn = 1, 2
         cltra = seddia3d(jn)
         CALL iom_get( numrsr, jpdom_unknown, cltra, zdta1(:,:,:,jn), &
            &          kstart=(/1,1,1/), kcount=(/jpi,jpj,jpksed/) )
      ENDDO

      zhipor(:,:) = 0.
      CALL pack_arr( jpoce, zhipor(1:jpoce,1:jpksed), &
         &             zdta1(1:jpi,1:jpj,1:jpksed,1), iarroce(1:jpoce) )

      ! Initialization of [h+] in mol/kg
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            hipor (ji,jk) = 10.**( -1. * zhipor(ji,jk) )
         ENDDO
      ENDDO
      
      CALL pack_arr( jpoce, co3por(1:jpoce,1:jpksed), &
         &             zdta1(1:jpi,1:jpj,1:jpksed,2), iarroce(1:jpoce) )

      ! Initialization of sediment composant only ie jk=2 to jk=jpksed 
      ! ( nothing in jk=1)
      solcp(1:jpoce,1,:) = 0.
      pwcp (1:jpoce,1,:) = 0.

      DEALLOCATE( zdta   )
      DEALLOCATE( zdta1  )
      DEALLOCATE( zhipor )
     
   END SUBROUTINE sed_rst_read

   SUBROUTINE sed_rst_wri( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_rst_wri  ***
      !!
      !! ** Purpose :  save field which are necessary for sediment restart
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  original
      !!----------------------------------------------------------------------
      !!* Modules used
      USE ioipsl
      !! *Arguments
      INTEGER, INTENT(in) ::   kt       ! number of iteration
      !! * local declarations
      INTEGER  :: ji, jk
      INTEGER  :: ic, jc, jn, itime 
      REAL(wp) :: zdate0
      REAL(wp), DIMENSION(1) ::  zinfo
      CHARACTER(len=50) :: clname,cln
      CHARACTER(len=20) :: cltra 
      REAL(wp), DIMENSION(:,:) , ALLOCATABLE  :: zdta   
      !! -----------------------------------------------------------------------

      ALLOCATE( zdta(jpoce,jpksed) )

      IF( MOD(kt,nstock) == 0 .OR. kt == nitsedend ) THEN
         
         !! 0. initialisations
         !! ------------------
         
         IF(lwp) WRITE(numsed,*) ' '
         IF(lwp) WRITE(numsed,*) 'sed_rst_write : write the sediment restart file in NetCDF format ',   &
            'at it= ',kt
         IF(lwp) WRITE(numsed,*) '~~~~~~~~~'
         
         !! 1. WRITE in nutwrs
         !! ------------------

         ic = 1
         DO jc = 1,16
            IF( cexper(jc:jc) /= ' ') ic = jc
         END DO
         WRITE( cln,'("_",i5.5,i2.2,i2.2,"_restart.sed")') nyear, nmonth, nday
         clname = cexper(1:ic)//cln
         itime = 0
         CALL ymds2ju( nyear, nmonth, nday, rdt, zdate0 )
         zdate0 = zdate0 - adatrj   !   set calendar origin to the beginning of the experiment
         CALL restini( 'NONE', jpi, jpj, glamt, gphit, jpksed, dz, &
            &         clname, itime, zdate0, dtsed*nstock, numrsw, domain_id=nidom )
         zinfo(1) = REAL( kt)
         CALL restput( numrsw, 'kt', 1,1, 1,0, zinfo  )



         ! Back to 2D geometry
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,1) , iarroce(1:jpoce), &
            &                    solcp(1:jpoce,1:jpksed,jsopal ) )
         
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,2) , iarroce(1:jpoce), &
            &                    solcp(1:jpoce,1:jpksed,jsclay ) )
         
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,3) , iarroce(1:jpoce), &
            &                    solcp(1:jpoce,1:jpksed,jspoc  ) )
         
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,4) , iarroce(1:jpoce), &
            &                    solcp(1:jpoce,1:jpksed,jscal  ) )   
         
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,5) , iarroce(1:jpoce), &
            &                    pwcp(1:jpoce,1:jpksed,jwsil  )  )
         
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,6)  , iarroce(1:jpoce), &
            &                    pwcp(1:jpoce,1:jpksed,jwoxy  ) )
         
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,7)  , iarroce(1:jpoce), &
            &                    pwcp(1:jpoce,1:jpksed,jwdic  ) )
         
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,8)  , iarroce(1:jpoce), &
            &                    pwcp(1:jpoce,1:jpksed,jwno3  ) )
         
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,9)  , iarroce(1:jpoce), &
            &                    pwcp(1:jpoce,1:jpksed,jwpo4  ) )
         
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,10)  , iarroce(1:jpoce), &
            &                    pwcp(1:jpoce,1:jpksed,jwalk  ) )
         
         CALL unpack_arr( jpoce, trcsedi(1:jpi,1:jpj,1:jpksed,11)  , iarroce(1:jpoce), &
         &                    pwcp(1:jpoce,1:jpksed,jwc13  ) )
         
         ! porosity
         zdta(:,:) = 0.
         DO jk = 1, jpksed
            DO ji = 1, jpoce
               zdta(ji,jk) = -LOG10( hipor(ji,jk) / densSW(ji) )
            ENDDO
         ENDDO
         CALL unpack_arr( jpoce, flxsedi3d(1:jpi,1:jpj,1:jpksed,1)  , iarroce(1:jpoce), &
            &                   zdta(1:jpoce,1:jpksed)  )
         
         CALL unpack_arr( jpoce, flxsedi3d(1:jpi,1:jpj,1:jpksed,2)  , iarroce(1:jpoce), &
            &                   co3por(1:jpoce,1:jpksed)  )
      
         ! prognostic variables
         ! --------------------


         DO jn = 1, jptrased
            cltra = sedtrcd(jn)
            CALL restput( numrsw, cltra, jpi, jpj, jpksed, 0, trcsedi(:,:,:,jn) )
         ENDDO

         DO jn = 1, 2
            cltra = seddia3d(jn)
            CALL restput( numrsw, cltra, jpi, jpj, jpksed, 0, flxsedi3d(:,:,:,jn) )
         ENDDO


         CALL restclo( numrsw )

      ENDIF

      DEALLOCATE( zdta )  
         
   END SUBROUTINE sed_rst_wri
#else
   !!======================================================================
   !! MODULE sedrst :   Dummy module 
   !!======================================================================
CONTAINS
   SUBROUTINE sed_rst_read                      ! Empty routines
   END SUBROUTINE sed_rst_read
   SUBROUTINE sed_rst_wri( kt )
      INTEGER, INTENT ( in ) :: kt
      WRITE(*,*) 'sed_rst_wri: You should not have seen this print! error?', kt
   END SUBROUTINE sed_rst_wri   
#endif

END MODULE sedrst
