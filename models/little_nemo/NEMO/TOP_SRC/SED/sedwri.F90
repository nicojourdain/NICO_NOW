MODULE sedwri
#if defined key_sed
   !!======================================================================
   !!                     ***  MODULE  sedwri  ***
   !!         Sediment diagnostics :  write sediment output files
   !!======================================================================
   USE sed
   USE sedarr
   USE ioipsl
   USE dianam    ! build name of file (routine)

   IMPLICIT NONE
   PRIVATE

   !! * Accessibility
   PUBLIC sed_wri 

   INTEGER  :: nised
   INTEGER  :: nhorised
   INTEGER  :: ndimt52
   INTEGER  :: ndimt51
   INTEGER  :: ndepsed
   REAL(wp) :: zjulian
   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) :: ndext52  
   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) :: ndext51

CONTAINS

   !!----------------------------------------------------------------------
   !!                                                   NetCDF output file
   !!----------------------------------------------------------------------
   SUBROUTINE sed_wri( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_wri  ***
      !!
      !! ** Purpose :  output of sediment passive tracer
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  original
      !!----------------------------------------------------------------------

      INTEGER, INTENT(in) :: kt

      CHARACTER(len = 60)  ::  clhstnam, clop
      INTEGER  :: ji, jk, js, jw, jn
      REAL(wp) :: zsto,zout, zdt
      INTEGER  :: iimi, iima, ijmi, ijma,ipk, it, itmod
      CHARACTER(len = 20)  ::  cltra , cltrau
      CHARACTER(len = 80)  ::  cltral
      REAL(wp)  :: zrate
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: zdta, zflx

      !!-------------------------------------------------------------------


      ! Initialisation
      ! ----------------- 
      IF( kt == nittrc000 )   ALLOCATE( ndext52(jpij*jpksed), ndext51(jpij) )

      ! Define frequency of output and means
      zdt = dtsed
      IF( ln_mskland )   THEN   ;   clop = "only(x)"   ! put 1.e+20 on land (very expensive!!)
      ELSE                      ;   clop = "x"         ! no use of the mask value (require less cpu time)
      ENDIF
#if defined key_diainstant
      zsto = nwrised * zdt
      clop = "inst("//TRIM(clop)//")"
#else
      zsto = zdt
      clop = "ave("//TRIM(clop)//")"
#endif
      zout = nwrised * zdt

      ! Define indices of the horizontal output zoom and vertical limit storage
      iimi = 1      ;      iima = jpi
      ijmi = 1      ;      ijma = jpj
      ipk = jpksed

      ! define time axis
      it = kt
      itmod = kt - nitsed000 + 1


      ! 1.  Initilisations
      ! -----------------------------------------------------------------
      WRITE(numsed,*) ' '
      WRITE(numsed,*) 'sed_wri kt = ', kt
      WRITE(numsed,*) ' '
      
      ALLOCATE( zdta(jpoce,jpksed) )    ;   ALLOCATE( zflx(jpoce,jpwatp1) )


      ! 2.  Back to 2D geometry
      ! -----------------------------------------------------------------
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
      
      
      ! computation of delta 13C
      zdta(:,:) = 0.
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zdta(ji,jk) = ( ( pwcp(ji,jk,jwc13) / pwcp(ji,jk,jwdic) / pdb ) - 1. ) &
               &              * 1000.
         ENDDO
      ENDDO
      CALL unpack_arr( jpoce, flxsedi3d(1:jpi,1:jpj,1:jpksed,3)  , iarroce(1:jpoce), &
         &                   zdta(1:jpoce,1:jpksed)  )
      
 
      zflx(:,:) = 0.    
      ! Calculation of fluxes mol/cm2/s
      DO jw = 1, jpwat
         DO ji = 1, jpoce
            zflx(ji,jw) = ( pwcp(ji,1,jw) - pwcp_dta(ji,jw) ) &
               &         * 1.e-3 * dzkbot(ji) / dtsed
         ENDDO
      ENDDO
      ! Calculation of accumulation rate per dt
      DO js = 1, jpsol
         zrate =  mol_wgt(js) / ( dens * por1(jpksed) ) / dtsed
         DO ji = 1, jpoce
            zflx(ji,jpwatp1) = zflx(ji,jpwatp1) + ( tosed(ji,js) - fromsed(ji,js) ) * zrate
         ENDDO
      ENDDO

      CALL unpack_arr( jpoce, flxsedi2d(1:jpi,1:jpj,1), iarroce(1:jpoce), zflx(1:jpoce,1)  )
      CALL unpack_arr( jpoce, flxsedi2d(1:jpi,1:jpj,2), iarroce(1:jpoce), zflx(1:jpoce,2)  )
      CALL unpack_arr( jpoce, flxsedi2d(1:jpi,1:jpj,3), iarroce(1:jpoce), zflx(1:jpoce,3)  )
      CALL unpack_arr( jpoce, flxsedi2d(1:jpi,1:jpj,4), iarroce(1:jpoce), zflx(1:jpoce,4)  )
      CALL unpack_arr( jpoce, flxsedi2d(1:jpi,1:jpj,5), iarroce(1:jpoce), zflx(1:jpoce,5)  )
      CALL unpack_arr( jpoce, flxsedi2d(1:jpi,1:jpj,6), iarroce(1:jpoce), zflx(1:jpoce,6)  )
      CALL unpack_arr( jpoce, flxsedi2d(1:jpi,1:jpj,7), iarroce(1:jpoce), zflx(1:jpoce,8)  )


      ! 3. Define NETCDF files and fields at beginning of first time step
      ! -----------------------------------------------------------------

      IF( kt == nitsed000 ) THEN

         ! Define the NETCDF files        
         CALL ymds2ju ( nyear, nmonth, nday, rdt, zjulian )
         zjulian = zjulian - adatrj   !   set calendar origin to the beginning of the experiment
         CALL dia_nam ( clhstnam, nwrised, 'sed_T' )
         CALL histbeg ( clhstnam, jpi, glamt, jpj, gphit,     &
            &             iimi, iima-iimi+1, ijmi, ijma-ijmi+1,         &
            &             nitsed000-1, zjulian, zdt,  nhorised, nised , domain_id=nidom, snc4chunks=snc4set )
         CALL histvert( nised,'deptht','Vertic.sed.T levels','m',ipk, profsed, ndepsed, 'down' )
         CALL wheneq  ( jpi*jpj*ipk, tmasksed, 1, 1., ndext52, ndimt52 )
         CALL wheneq  ( jpi*jpj, tmasksed(:,:,1), 1, 1., ndext51, ndimt51 )

         ! Declare all the output fields as NETCDF variables

         DO jn = 1, jptrased
            cltra  = sedtrcd(jn)   ! short title for sediment variable
            cltral = sedtrcl(jn)   ! long title for  sediment variable
            cltrau = sedtrcu(jn)   ! unit for  sediment variable

            CALL histdef( nised, cltra,cltral,cltrau, jpi, jpj, nhorised, &
               &          ipk, 1, ipk, ndepsed, 32, clop, zsto, zout )
         ENDDO

         ! 3D diagnostic
         DO jn = 1, jpdia3dsed
            cltra  = seddia3d(jn)   ! short title for 3D diagnostic
            cltral = seddia3l(jn)   ! long title for 3D diagnostic
            cltrau = seddia3u(jn)   ! UNIT for 3D diagnostic

            CALL histdef( nised, cltra,cltral,cltrau, jpi, jpj, nhorised, &
               &          ipk, 1, ipk, ndepsed, 32, clop, zsto, zout  )
         ENDDO

         ! Fluxes
         DO jn = 1, jpdia2dsed
            cltra  = seddia2d(jn)   ! short title for 2D diagnostic
            cltral = seddia2l(jn)   ! long title for 2D diagnostic
            cltrau = seddia2u(jn)   ! UNIT for 2D diagnostic
            
            CALL histdef( nised, cltra,cltral,cltrau, jpi, jpj, nhorised, &
               &          1, 1, 1, -99, 32, clop, zsto, zout )
         ENDDO


         CALL histend( nised, snc4set )

         WRITE(numsed,*)
         WRITE(numsed,*) 'End of NetCDF sediment output file Initialization'

       ENDIF

       ! Start writing data
       ! ---------------------
       DO jn = 1, jptrased
          cltra = sedtrcd(jn) ! short title for 3D diagnostic
          CALL histwrite( nised, cltra, it, trcsedi(:,:,:,jn), ndimt52, ndext52 )
       END DO

       DO jn = 1, jpdia3dsed
          cltra = seddia3d(jn) ! short title for 3D diagnostic
          CALL histwrite( nised, cltra, it, flxsedi3d(:,:,:,jn), ndimt52, ndext52 )
       END DO

       DO jn = 1, jpdia2dsed
             cltra = seddia2d(jn) ! short title for 2D diagnostic
             CALL histwrite( nised, cltra, it, flxsedi2d(:,:,jn  ), ndimt51, ndext51 )
       END DO


      ! 3. Closing all files
      ! --------------------
      IF( kt == nitsedend  ) THEN
          CALL histclo( nised )
      ENDIF

      DEALLOCATE( zdta )    ;   DEALLOCATE( zflx )

   END SUBROUTINE sed_wri

#else
   !!======================================================================
   !! MODULE sedwri  :   Dummy module
   !!======================================================================
CONTAINS
   SUBROUTINE sed_wri( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_adv: You should not have seen this print! error?', kt
   END SUBROUTINE sed_wri

   !!======================================================================
#endif

END MODULE sedwri
