MODULE limrst
   !!======================================================================
   !!                     ***  MODULE  limrst  ***
   !! Ice restart :  write the ice restart file
   !!======================================================================
   !! History:   -   ! 2005-04 (M. Vancoppenolle) Original code
   !!           3.0  ! 2008-03 (C. Ethe) restart files in using IOM interface
   !!           4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                   LIM sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_rst_opn     : open ice restart file
   !!   lim_rst_write   : write of the restart file 
   !!   lim_rst_read    : read  the restart file 
   !!----------------------------------------------------------------------
   USE ice              ! sea-ice variables
   USE par_ice          ! sea-ice parameters
   USE dom_oce          ! ocean domain
   USE sbc_oce          ! Surface boundary condition: ocean fields
   USE sbc_ice          ! Surface boundary condition: ice fields
   USE in_out_manager   ! I/O manager
   USE iom              ! I/O library
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_rst_opn    ! routine called by icestep.F90
   PUBLIC   lim_rst_write  ! routine called by icestep.F90
   PUBLIC   lim_rst_read   ! routine called by iceini.F90

   LOGICAL, PUBLIC ::   lrst_ice         !: logical to control the ice restart write 
   INTEGER, PUBLIC ::   numrir, numriw   !: logical unit for ice restart (read and write)

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limrst.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_rst_opn( kt )
      !!----------------------------------------------------------------------
      !!                    ***  lim_rst_opn  ***
      !!
      !! ** purpose  :   output of sea-ice variable in a netcdf file
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! number of iteration
      !
      CHARACTER(LEN=20)   ::   clkt     ! ocean time-step define as a character
      CHARACTER(LEN=50)   ::   clname   ! ice output restart file name
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 )   lrst_ice = .FALSE.   ! default definition

      ! in order to get better performances with NetCDF format, we open and define the ice restart file 
      ! one ice time step before writing the data (-> at nitrst - 2*nn_fsbc + 1), except if we write ice 
      ! restart files every ice time step or if an ice restart file was writen at nitend - 2*nn_fsbc + 1
      IF( kt == nitrst - 2*nn_fsbc + 1 .OR. nstock == nn_fsbc    &
         &                             .OR. ( kt == nitend - nn_fsbc + 1 .AND. .NOT. lrst_ice ) ) THEN
         ! beware of the format used to write kt (default is i8.8, that should be large enough...)
         IF( nitrst > 99999999 ) THEN   ;   WRITE(clkt, *       ) nitrst
         ELSE                           ;   WRITE(clkt, '(i8.8)') nitrst
         ENDIF
         ! create the file
         clname = TRIM(cexper)//"_"//TRIM(ADJUSTL(clkt))//"_"//TRIM(cn_icerst_out)
         IF(lwp) THEN
            WRITE(numout,*)
            SELECT CASE ( jprstlib )
            CASE ( jprstdimg )   ;   WRITE(numout,*) '             open ice restart binary file: '//clname
            CASE DEFAULT         ;   WRITE(numout,*) '             open ice restart NetCDF file: '//clname
            END SELECT
            IF( kt == nitrst - 2*nn_fsbc + 1 ) THEN   
               WRITE(numout,*)         '             kt = nitrst - 2*nn_fsbc + 1 = ', kt,' date= ', ndastp
            ELSE   ;   WRITE(numout,*) '             kt = '                         , kt,' date= ', ndastp
            ENDIF
         ENDIF
         !
         CALL iom_open( clname, numriw, ldwrt = .TRUE., kiolib = jprstlib )
         lrst_ice = .TRUE.
      ENDIF
      !
   END SUBROUTINE lim_rst_opn


   SUBROUTINE lim_rst_write( kt )
      !!----------------------------------------------------------------------
      !!                    ***  lim_rst_write  ***
      !!
      !! ** purpose  :   output of sea-ice variable in a netcdf file
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! number of iteration
      !!
      INTEGER ::   ji, jj, jk ,jl   ! dummy loop indices
      INTEGER ::   iter
      CHARACTER(len=15) ::   znam
      CHARACTER(len=1)  ::   zchar, zchar1
      REAL(wp), POINTER, DIMENSION(:,:) :: z2d
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, z2d )

      iter = kt + nn_fsbc - 1   ! ice restarts are written at kt == nitrst - nn_fsbc + 1

      IF( iter == nitrst ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'lim_rst_write : write ice restart file  kt =', kt
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~'         
      ENDIF

      ! Write in numriw (if iter == nitrst)
      ! ------------------ 
      !                                                                        ! calendar control
      CALL iom_rstput( iter, nitrst, numriw, 'nn_fsbc', REAL( nn_fsbc, wp ) )      ! time-step 
      CALL iom_rstput( iter, nitrst, numriw, 'kt_ice' , REAL( iter   , wp ) )      ! date

      ! Prognostic variables 
      DO jl = 1, jpl 
         WRITE(zchar,'(I1)') jl
         znam = 'v_i'//'_htc'//zchar
         z2d(:,:) = v_i(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'v_s'//'_htc'//zchar
         z2d(:,:) = v_s(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'smv_i'//'_htc'//zchar
         z2d(:,:) = smv_i(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'oa_i'//'_htc'//zchar
         z2d(:,:) = oa_i(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'a_i'//'_htc'//zchar
         z2d(:,:) = a_i(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 't_su'//'_htc'//zchar
         z2d(:,:) = t_su(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
      END DO

      DO jl = 1, jpl 
         WRITE(zchar,'(I1)') jl
         znam = 'tempt_sl1'//'_htc'//zchar
         z2d(:,:) = e_s(:,:,1,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
      END DO

      DO jl = 1, jpl 
         WRITE(zchar,'(I1)') jl
         DO jk = 1, nlay_i 
            WRITE(zchar1,'(I1)') jk
            znam = 'tempt'//'_il'//zchar1//'_htc'//zchar
            z2d(:,:) = e_i(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         END DO
      END DO

      CALL iom_rstput( iter, nitrst, numriw, 'u_ice'     , u_ice      )
      CALL iom_rstput( iter, nitrst, numriw, 'v_ice'     , v_ice      )
      CALL iom_rstput( iter, nitrst, numriw, 'fsbbq'     , fsbbq      )
      CALL iom_rstput( iter, nitrst, numriw, 'stress1_i' , stress1_i  )
      CALL iom_rstput( iter, nitrst, numriw, 'stress2_i' , stress2_i  )
      CALL iom_rstput( iter, nitrst, numriw, 'stress12_i', stress12_i )

      DO jl = 1, jpl 
         WRITE(zchar,'(I1)') jl
         znam = 'sxice'//'_htc'//zchar
         z2d(:,:) = sxice(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syice'//'_htc'//zchar
         z2d(:,:) = syice(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxice'//'_htc'//zchar
         z2d(:,:) = sxxice(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syyice'//'_htc'//zchar
         z2d(:,:) = syyice(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxyice'//'_htc'//zchar
         z2d(:,:) = sxyice(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxsn'//'_htc'//zchar
         z2d(:,:) = sxsn(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sysn'//'_htc'//zchar
         z2d(:,:) = sysn(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxsn'//'_htc'//zchar
         z2d(:,:) = sxxsn(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syysn'//'_htc'//zchar
         z2d(:,:) = syysn(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxysn'//'_htc'//zchar
         z2d(:,:) = sxysn(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxa'//'_htc'//zchar
         z2d(:,:) = sxa(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sya'//'_htc'//zchar
         z2d(:,:) = sya(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxa'//'_htc'//zchar
         z2d(:,:) = sxxa(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syya'//'_htc'//zchar
         z2d(:,:) = syya(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxya'//'_htc'//zchar
         z2d(:,:) = sxya(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxc0'//'_htc'//zchar
         z2d(:,:) = sxc0(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syc0'//'_htc'//zchar
         z2d(:,:) = syc0(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxc0'//'_htc'//zchar
         z2d(:,:) = sxxc0(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syyc0'//'_htc'//zchar
         z2d(:,:) = syyc0(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxyc0'//'_htc'//zchar
         z2d(:,:) = sxyc0(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxsal'//'_htc'//zchar
         z2d(:,:) = sxsal(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sysal'//'_htc'//zchar
         z2d(:,:) = sysal(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxsal'//'_htc'//zchar
         z2d(:,:) = sxxsal(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syysal'//'_htc'//zchar
         z2d(:,:) = syysal(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxysal'//'_htc'//zchar
         z2d(:,:) = sxysal(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxage'//'_htc'//zchar
         z2d(:,:) = sxage(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syage'//'_htc'//zchar
         z2d(:,:) = syage(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxxage'//'_htc'//zchar
         z2d(:,:) = sxxage(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'syyage'//'_htc'//zchar
         z2d(:,:) = syyage(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         znam = 'sxyage'//'_htc'//zchar
         z2d(:,:) = sxyage(:,:,jl)
         CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
      END DO

      CALL iom_rstput( iter, nitrst, numriw, 'sxopw ' ,  sxopw  )
      CALL iom_rstput( iter, nitrst, numriw, 'syopw ' ,  syopw  )
      CALL iom_rstput( iter, nitrst, numriw, 'sxxopw' ,  sxxopw )
      CALL iom_rstput( iter, nitrst, numriw, 'syyopw' ,  syyopw )
      CALL iom_rstput( iter, nitrst, numriw, 'sxyopw' ,  sxyopw )

      DO jl = 1, jpl 
         WRITE(zchar,'(I1)') jl
         DO jk = 1, nlay_i 
            WRITE(zchar1,'(I1)') jk
            znam = 'sxe'//'_il'//zchar1//'_htc'//zchar
            z2d(:,:) = sxe(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
            znam = 'sye'//'_il'//zchar1//'_htc'//zchar
            z2d(:,:) = sye(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
            znam = 'sxxe'//'_il'//zchar1//'_htc'//zchar
            z2d(:,:) = sxxe(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
            znam = 'syye'//'_il'//zchar1//'_htc'//zchar
            z2d(:,:) = syye(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
            znam = 'sxye'//'_il'//zchar1//'_htc'//zchar
            z2d(:,:) = sxye(:,:,jk,jl)
            CALL iom_rstput( iter, nitrst, numriw, znam , z2d )
         END DO
      END DO

      IF( iter == nitrst ) THEN
         CALL iom_close( numriw )                         ! close the restart file
         lrst_ice = .FALSE.
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, z2d )
      !
   END SUBROUTINE lim_rst_write


   SUBROUTINE lim_rst_read
      !!----------------------------------------------------------------------
      !!                    ***  lim_rst_read  ***
      !!
      !! ** purpose  :   read of sea-ice variable restart in a netcdf file
      !!----------------------------------------------------------------------
      INTEGER :: ji, jj, jk, jl, indx
      REAL(wp) ::   zfice, ziter
      REAL(wp) ::   zs_inf, z_slope_s, zsmax, zsmin, zalpha, zindb   ! local scalars used for the salinity profile
      REAL(wp), POINTER, DIMENSION(:)  ::   zs_zero 
      REAL(wp), POINTER, DIMENSION(:,:) ::   z2d
      CHARACTER(len=15) ::   znam
      CHARACTER(len=1)  ::   zchar, zchar1
      INTEGER           ::   jlibalt = jprstlib
      LOGICAL           ::   llok
      !!----------------------------------------------------------------------

      CALL wrk_alloc( nlay_i, zs_zero )
      CALL wrk_alloc( jpi, jpj, z2d )

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_rst_read : read ice NetCDF restart file'
         WRITE(numout,*) '~~~~~~~~~~~~~'
      ENDIF

      IF ( jprstlib == jprstdimg ) THEN
        ! eventually read netcdf file (monobloc)  for restarting on different number of processors
        ! if {cn_icerst_in}.nc exists, then set jlibalt to jpnf90
        INQUIRE( FILE = TRIM(cn_icerst_in)//'.nc', EXIST = llok )
        IF ( llok ) THEN ; jlibalt = jpnf90  ; ELSE ; jlibalt = jprstlib ; ENDIF
      ENDIF

      CALL iom_open ( cn_icerst_in, numrir, kiolib = jprstlib )

      CALL iom_get( numrir, 'nn_fsbc', zfice )
      CALL iom_get( numrir, 'kt_ice' , ziter )    
      IF(lwp) WRITE(numout,*) '   read ice restart file at time step    : ', ziter
      IF(lwp) WRITE(numout,*) '   in any case we force it to nit000 - 1 : ', nit000 - 1

      !Control of date

      IF( ( nit000 - INT(ziter) ) /= 1 .AND. ABS( nrstdt ) == 1 )   &
         &     CALL ctl_stop( 'lim_rst_read ===>>>> : problem with nit000 in ice restart',  &
         &                   '   verify the file or rerun with the value 0 for the',        &
         &                   '   control of time parameter  nrstdt' )
      IF( INT(zfice) /= nn_fsbc          .AND. ABS( nrstdt ) == 1 )   &
         &     CALL ctl_stop( 'lim_rst_read ===>>>> : problem with nn_fsbc in ice restart',  &
         &                   '   verify the file or rerun with the value 0 for the',         &
         &                   '   control of time parameter  nrstdt' )

      DO jl = 1, jpl 
         WRITE(zchar,'(I1)') jl
         znam = 'v_i'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         v_i(:,:,jl) = z2d(:,:)
         znam = 'v_s'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         v_s(:,:,jl) = z2d(:,:) 
         znam = 'smv_i'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         smv_i(:,:,jl) = z2d(:,:)
         znam = 'oa_i'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         oa_i(:,:,jl) = z2d(:,:)
         znam = 'a_i'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         a_i(:,:,jl) = z2d(:,:)
         znam = 't_su'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         t_su(:,:,jl) = z2d(:,:)
      END DO

      DO jl = 1, jpl 
         CALL lbc_lnk( smv_i(:,:,jl) , 'T' ,  1. )
         CALL lbc_lnk( v_i  (:,:,jl) , 'T' ,  1. )
         CALL lbc_lnk( a_i  (:,:,jl) , 'T' ,  1. )
      END DO

      ! we first with bulk ice salinity
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               zindb          = MAX( 0.0 , SIGN( 1.0 , v_i(ji,jj,jl) - 1.0e-4 ) ) 
               sm_i(ji,jj,jl) = smv_i(ji,jj,jl) / MAX(v_i(ji,jj,jl),1.0e-6) * zindb
               ht_i(ji,jj,jl) = v_i(ji,jj,jl)   / MAX(a_i(ji,jj,jl),1.0e-6) * zindb
            END DO
         END DO
      END DO

      DO jk = 1, nlay_i
         s_i(:,:,jk,:) = sm_i(:,:,:)
      END DO

      IF( num_sal == 2 ) THEN      ! Salinity profile
         DO jl = 1, jpl
            DO jk = 1, nlay_i
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     zs_inf        = sm_i(ji,jj,jl)
                     z_slope_s     = 2._wp * sm_i(ji,jj,jl) / MAX( 0.01_wp , ht_i(ji,jj,jl) )
                     !- slope of the salinity profile
                     zs_zero(jk)   = z_slope_s * ( REAL(jk,wp) - 0.5_wp ) * ht_i(ji,jj,jl) / REAL(nlay_i,wp)
                     zsmax = 4.5_wp
                     zsmin = 3.5_wp
                     IF( sm_i(ji,jj,jl) .LT. zsmin ) THEN
                        zalpha = 1._wp
                     ELSEIF( sm_i(ji,jj,jl) .LT.zsmax ) THEN
                        zalpha = sm_i(ji,jj,jl) / ( zsmin - zsmax ) + zsmax / ( zsmax - zsmin )
                     ELSE
                        zalpha = 0._wp
                     ENDIF
                     s_i(ji,jj,jk,jl) = zalpha * zs_zero(jk) + ( 1._wp - zalpha ) * zs_inf
                  END DO
               END DO
            END DO
         END DO
      ENDIF

      DO jl = 1, jpl 
         WRITE(zchar,'(I1)') jl
         znam = 'tempt_sl1'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         e_s(:,:,1,jl) = z2d(:,:)
      END DO

      DO jl = 1, jpl 
         WRITE(zchar,'(I1)') jl
         DO jk = 1, nlay_i 
            WRITE(zchar1,'(I1)') jk
            znam = 'tempt'//'_il'//zchar1//'_htc'//zchar
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            e_i(:,:,jk,jl) = z2d(:,:)
         END DO
      END DO

      CALL iom_get( numrir, jpdom_autoglo, 'u_ice'     , u_ice      )
      CALL iom_get( numrir, jpdom_autoglo, 'v_ice'     , v_ice      )
      CALL iom_get( numrir, jpdom_autoglo, 'fsbbq'     , fsbbq      )
      CALL iom_get( numrir, jpdom_autoglo, 'stress1_i' , stress1_i  )
      CALL iom_get( numrir, jpdom_autoglo, 'stress2_i' , stress2_i  )
      CALL iom_get( numrir, jpdom_autoglo, 'stress12_i', stress12_i )

      DO jl = 1, jpl 
         WRITE(zchar,'(I1)') jl
         znam = 'sxice'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxice(:,:,jl) = z2d(:,:)
         znam = 'syice'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syice(:,:,jl) = z2d(:,:)
         znam = 'sxxice'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxice(:,:,jl) = z2d(:,:)
         znam = 'syyice'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syyice(:,:,jl) = z2d(:,:)
         znam = 'sxyice'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxyice(:,:,jl) = z2d(:,:)
         znam = 'sxsn'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxsn(:,:,jl) = z2d(:,:)
         znam = 'sysn'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sysn(:,:,jl) = z2d(:,:)
         znam = 'sxxsn'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxsn(:,:,jl) = z2d(:,:)
         znam = 'syysn'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syysn(:,:,jl) = z2d(:,:)
         znam = 'sxysn'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxysn(:,:,jl) = z2d(:,:)
         znam = 'sxa'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxa(:,:,jl) = z2d(:,:)
         znam = 'sya'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sya(:,:,jl) = z2d(:,:)
         znam = 'sxxa'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxa(:,:,jl) = z2d(:,:)
         znam = 'syya'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syya(:,:,jl) = z2d(:,:)
         znam = 'sxya'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxya(:,:,jl) = z2d(:,:)
         znam = 'sxc0'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxc0(:,:,jl) = z2d(:,:)
         znam = 'syc0'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syc0(:,:,jl) = z2d(:,:)
         znam = 'sxxc0'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxc0(:,:,jl) = z2d(:,:)
         znam = 'syyc0'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syyc0(:,:,jl) = z2d(:,:)
         znam = 'sxyc0'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxyc0(:,:,jl) = z2d(:,:)
         znam = 'sxsal'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxsal(:,:,jl) = z2d(:,:)
         znam = 'sysal'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sysal(:,:,jl) = z2d(:,:)
         znam = 'sxxsal'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxsal(:,:,jl) = z2d(:,:)
         znam = 'syysal'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syysal(:,:,jl) = z2d(:,:)
         znam = 'sxysal'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxysal(:,:,jl) = z2d(:,:)
         znam = 'sxage'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxage(:,:,jl) = z2d(:,:)
         znam = 'syage'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syage(:,:,jl) = z2d(:,:)
         znam = 'sxxage'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxxage(:,:,jl) = z2d(:,:)
         znam = 'syyage'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         syyage(:,:,jl) = z2d(:,:)
         znam = 'sxyage'//'_htc'//zchar
         CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
         sxyage(:,:,jl)= z2d(:,:)
      END DO

      CALL iom_get( numrir, jpdom_autoglo, 'sxopw ' ,  sxopw  )
      CALL iom_get( numrir, jpdom_autoglo, 'syopw ' ,  syopw  )
      CALL iom_get( numrir, jpdom_autoglo, 'sxxopw' ,  sxxopw )
      CALL iom_get( numrir, jpdom_autoglo, 'syyopw' ,  syyopw )
      CALL iom_get( numrir, jpdom_autoglo, 'sxyopw' ,  sxyopw )

      DO jl = 1, jpl 
         WRITE(zchar,'(I1)') jl
         DO jk = 1, nlay_i 
            WRITE(zchar1,'(I1)') jk
            znam = 'sxe'//'_il'//zchar1//'_htc'//zchar
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            sxe(:,:,jk,jl) = z2d(:,:)
            znam = 'sye'//'_il'//zchar1//'_htc'//zchar
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            sye(:,:,jk,jl) = z2d(:,:)
            znam = 'sxxe'//'_il'//zchar1//'_htc'//zchar
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            sxxe(:,:,jk,jl) = z2d(:,:)
            znam = 'syye'//'_il'//zchar1//'_htc'//zchar
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            syye(:,:,jk,jl) = z2d(:,:)
            znam = 'sxye'//'_il'//zchar1//'_htc'//zchar
            CALL iom_get( numrir, jpdom_autoglo, znam , z2d )
            sxye(:,:,jk,jl) = z2d(:,:)
         END DO
      END DO
      !
      CALL iom_close( numrir )
      !
      CALL wrk_dealloc( nlay_i, zs_zero )
      CALL wrk_dealloc( jpi, jpj, z2d )
      !
   END SUBROUTINE lim_rst_read

#else
   !!----------------------------------------------------------------------
   !!   Default option :       Empty module            NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_rst_read             ! Empty routine
   END SUBROUTINE lim_rst_read
   SUBROUTINE lim_rst_write            ! Empty routine
   END SUBROUTINE lim_rst_write
#endif

   !!======================================================================
END MODULE limrst
