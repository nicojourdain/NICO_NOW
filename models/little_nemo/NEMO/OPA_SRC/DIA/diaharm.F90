MODULE diaharm 

#if defined key_diaharm && defined key_tide
   !!=================================================================================
   !!                       ***  MODULE  diaharm  ***
   !! Harmonic analysis of tidal constituents 
   !!=================================================================================
   !! * Modules used
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain
   USE in_out_manager  ! I/O units
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE ioipsl          ! NetCDF IPSL library
   USE diadimg         ! To write dimg
   USE phycst
   USE dynspg_oce
   USE dynspg_ts
   USE daymod
   USE tide_mod
   USE iom 
   USE timing          ! preformance summary
   USE wrk_nemo        ! working arrays

   IMPLICIT NONE
   PRIVATE

   LOGICAL, PUBLIC, PARAMETER :: lk_diaharm  = .TRUE.
   
   INTEGER, PARAMETER :: jpincomax    = 2.*jpmax_harmo
   INTEGER, PARAMETER :: jpdimsparse  = jpincomax*300*24

   INTEGER ::                            & !! namelist variables
                         nit000_han = 1, & ! First time step used for harmonic analysis
                         nitend_han = 1, & ! Last time step used for harmonic analysis
                         nstep_han  = 1, & ! Time step frequency for harmonic analysis
                         nb_ana            ! Number of harmonics to analyse

   INTEGER , ALLOCATABLE, DIMENSION(:)       :: name
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: ana_temp
   REAL(wp), ALLOCATABLE, DIMENSION(:)       :: ana_freq, vt, ut, ft
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:)   :: out_eta, &
                                                out_u  , &
                                                out_v

   INTEGER :: ninco, nsparse
   INTEGER ,       DIMENSION(jpdimsparse)         :: njsparse, nisparse
   INTEGER , SAVE, DIMENSION(jpincomax)           :: ipos1
   REAL(wp),       DIMENSION(jpdimsparse)         :: valuesparse
   REAL(wp),       DIMENSION(jpincomax)           :: ztmp4 , ztmp7
   REAL(wp), SAVE, DIMENSION(jpincomax,jpincomax) :: ztmp3 , zpilier
   REAL(wp), SAVE, DIMENSION(jpincomax)           :: zpivot

   CHARACTER (LEN=4), DIMENSION(jpmax_harmo) ::   &
       tname         ! Names of tidal constituents ('M2', 'K1',...)


!! * Routine accessibility
   PUBLIC  dia_harm    ! routine called by step.F90

   !!---------------------------------------------------------------------------------
   !!  
   !!---------------------------------------------------------------------------------

CONTAINS

   SUBROUTINE dia_harm_init 
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE dia_harm_init  ***
      !!----------------------------------------------------------------------
      !!         
      !! ** Purpose :   Initialization of tidal harmonic analysis
      !!
      !! ** Method  :   Initialize frequency array and  nodal factor for nit000_han
      !!
      !! History :
      !!   9.0  O. Le Galloudec and J. Chanut (Original)
      !!--------------------------------------------------------------------
      !! * Local declarations 
      INTEGER :: jh, nhan, jk, ji

      NAMELIST/nam_diaharm/ nit000_han, nitend_han, nstep_han, tname
      !!----------------------------------------------------------------------

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dia_harm_init: Tidal harmonic analysis initialization'
         WRITE(numout,*) '~~~~~~~ '
      ENDIF
      !
      CALL tide_init_Wave
      !
      tname(:)=''
      !
      ! Read Namelist nam_diaharm
      REWIND ( numnam )
      READ   ( numnam, nam_diaharm )
      !
      IF(lwp) THEN
         WRITE(numout,*) 'First time step used for analysis:  nit000_han= ', nit000_han
         WRITE(numout,*) 'Last  time step used for analysis:  nitend_han= ', nitend_han
         WRITE(numout,*) 'Time step frequency for harmonic analysis:  nstep_han= ', nstep_han
      ENDIF

      ! Basic checks on harmonic analysis time window:
      ! ----------------------------------------------
      IF (nit000 > nit000_han) THEN
        IF(lwp) WRITE(numout,*) ' E R R O R dia_harm_init : nit000_han must be greater than nit000, stop'
        IF(lwp) WRITE(numout,*) ' restart capability not implemented'
        nstop = nstop + 1
      ENDIF
      IF (nitend < nitend_han) THEN
        IF(lwp) WRITE(numout,*) ' E R R O R dia_harm_init : nitend_han must be lower than nitend, stop'
        IF(lwp) WRITE(numout,*) ' restart capability not implemented'
        nstop = nstop + 1
      ENDIF

      IF (MOD(nitend_han-nit000_han+1,nstep_han).NE.0) THEN
        IF(lwp) WRITE(numout,*) ' E R R O R dia_harm_init : analysis time span must be a multiple of nstep_han, stop'
        nstop = nstop + 1
      END IF

      nb_ana=0
      DO jk=1,jpmax_harmo
         DO ji=1,jpmax_harmo
            IF(TRIM(tname(jk)) == Wave(ji)%cname_tide) THEN
               nb_ana=nb_ana+1
            ENDIF
         END DO
      ENDDO
      !
      IF(lwp) THEN
         WRITE(numout,*) '        Namelist nam_diaharm'
         WRITE(numout,*) '        nb_ana    = ', nb_ana
         CALL flush(numout)
      ENDIF
      !
      IF (nb_ana > jpmax_harmo) THEN
        IF(lwp) WRITE(numout,*) ' E R R O R dia_harm_init : nb_ana must be lower than jpmax_harmo, stop'
        IF(lwp) WRITE(numout,*) ' jpmax_harmo= ', jpmax_harmo
        nstop = nstop + 1
      ENDIF

      ALLOCATE(name    (nb_ana))
      DO jk=1,nb_ana
       DO ji=1,jpmax_harmo
          IF (TRIM(tname(jk)) .eq. Wave(ji)%cname_tide) THEN
             name(jk) = ji
             EXIT
          END IF
       END DO
      END DO

      ! Initialize frequency array:
      ! ---------------------------
      ALLOCATE(ana_freq(nb_ana))
      ALLOCATE(vt      (nb_ana))
      ALLOCATE(ut      (nb_ana))
      ALLOCATE(ft      (nb_ana))

      CALL tide_harmo(ana_freq, vt, ut , ft, name ,nb_ana)

      IF(lwp) WRITE(numout,*) 'Analysed frequency  : ',nb_ana ,'Frequency '

      DO jh = 1, nb_ana
        IF(lwp) WRITE(numout,*) '                    : ',tname(jh),' ',ana_freq(jh)
      END DO

      ! Initialize temporary arrays:
      ! ----------------------------
      ALLOCATE( ana_temp(jpi,jpj,nb_ana*2,3))
      ana_temp(:,:,:,:) = 0.e0

   END SUBROUTINE dia_harm_init
   
   SUBROUTINE dia_harm ( kt )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE dia_harm  ***
      !!----------------------------------------------------------------------
      !!         
      !! ** Purpose :   Tidal harmonic analysis main routine
      !!
      !! ** Action  :   Sums ssh/u/v over time analysis [nit000_han,nitend_han]
      !!
      !! History :
      !!   9.0  O. Le Galloudec and J. Chanut (Original)
      !!--------------------------------------------------------------------
      !! * Argument:
      INTEGER, INTENT( IN ) :: kt

      !! * Local declarations
      INTEGER  :: ji, jj, jh, jc, nhc
      REAL(wp) :: ztime, ztemp
      !!--------------------------------------------------------------------
      IF( nn_timing == 1 )   CALL timing_start('dia_harm')

      IF ( kt .EQ. nit000 ) CALL dia_harm_init

      IF ( ((kt.GE.nit000_han).AND.(kt.LE.nitend_han)).AND. &
           (MOD(kt,nstep_han).EQ.0) ) THEN

        ztime = (kt-nit000+1)*rdt 
       
        nhc = 0
        DO jh = 1,nb_ana
          DO jc = 1,2
            nhc = nhc+1
            ztemp =(     MOD(jc,2) * ft(jh) *COS(ana_freq(jh)*ztime + vt(jh) + ut(jh))  &
                    +(1.-MOD(jc,2))* ft(jh) *SIN(ana_freq(jh)*ztime + vt(jh) + ut(jh)))

            DO jj = 1,jpj
              DO ji = 1,jpi
                ! Elevation
                ana_temp(ji,jj,nhc,1) = ana_temp(ji,jj,nhc,1)                &
                                        + ztemp*sshn(ji,jj)*tmask(ji,jj,1)        
#if defined key_dynspg_ts
                ! ubar
                ana_temp(ji,jj,nhc,2) = ana_temp(ji,jj,nhc,2)                &
                                        + ztemp*un_b(ji,jj)*hur(ji,jj)*umask(ji,jj,1)
                ! vbar
                ana_temp(ji,jj,nhc,3) = ana_temp(ji,jj,nhc,3)                &
                                        + ztemp*vn_b(ji,jj)*hvr(ji,jj)*vmask(ji,jj,1)
#endif
              END DO
            END DO

          END DO
        END DO
       
      END IF

      IF ( kt .EQ. nitend_han ) CALL dia_harm_end

      IF( nn_timing == 1 )   CALL timing_stop('dia_harm')
 
   END SUBROUTINE dia_harm

   SUBROUTINE dia_harm_end
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE diaharm_end  ***
      !!----------------------------------------------------------------------
      !!         
      !! ** Purpose :  Compute the Real and Imaginary part of tidal constituents
      !!
      !! ** Action  :  Decompose the signal on the harmonic constituents 
      !!
      !! History :
      !!   9.0  O. Le Galloudec and J. Chanut (Original)
      !!--------------------------------------------------------------------

      !! * Local declarations
      INTEGER :: ji, jj, jh, jc, jn, nhan, jl
      INTEGER :: ksp, kun, keq
      REAL(wp) :: ztime, ztime_ini, ztime_end
      REAL(wp) :: X1,X2
      REAL(wp), POINTER, DIMENSION(:,:,:,:) :: ana_amp
      !!--------------------------------------------------------------------
      CALL wrk_alloc( jpi , jpj , jpmax_harmo , 2 , ana_amp )

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'anharmo_end: kt=nitend_han: Perform harmonic analysis'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'

      ztime_ini = nit000_han*rdt                 ! Initial time in seconds at the beginning of analysis
      ztime_end = nitend_han*rdt                 ! Final time in seconds at the end of analysis
      nhan = (nitend_han-nit000_han+1)/nstep_han ! Number of dumps used for analysis

      ninco = 2*nb_ana

      ksp = 0
      keq = 0        
      DO jn = 1, nhan
         ztime=( (nhan-jn)*ztime_ini + (jn-1)*ztime_end )/FLOAT(nhan-1)
         keq = keq + 1
         kun = 0
         DO jh = 1,nb_ana
            DO jc = 1,2
               kun = kun + 1
               ksp = ksp + 1
               nisparse(ksp) = keq
               njsparse(ksp) = kun
               valuesparse(ksp)= &
                   +(   MOD(jc,2) * ft(jh) * COS(ana_freq(jh)*ztime + vt(jh) + ut(jh)) &
                   +(1.-MOD(jc,2))* ft(jh) * SIN(ana_freq(jh)*ztime + vt(jh) + ut(jh)))
            END DO
         END DO
      END DO

      nsparse=ksp

      ! Elevation:
      DO jj = 1, jpj
         DO ji = 1, jpi
            ! Fill input array
            kun=0
            DO jh = 1,nb_ana
               DO jc = 1,2
                  kun = kun + 1
                  ztmp4(kun)=ana_temp(ji,jj,kun,1)
               ENDDO
            ENDDO

            CALL SUR_DETERMINE(jj)

            ! Fill output array
            DO jh = 1, nb_ana
               ana_amp(ji,jj,jh,1)=ztmp7((jh-1)*2+1)
               ana_amp(ji,jj,jh,2)=ztmp7((jh-1)*2+2)
            END DO
         END DO
      END DO

      ALLOCATE(out_eta(jpi,jpj,2*nb_ana))
      ALLOCATE(out_u  (jpi,jpj,2*nb_ana))
      ALLOCATE(out_v  (jpi,jpj,2*nb_ana))

      DO jj = 1, jpj
         DO ji = 1, jpi
            DO jh = 1, nb_ana 
               X1=ana_amp(ji,jj,jh,1)
               X2=-ana_amp(ji,jj,jh,2)
               out_eta(ji,jj,jh)=X1 * tmask(ji,jj,1)
               out_eta(ji,jj,nb_ana+jh)=X2 * tmask(ji,jj,1)
            ENDDO
         ENDDO
      ENDDO

      ! ubar:
      DO jj = 1, jpj
         DO ji = 1, jpi
            ! Fill input array
            kun=0
            DO jh = 1,nb_ana
               DO jc = 1,2
                  kun = kun + 1
                  ztmp4(kun)=ana_temp(ji,jj,kun,2)
               ENDDO
            ENDDO

            CALL SUR_DETERMINE(jj+1)

            ! Fill output array
            DO jh = 1, nb_ana
               ana_amp(ji,jj,jh,1)=ztmp7((jh-1)*2+1)
               ana_amp(ji,jj,jh,2)=ztmp7((jh-1)*2+2)
            END DO

         END DO
      END DO

      DO jj = 1, jpj
         DO ji = 1, jpi
            DO jh = 1, nb_ana 
               X1=ana_amp(ji,jj,jh,1)
               X2=-ana_amp(ji,jj,jh,2)
               out_u(ji,jj,jh) = X1 * umask(ji,jj,1)
               out_u (ji,jj,nb_ana+jh) = X2 * umask(ji,jj,1)
            ENDDO
         ENDDO
      ENDDO

      ! vbar:
      DO jj = 1, jpj
         DO ji = 1, jpi
            ! Fill input array
            kun=0
            DO jh = 1,nb_ana
               DO jc = 1,2
                  kun = kun + 1
                  ztmp4(kun)=ana_temp(ji,jj,kun,3)
               ENDDO
            ENDDO

            CALL SUR_DETERMINE(jj+1)

            ! Fill output array
            DO jh = 1, nb_ana
               ana_amp(ji,jj,jh,1)=ztmp7((jh-1)*2+1)
               ana_amp(ji,jj,jh,2)=ztmp7((jh-1)*2+2)
            END DO

         END DO
      END DO

      DO jj = 1, jpj
         DO ji = 1, jpi
            DO jh = 1, nb_ana 
               X1=ana_amp(ji,jj,jh,1)
               X2=-ana_amp(ji,jj,jh,2)
               out_v(ji,jj,jh)=X1 * vmask(ji,jj,1)
               out_v(ji,jj,nb_ana+jh)=X2 * vmask(ji,jj,1)
            ENDDO
         ENDDO
      ENDDO

      CALL dia_wri_harm ! Write results in files
      CALL wrk_dealloc( jpi , jpj , jpmax_harmo , 2 , ana_amp )
      !
   END SUBROUTINE dia_harm_end

   SUBROUTINE dia_wri_harm
      !!--------------------------------------------------------------------
      !!                 ***  ROUTINE dia_wri_harm  ***
      !!--------------------------------------------------------------------
      !!         
      !! ** Purpose : Write tidal harmonic analysis results in a netcdf file
      !!
      !!
      !! History :
      !!   9.0  O. Le Galloudec and J. Chanut (Original)
      !!--------------------------------------------------------------------

      !! * Local declarations
      CHARACTER(LEN=lc) :: cltext
      CHARACTER(LEN=lc) ::   &
         cdfile_name_T   ,   & ! name of the file created (T-points)
         cdfile_name_U   ,   & ! name of the file created (U-points)
         cdfile_name_V         ! name of the file created (V-points)
      INTEGER  ::   jh
      !!----------------------------------------------------------------------

#if defined key_dimgout
      cdfile_name_T = TRIM(cexper)//'_Tidal_harmonics_gridT.dimgproc'
      cdfile_name_U = TRIM(cexper)//'_Tidal_harmonics_gridU.dimgproc'
      cdfile_name_V = TRIM(cexper)//'_Tidal_harmonics_gridV.dimgproc'
#endif

      IF(lwp) WRITE(numout,*) '  '
      IF(lwp) WRITE(numout,*) 'dia_wri_harm : Write harmonic analysis results'
#if defined key_dimgout
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~  Output files: ', TRIM(cdfile_name_T)
      IF(lwp) WRITE(numout,*) '                             ', TRIM(cdfile_name_U)
      IF(lwp) WRITE(numout,*) '                             ', TRIM(cdfile_name_V)
#endif
      IF(lwp) WRITE(numout,*) '  '

      ! A) Elevation
      !/////////////
      !
#if defined key_dimgout
      cltext='Elevation amplitude and phase'
      CALL dia_wri_dimg(TRIM(cdfile_name_T), TRIM(cltext), out_eta, 2*nb_ana, '2')
#else
      DO jh = 1, nb_ana
      CALL iom_put( TRIM(tname(jh))//'x', out_eta(:,:,jh) )
      CALL iom_put( TRIM(tname(jh))//'y', out_eta(:,:,nb_ana+jh) )
      END DO
#endif

      ! B) ubar
      !/////////
      !
#if defined key_dimgout
      cltext='ubar amplitude and phase'
      CALL dia_wri_dimg(TRIM(cdfile_name_U), TRIM(cltext), out_u, 2*nb_ana, '2')
#else
      DO jh = 1, nb_ana
      CALL iom_put( TRIM(tname(jh))//'x_u', out_u(:,:,jh) )
      CALL iom_put( TRIM(tname(jh))//'y_u', out_u(:,:,nb_ana+jh) )
      END DO
#endif

      ! C) vbar
      !/////////
      !
#if defined key_dimgout
      cltext='vbar amplitude and phase'
      CALL dia_wri_dimg(TRIM(cdfile_name_V), TRIM(cltext), out_v, 2*nb_ana, '2')
#else
      DO jh = 1, nb_ana
      CALL iom_put( TRIM(tname(jh))//'x_v', out_u(:,:,jh) )
      CALL iom_put( TRIM(tname(jh))//'y_v', out_u(:,:,nb_ana+jh) )
      END DO
#endif

   END SUBROUTINE dia_wri_harm

   SUBROUTINE SUR_DETERMINE(init)
   !!---------------------------------------------------------------------------------
   !!                      *** ROUTINE SUR_DETERMINE ***
   !!    
   !!    
   !!       
   !!---------------------------------------------------------------------------------
   INTEGER, INTENT(in) :: init 
               
   INTEGER                         :: ji_sd, jj_sd, ji1_sd, ji2_sd, jk1_sd, jk2_sd
   REAL(wp)                        :: zval1, zval2, zx1
   REAL(wp), POINTER, DIMENSION(:) :: ztmpx, zcol1, zcol2
   INTEGER , POINTER, DIMENSION(:) :: ipos2, ipivot
   !---------------------------------------------------------------------------------
   CALL wrk_alloc( jpincomax , ztmpx , zcol1 , zcol2 )
   CALL wrk_alloc( jpincomax , ipos2 , ipivot        )
            
   IF( init==1 )THEN

      IF( nsparse .GT. jpdimsparse ) &
         CALL ctl_stop( 'STOP', 'SUR_DETERMINE : nsparse .GT. jpdimsparse')

      IF( ninco .GT. jpincomax ) &
         CALL ctl_stop( 'STOP', 'SUR_DETERMINE : ninco .GT. jpincomax')

      ztmp3(:,:)=0.e0

      DO jk1_sd = 1, nsparse
         DO jk2_sd = 1, nsparse

            nisparse(jk2_sd)=nisparse(jk2_sd)
            njsparse(jk2_sd)=njsparse(jk2_sd)

            IF( nisparse(jk2_sd) == nisparse(jk1_sd) ) THEN
               ztmp3(njsparse(jk1_sd),njsparse(jk2_sd)) = ztmp3(njsparse(jk1_sd),njsparse(jk2_sd))  &
                                                        + valuesparse(jk1_sd)*valuesparse(jk2_sd)
            ENDIF

         ENDDO
      ENDDO

      DO jj_sd = 1 ,ninco
          ipos1(jj_sd) = jj_sd
          ipos2(jj_sd) = jj_sd
      ENDDO

      DO ji_sd = 1 , ninco

         !find greatest non-zero pivot:
         zval1 = ABS(ztmp3(ji_sd,ji_sd))

         ipivot(ji_sd) = ji_sd
         DO jj_sd = ji_sd, ninco
            zval2 = ABS(ztmp3(ji_sd,jj_sd))
            IF( zval2.GE.zval1 )THEN
               ipivot(ji_sd) = jj_sd
               zval1         = zval2
            ENDIF
         ENDDO

         DO ji1_sd = 1, ninco
            zcol1(ji1_sd)               = ztmp3(ji1_sd,ji_sd)
            zcol2(ji1_sd)               = ztmp3(ji1_sd,ipivot(ji_sd))
            ztmp3(ji1_sd,ji_sd)         = zcol2(ji1_sd)
            ztmp3(ji1_sd,ipivot(ji_sd)) = zcol1(ji1_sd)
         ENDDO

         ipos2(ji_sd)         = ipos1(ipivot(ji_sd))
         ipos2(ipivot(ji_sd)) = ipos1(ji_sd)
         ipos1(ji_sd)         = ipos2(ji_sd)
         ipos1(ipivot(ji_sd)) = ipos2(ipivot(ji_sd))
         zpivot(ji_sd)        = ztmp3(ji_sd,ji_sd)
         DO jj_sd = 1, ninco
            ztmp3(ji_sd,jj_sd) = ztmp3(ji_sd,jj_sd) / zpivot(ji_sd)
         ENDDO

         DO ji2_sd = ji_sd+1, ninco
            zpilier(ji2_sd,ji_sd)=ztmp3(ji2_sd,ji_sd)
            DO jj_sd=1,ninco
               ztmp3(ji2_sd,jj_sd)=  ztmp3(ji2_sd,jj_sd) - ztmp3(ji_sd,jj_sd) * zpilier(ji2_sd,ji_sd)
            ENDDO
         ENDDO

      ENDDO

   ENDIF ! End init==1

   DO ji_sd = 1, ninco
      ztmp4(ji_sd) = ztmp4(ji_sd) / zpivot(ji_sd)
      DO ji2_sd = ji_sd+1, ninco
         ztmp4(ji2_sd) = ztmp4(ji2_sd) - ztmp4(ji_sd) * zpilier(ji2_sd,ji_sd)
      ENDDO
   ENDDO

   !system solving: 
   ztmpx(ninco) = ztmp4(ninco) / ztmp3(ninco,ninco)
   ji_sd = ninco
   DO ji_sd = ninco-1, 1, -1
      zx1=0.
      DO jj_sd = ji_sd+1, ninco
         zx1 = zx1 + ztmpx(jj_sd) * ztmp3(ji_sd,jj_sd)
      ENDDO
      ztmpx(ji_sd) = ztmp4(ji_sd)-zx1
   ENDDO

   DO jj_sd =1, ninco
      ztmp7(ipos1(jj_sd))=ztmpx(jj_sd)
   ENDDO


   CALL wrk_dealloc( jpincomax , ztmpx , zcol1 , zcol2 )
   CALL wrk_dealloc( jpincomax , ipos2 , ipivot        )

  END SUBROUTINE SUR_DETERMINE


#else
   !!----------------------------------------------------------------------
   !!   Default case :   Empty module
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_diaharm = .FALSE.
CONTAINS

   SUBROUTINE dia_harm ( kt )     ! Empty routine
      INTEGER, INTENT( IN ) :: kt  
      WRITE(*,*) 'dia_harm: you should not have seen this print'
   END SUBROUTINE dia_harm


#endif
   !!======================================================================
END MODULE diaharm
