MODULE obs_write
   !!======================================================================
   !!                       ***  MODULE obs_write   ***
   !! Observation diagnosticss: Write observation related diagnostics
   !!=====================================================================

   !!----------------------------------------------------------------------
   !!   cdf_wri_p3d   : Write profile observation diagnostics in NetCDF format
   !!   obs_wri_sla   : Write SLA observation related diagnostics
   !!   obs_wri_sst   : Write SST observation related diagnostics
   !!   obs_wri_seaice: Write seaice observation related diagnostics
   !!   cdf_wri_vel   : Write velocity observation diagnostics in NetCDF format
   !!----------------------------------------------------------------------

   !! * Modules used
   USE par_kind, ONLY : &   ! Precision variables
      & wp
   USE in_out_manager       ! I/O manager
   USE dom_oce              ! Ocean space and time domain variables
   USE obs_types            ! Observation type integer to character translation
   USE julian, ONLY : &         ! Julian date routines
      & greg2jul
   USE obs_utils, ONLY : &  ! Observation operator utility functions
      & chkerr
   USE obs_profiles_def     ! Type definitions for profiles
   USE obs_surf_def         ! Type defintions for surface observations
   USE obs_fbm              ! Observation feedback I/O
   USE obs_grid             ! Grid tools
   USE obs_conv             ! Conversion between units
   USE obs_const
   USE obs_sla_types
   USE obs_rot_vel          ! Rotation of velocities

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE
   PUBLIC obs_wri_p3d, &    ! Write profile observation related diagnostics
      &   obs_wri_sla, &    ! Write SLA observation related diagnostics
      &   obs_wri_sst, &    ! Write SST observation related diagnostics
      &   obs_wri_sss, &    ! Write SSS observation related diagnostics
      &   obs_wri_seaice, & ! Write seaice observation related diagnostics
      &   obs_wri_vel, &    ! Write velocity observation related diagnostics
      &   obswriinfo
   
   TYPE obswriinfo
      INTEGER :: inum
      INTEGER, POINTER, DIMENSION(:) :: ipoint
      CHARACTER(len=ilenname), POINTER, DIMENSION(:) :: cdname
      CHARACTER(len=ilenlong), POINTER, DIMENSION(:,:) :: cdlong
      CHARACTER(len=ilenunit), POINTER, DIMENSION(:,:) :: cdunit
   END TYPE obswriinfo

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_write.F90 2287 2010-10-18 07:53:52Z smasson $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obs_wri_p3d( cprefix, profdata, padd, pext )
      !!-----------------------------------------------------------------------
      !!
      !!                     *** ROUTINE obs_wri_p3d  ***
      !!
      !! ** Purpose : Write temperature and salinity (profile) observation 
      !!              related diagnostics
      !!
      !! ** Method  : NetCDF
      !! 
      !! ** Action  :
      !!
      !! History :
      !!      ! 06-04  (A. Vidard) Original
      !!      ! 06-04  (A. Vidard) Reformatted
      !!      ! 06-10  (A. Weaver) Cleanup
      !!      ! 07-01  (K. Mogensen) Use profile data types
      !!      ! 07-03  (K. Mogensen) General handling of profiles
      !!      ! 09-01  (K. Mogensen) New feedback format
      !!-----------------------------------------------------------------------

      !! * Modules used

      !! * Arguments
      CHARACTER(LEN=*), INTENT(IN) :: cprefix        ! Prefix for output files
      TYPE(obs_prof), INTENT(INOUT) :: profdata      ! Full set of profile data
      TYPE(obswriinfo), OPTIONAL :: padd             ! Additional info for each variable
      TYPE(obswriinfo), OPTIONAL :: pext             ! Extra info
      
      !! * Local declarations
      TYPE(obfbdata) :: fbdata
      CHARACTER(LEN=40) :: cfname
      INTEGER :: ilevel
      INTEGER :: jvar
      INTEGER :: jo
      INTEGER :: jk
      INTEGER :: ik
      INTEGER :: ja
      INTEGER :: je
      REAL(wp) :: zpres
      INTEGER :: nadd
      INTEGER :: next

      IF ( PRESENT( padd ) ) THEN
         nadd = padd%inum
      ELSE
         nadd = 0
      ENDIF

      IF ( PRESENT( pext ) ) THEN
         next = pext%inum
      ELSE
         next = 0
      ENDIF
      
      CALL init_obfbdata( fbdata )

      ! Find maximum level
      ilevel = 0
      DO jvar = 1, 2
         ilevel = MAX( ilevel, MAXVAL( profdata%var(jvar)%nvlidx(:) ) )
      END DO
      CALL alloc_obfbdata( fbdata, 2, profdata%nprof, ilevel, &
         &                 1 + nadd, 1 + next, .TRUE. )

      fbdata%cname(1)      = 'POTM'
      fbdata%cname(2)      = 'PSAL'
      fbdata%coblong(1)    = 'Potential temperature'
      fbdata%coblong(2)    = 'Practical salinity'
      fbdata%cobunit(1)    = 'Degrees centigrade'
      fbdata%cobunit(2)    = 'PSU'
      fbdata%cextname(1)   = 'TEMP'
      fbdata%cextlong(1)   = 'Insitu temperature'
      fbdata%cextunit(1)   = 'Degrees centigrade'
      DO je = 1, next
         fbdata%cextname(1+je) = pext%cdname(je)
         fbdata%cextlong(1+je) = pext%cdlong(je,1)
         fbdata%cextunit(1+je) = pext%cdunit(je,1)
      END DO
      fbdata%caddname(1)   = 'Hx'
      fbdata%caddlong(1,1) = 'Model interpolated potential temperature'
      fbdata%caddlong(1,2) = 'Model interpolated practical salinity'
      fbdata%caddunit(1,1) = 'Degrees centigrade'
      fbdata%caddunit(1,2) = 'PSU'
      fbdata%cgrid(:)      = 'T'
      DO ja = 1, nadd
         fbdata%caddname(1+ja) = padd%cdname(ja)
         DO jvar = 1, 2
            fbdata%caddlong(1+ja,jvar) = padd%cdlong(ja,jvar)
            fbdata%caddunit(1+ja,jvar) = padd%cdunit(ja,jvar)
         END DO
      END DO
         
      WRITE(cfname, FMT="(A,'_fdbk_',I4.4,'.nc')") TRIM(cprefix), nproc

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*)'obs_wri_p3d :'
         WRITE(numout,*)'~~~~~~~~~~~~~'
         WRITE(numout,*)'Writing profile feedback file : ',TRIM(cfname)
      ENDIF

      ! Transform obs_prof data structure into obfbdata structure
      fbdata%cdjuldref = '19500101000000'
      DO jo = 1, profdata%nprof
         fbdata%plam(jo)      = profdata%rlam(jo)
         fbdata%pphi(jo)      = profdata%rphi(jo)
         WRITE(fbdata%cdtyp(jo),'(I4)') profdata%ntyp(jo)
         fbdata%ivqc(jo,:)    = profdata%ivqc(jo,:)
         fbdata%ivqcf(:,jo,:) = profdata%ivqcf(:,jo,:)
         IF ( profdata%nqc(jo) > 10 ) THEN
            fbdata%ioqc(jo)    = 4
            fbdata%ioqcf(1,jo) = profdata%nqcf(1,jo)
            fbdata%ioqcf(2,jo) = profdata%nqc(jo) - 10
         ELSE
            fbdata%ioqc(jo)    = profdata%nqc(jo)
            fbdata%ioqcf(:,jo) = profdata%nqcf(:,jo)
         ENDIF
         fbdata%ipqc(jo)      = profdata%ipqc(jo)
         fbdata%ipqcf(:,jo)   = profdata%ipqcf(:,jo)
         fbdata%itqc(jo)      = profdata%itqc(jo)
         fbdata%itqcf(:,jo)   = profdata%itqcf(:,jo)
         fbdata%cdwmo(jo)     = profdata%cwmo(jo)
         fbdata%kindex(jo)    = profdata%npfil(jo)
         DO jvar = 1, profdata%nvar
            IF (ln_grid_global) THEN
               fbdata%iobsi(jo,jvar) = profdata%mi(jo,jvar)
               fbdata%iobsj(jo,jvar) = profdata%mj(jo,jvar)
            ELSE
               fbdata%iobsi(jo,jvar) = mig(profdata%mi(jo,jvar))
               fbdata%iobsj(jo,jvar) = mjg(profdata%mj(jo,jvar))
            ENDIF
         END DO
         CALL greg2jul( 0, &
            &           profdata%nmin(jo), &
            &           profdata%nhou(jo), &
            &           profdata%nday(jo), &
            &           profdata%nmon(jo), &
            &           profdata%nyea(jo), &
            &           fbdata%ptim(jo),   &
            &           krefdate = 19500101 )
         ! Reform the profiles arrays for output
         DO jvar = 1, 2
            DO jk = profdata%npvsta(jo,jvar), profdata%npvend(jo,jvar)
               ik = profdata%var(jvar)%nvlidx(jk)
               fbdata%padd(ik,jo,1,jvar) = profdata%var(jvar)%vmod(jk)
               fbdata%pob(ik,jo,jvar)    = profdata%var(jvar)%vobs(jk)
               fbdata%pdep(ik,jo)        = profdata%var(jvar)%vdep(jk)
               fbdata%idqc(ik,jo)        = profdata%var(jvar)%idqc(jk)
               fbdata%idqcf(:,ik,jo)     = profdata%var(jvar)%idqcf(:,jk)
               IF ( profdata%var(jvar)%nvqc(jk) > 10 ) THEN
                  fbdata%ivlqc(ik,jo,jvar) = 4
                  fbdata%ivlqcf(1,ik,jo,jvar) = profdata%var(jvar)%nvqcf(1,jk)
                  fbdata%ivlqcf(2,ik,jo,jvar) = profdata%var(jvar)%nvqc(jk) - 10
               ELSE
                  fbdata%ivlqc(ik,jo,jvar) = profdata%var(jvar)%nvqc(jk)
                  fbdata%ivlqcf(:,ik,jo,jvar) = profdata%var(jvar)%nvqcf(:,jk)
               ENDIF
               fbdata%iobsk(ik,jo,jvar)  = profdata%var(jvar)%mvk(jk)
               DO ja = 1, nadd
                  fbdata%padd(ik,jo,1+ja,jvar) = &
                     & profdata%var(jvar)%vext(jk,padd%ipoint(ja))
               END DO
               DO je = 1, next
                  fbdata%pext(ik,jo,1+je) = &
                     & profdata%var(jvar)%vext(jk,pext%ipoint(je))
               END DO
               IF ( jvar == 1 ) THEN
                  fbdata%pext(ik,jo,1) = profdata%var(jvar)%vext(jk,1)
               ENDIF 
            END DO
         END DO
      END DO

      ! Convert insitu temperature to potential temperature using the model
      ! salinity if no potential temperature
      DO jo = 1, fbdata%nobs
         IF ( fbdata%pphi(jo) < 9999.0 ) THEN
            DO jk = 1, fbdata%nlev
               IF ( ( fbdata%pob(jk,jo,1) >= 9999.0 ) .AND. &
                  & ( fbdata%pdep(jk,jo) < 9999.0 ) .AND. &
                  & ( fbdata%padd(jk,jo,1,2) < 9999.0 ) .AND. &
                  & ( fbdata%pext(jk,jo,1) < 9999.0 ) ) THEN
                  zpres = dep_to_p( REAL(fbdata%pdep(jk,jo),wp), &
                     &              REAL(fbdata%pphi(jo),wp) )
                  fbdata%pob(jk,jo,1) = potemp( &
                     &                     REAL(fbdata%padd(jk,jo,1,2), wp),  &
                     &                     REAL(fbdata%pext(jk,jo,1), wp), &
                     &                     zpres, 0.0_wp )
               ENDIF
            END DO
         ENDIF
      END DO
      
      ! Write the obfbdata structure
      CALL write_obfbdata( cfname, fbdata )
      
      CALL dealloc_obfbdata( fbdata )
     
   END SUBROUTINE obs_wri_p3d

   SUBROUTINE obs_wri_sla( cprefix, sladata, padd, pext )
      !!-----------------------------------------------------------------------
      !!
      !!                     *** ROUTINE obs_wri_sla  ***
      !!
      !! ** Purpose : Write SLA observation diagnostics
      !!              related 
      !!
      !! ** Method  : NetCDF
      !! 
      !! ** Action  :
      !!
      !!      ! 07-03  (K. Mogensen) Original
      !!      ! 09-01  (K. Mogensen) New feedback format.
      !!-----------------------------------------------------------------------

      !! * Modules used
      IMPLICIT NONE

      !! * Arguments
      CHARACTER(LEN=*), INTENT(IN) :: cprefix          ! Prefix for output files
      TYPE(obs_surf), INTENT(INOUT) :: sladata         ! Full set of SLAa
      TYPE(obswriinfo), OPTIONAL :: padd               ! Additional info for each variable
      TYPE(obswriinfo), OPTIONAL :: pext               ! Extra info

      !! * Local declarations
      TYPE(obfbdata) :: fbdata
      CHARACTER(LEN=40) :: cfname         ! netCDF filename
      CHARACTER(LEN=12), PARAMETER :: cpname = 'obs_wri_sla'
      INTEGER :: jo
      INTEGER :: ja
      INTEGER :: je
      INTEGER :: nadd
      INTEGER :: next

      IF ( PRESENT( padd ) ) THEN
         nadd = padd%inum
      ELSE
         nadd = 0
      ENDIF

      IF ( PRESENT( pext ) ) THEN
         next = pext%inum
      ELSE
         next = 0
      ENDIF

      CALL init_obfbdata( fbdata )

      CALL alloc_obfbdata( fbdata, 1, sladata%nsurf, 1, &
         &                 2 + nadd, 1 + next, .TRUE. )

      fbdata%cname(1)      = 'SLA'
      fbdata%coblong(1)    = 'Sea level anomaly'
      fbdata%cobunit(1)    = 'Metres'
      fbdata%cextname(1)   = 'MDT'
      fbdata%cextlong(1)   = 'Mean dynamic topography'
      fbdata%cextunit(1)   = 'Metres'
      DO je = 1, next
         fbdata%cextname(1+je) = pext%cdname(je)
         fbdata%cextlong(1+je) = pext%cdlong(je,1)
         fbdata%cextunit(1+je) = pext%cdunit(je,1)
      END DO
      fbdata%caddname(1)   = 'Hx'
      fbdata%caddlong(1,1) = 'Model interpolated SSH - MDT'
      fbdata%caddunit(1,1) = 'Metres' 
      fbdata%caddname(2)   = 'SSH'
      fbdata%caddlong(2,1) = 'Model Sea surface height'
      fbdata%caddunit(2,1) = 'Metres'
      fbdata%cgrid(1)      = 'T'
      DO ja = 1, nadd
         fbdata%caddname(2+ja) = padd%cdname(ja)
         fbdata%caddlong(2+ja,1) = padd%cdlong(ja,1)
         fbdata%caddunit(2+ja,1) = padd%cdunit(ja,1)
      END DO

      WRITE(cfname, FMT="(A,'_fdbk_',I4.4,'.nc')") TRIM(cprefix), nproc

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*)'obs_wri_sla :'
         WRITE(numout,*)'~~~~~~~~~~~~~'
         WRITE(numout,*)'Writing SLA feedback file : ',TRIM(cfname)
      ENDIF

      ! Transform obs_prof data structure into obfbdata structure
      fbdata%cdjuldref = '19500101000000'
      DO jo = 1, sladata%nsurf
         fbdata%plam(jo)      = sladata%rlam(jo)
         fbdata%pphi(jo)      = sladata%rphi(jo)
         WRITE(fbdata%cdtyp(jo),'(I4)') sladata%ntyp(jo)
         fbdata%ivqc(jo,:)    = 0
         fbdata%ivqcf(:,jo,:) = 0
         IF ( sladata%nqc(jo) > 10 ) THEN
            fbdata%ioqc(jo)    = 4
            fbdata%ioqcf(1,jo) = 0
            fbdata%ioqcf(2,jo) = sladata%nqc(jo) - 10
         ELSE
            fbdata%ioqc(jo)    = sladata%nqc(jo)
            fbdata%ioqcf(:,jo) = 0
         ENDIF
         fbdata%ipqc(jo)      = 0
         fbdata%ipqcf(:,jo)   = 0
         fbdata%itqc(jo)      = 0
         fbdata%itqcf(:,jo)   = 0
         fbdata%cdwmo(jo)     = sladata%cwmo(jo)
         fbdata%kindex(jo)    = sladata%nsfil(jo)
         IF (ln_grid_global) THEN
            fbdata%iobsi(jo,1) = sladata%mi(jo)
            fbdata%iobsj(jo,1) = sladata%mj(jo)
         ELSE
            fbdata%iobsi(jo,1) = mig(sladata%mi(jo))
            fbdata%iobsj(jo,1) = mjg(sladata%mj(jo))
         ENDIF
         CALL greg2jul( 0, &
            &           sladata%nmin(jo), &
            &           sladata%nhou(jo), &
            &           sladata%nday(jo), &
            &           sladata%nmon(jo), &
            &           sladata%nyea(jo), &
            &           fbdata%ptim(jo),   &
            &           krefdate = 19500101 )
         fbdata%padd(1,jo,1,1) = sladata%rmod(jo,1)
         fbdata%padd(1,jo,2,1) = sladata%rext(jo,1)
         fbdata%pob(1,jo,1)    = sladata%robs(jo,1) 
         fbdata%pdep(1,jo)     = 0.0
         fbdata%idqc(1,jo)     = 0
         fbdata%idqcf(:,1,jo)  = 0
         IF ( sladata%nqc(jo) > 10 ) THEN
            fbdata%ivqc(jo,1)       = 4
            fbdata%ivlqc(1,jo,1)    = 4
            fbdata%ivlqcf(1,1,jo,1) = 0
            fbdata%ivlqcf(2,1,jo,1) = sladata%nqc(jo) - 10
         ELSE
            fbdata%ivqc(jo,1)       = sladata%nqc(jo)
            fbdata%ivlqc(1,jo,1)    = sladata%nqc(jo)
            fbdata%ivlqcf(:,1,jo,1) = 0
         ENDIF
         fbdata%iobsk(1,jo,1)  = 0
         fbdata%pext(1,jo,1) = sladata%rext(jo,2)
         DO ja = 1, nadd
            fbdata%padd(1,jo,2+ja,1) = &
               & sladata%rext(jo,padd%ipoint(ja))
         END DO
         DO je = 1, next
            fbdata%pext(1,jo,1+je) = &
               & sladata%rext(jo,pext%ipoint(je))
         END DO
      END DO

      ! Write the obfbdata structure
      CALL write_obfbdata( cfname, fbdata )

      CALL dealloc_obfbdata( fbdata )

   END SUBROUTINE obs_wri_sla

   SUBROUTINE obs_wri_sst( cprefix, sstdata, padd, pext )
      !!-----------------------------------------------------------------------
      !!
      !!                     *** ROUTINE obs_wri_sst  ***
      !!
      !! ** Purpose : Write SST observation diagnostics
      !!              related 
      !!
      !! ** Method  : NetCDF
      !! 
      !! ** Action  :
      !!
      !!      ! 07-07  (S. Ricci) Original
      !!      ! 09-01  (K. Mogensen) New feedback format.
      !!-----------------------------------------------------------------------

      !! * Modules used
      IMPLICIT NONE

      !! * Arguments
      CHARACTER(LEN=*), INTENT(IN) :: cprefix       ! Prefix for output files
      TYPE(obs_surf), INTENT(INOUT) :: sstdata      ! Full set of SST
      TYPE(obswriinfo), OPTIONAL :: padd            ! Additional info for each variable
      TYPE(obswriinfo), OPTIONAL :: pext            ! Extra info

      !! * Local declarations 
      TYPE(obfbdata) :: fbdata
      CHARACTER(LEN=40) ::  cfname             ! netCDF filename
      CHARACTER(LEN=12), PARAMETER :: cpname = 'obs_wri_sst'
      INTEGER :: jo
      INTEGER :: ja
      INTEGER :: je
      INTEGER :: nadd
      INTEGER :: next

      IF ( PRESENT( padd ) ) THEN
         nadd = padd%inum
      ELSE
         nadd = 0
      ENDIF

      IF ( PRESENT( pext ) ) THEN
         next = pext%inum
      ELSE
         next = 0
      ENDIF

      CALL init_obfbdata( fbdata )

      CALL alloc_obfbdata( fbdata, 1, sstdata%nsurf, 1, &
         &                 1 + nadd, next, .TRUE. )

      fbdata%cname(1)      = 'SST'
      fbdata%coblong(1)    = 'Sea surface temperature'
      fbdata%cobunit(1)    = 'Degree centigrade'
      DO je = 1, next
         fbdata%cextname(je) = pext%cdname(je)
         fbdata%cextlong(je) = pext%cdlong(je,1)
         fbdata%cextunit(je) = pext%cdunit(je,1)
      END DO
      fbdata%caddname(1)   = 'Hx'
      fbdata%caddlong(1,1) = 'Model interpolated SST'
      fbdata%caddunit(1,1) = 'Degree centigrade'
      fbdata%cgrid(1)      = 'T'
      DO ja = 1, nadd
         fbdata%caddname(1+ja) = padd%cdname(ja)
         fbdata%caddlong(1+ja,1) = padd%cdlong(ja,1)
         fbdata%caddunit(1+ja,1) = padd%cdunit(ja,1)
      END DO

      WRITE(cfname, FMT="(A,'_fdbk_',I4.4,'.nc')") TRIM(cprefix), nproc

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*)'obs_wri_sst :'
         WRITE(numout,*)'~~~~~~~~~~~~~'
         WRITE(numout,*)'Writing SST feedback file : ',TRIM(cfname)
      ENDIF

      ! Transform obs_prof data structure into obfbdata structure
      fbdata%cdjuldref = '19500101000000'
      DO jo = 1, sstdata%nsurf
         fbdata%plam(jo)      = sstdata%rlam(jo)
         fbdata%pphi(jo)      = sstdata%rphi(jo)
         WRITE(fbdata%cdtyp(jo),'(I4)') sstdata%ntyp(jo)
         fbdata%ivqc(jo,:)    = 0
         fbdata%ivqcf(:,jo,:) = 0
         IF ( sstdata%nqc(jo) > 10 ) THEN
            fbdata%ioqc(jo)    = 4
            fbdata%ioqcf(1,jo) = 0
            fbdata%ioqcf(2,jo) = sstdata%nqc(jo) - 10
         ELSE
            fbdata%ioqc(jo)    = MAX(sstdata%nqc(jo),1)
            fbdata%ioqcf(:,jo) = 0
         ENDIF
         fbdata%ipqc(jo)      = 0
         fbdata%ipqcf(:,jo)   = 0
         fbdata%itqc(jo)      = 0
         fbdata%itqcf(:,jo)   = 0
         fbdata%cdwmo(jo)     = ''
         fbdata%kindex(jo)    = sstdata%nsfil(jo)
         IF (ln_grid_global) THEN
            fbdata%iobsi(jo,1) = sstdata%mi(jo)
            fbdata%iobsj(jo,1) = sstdata%mj(jo)
         ELSE
            fbdata%iobsi(jo,1) = mig(sstdata%mi(jo))
            fbdata%iobsj(jo,1) = mjg(sstdata%mj(jo))
         ENDIF
         CALL greg2jul( 0, &
            &           sstdata%nmin(jo), &
            &           sstdata%nhou(jo), &
            &           sstdata%nday(jo), &
            &           sstdata%nmon(jo), &
            &           sstdata%nyea(jo), &
            &           fbdata%ptim(jo),   &
            &           krefdate = 19500101 )
         fbdata%padd(1,jo,1,1) = sstdata%rmod(jo,1)
         fbdata%pob(1,jo,1)    = sstdata%robs(jo,1)
         fbdata%pdep(1,jo)     = 0.0
         fbdata%idqc(1,jo)     = 0
         fbdata%idqcf(:,1,jo)  = 0
         IF ( sstdata%nqc(jo) > 10 ) THEN
            fbdata%ivqc(jo,1)       = 4
            fbdata%ivlqc(1,jo,1)    = 4
            fbdata%ivlqcf(1,1,jo,1) = 0
            fbdata%ivlqcf(2,1,jo,1) = sstdata%nqc(jo) - 10
         ELSE
            fbdata%ivqc(jo,1)       = MAX(sstdata%nqc(jo),1)
            fbdata%ivlqc(1,jo,1)    = MAX(sstdata%nqc(jo),1)
            fbdata%ivlqcf(:,1,jo,1) = 0
         ENDIF
         fbdata%iobsk(1,jo,1)  = 0
         DO ja = 1, nadd
            fbdata%padd(1,jo,1+ja,1) = &
               & sstdata%rext(jo,padd%ipoint(ja))
         END DO
         DO je = 1, next
            fbdata%pext(1,jo,je) = &
               & sstdata%rext(jo,pext%ipoint(je))
         END DO

      END DO

      ! Write the obfbdata structure

      CALL write_obfbdata( cfname, fbdata )

      CALL dealloc_obfbdata( fbdata )

   END SUBROUTINE obs_wri_sst

   SUBROUTINE obs_wri_sss
   END SUBROUTINE obs_wri_sss

   SUBROUTINE obs_wri_seaice( cprefix, seaicedata, padd, pext )
      !!-----------------------------------------------------------------------
      !!
      !!                     *** ROUTINE obs_wri_seaice  ***
      !!
      !! ** Purpose : Write sea ice observation diagnostics
      !!              related 
      !!
      !! ** Method  : NetCDF
      !! 
      !! ** Action  :
      !!
      !!      ! 07-07  (S. Ricci) Original
      !!      ! 09-01  (K. Mogensen) New feedback format.
      !!-----------------------------------------------------------------------

      !! * Modules used
      IMPLICIT NONE

      !! * Arguments
      CHARACTER(LEN=*), INTENT(IN) :: cprefix       ! Prefix for output files
      TYPE(obs_surf), INTENT(INOUT) :: seaicedata   ! Full set of sea ice
      TYPE(obswriinfo), OPTIONAL :: padd            ! Additional info for each variable
      TYPE(obswriinfo), OPTIONAL :: pext            ! Extra info

      !! * Local declarations 
      TYPE(obfbdata) :: fbdata
      CHARACTER(LEN=40) :: cfname             ! netCDF filename
      CHARACTER(LEN=12), PARAMETER :: cpname = 'obs_wri_seaice'
      INTEGER :: jo
      INTEGER :: ja
      INTEGER :: je
      INTEGER :: nadd
      INTEGER :: next

      IF ( PRESENT( padd ) ) THEN
         nadd = padd%inum
      ELSE
         nadd = 0
      ENDIF

      IF ( PRESENT( pext ) ) THEN
         next = pext%inum
      ELSE
         next = 0
      ENDIF

      CALL init_obfbdata( fbdata )

      CALL alloc_obfbdata( fbdata, 1, seaicedata%nsurf, 1, 1, 0, .TRUE. )

      fbdata%cname(1)      = 'SEAICE'
      fbdata%coblong(1)    = 'Sea ice'
      fbdata%cobunit(1)    = 'Fraction'
      DO je = 1, next
         fbdata%cextname(je) = pext%cdname(je)
         fbdata%cextlong(je) = pext%cdlong(je,1)
         fbdata%cextunit(je) = pext%cdunit(je,1)
      END DO
      fbdata%caddname(1)   = 'Hx'
      fbdata%caddlong(1,1) = 'Model interpolated ICE'
      fbdata%caddunit(1,1) = 'Fraction'
      fbdata%cgrid(1)      = 'T'
      DO ja = 1, nadd
         fbdata%caddname(1+ja) = padd%cdname(ja)
         fbdata%caddlong(1+ja,1) = padd%cdlong(ja,1)
         fbdata%caddunit(1+ja,1) = padd%cdunit(ja,1)
      END DO

      WRITE(cfname, FMT="(A,'_fdbk_',I4.4,'.nc')") TRIM(cprefix), nproc

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*)'obs_wri_seaice :'
         WRITE(numout,*)'~~~~~~~~~~~~~~~~'
         WRITE(numout,*)'Writing SEAICE feedback file : ',TRIM(cfname)
      ENDIF

      ! Transform obs_prof data structure into obfbdata structure
      fbdata%cdjuldref = '19500101000000'
      DO jo = 1, seaicedata%nsurf
         fbdata%plam(jo)      = seaicedata%rlam(jo)
         fbdata%pphi(jo)      = seaicedata%rphi(jo)
         WRITE(fbdata%cdtyp(jo),'(I4)') seaicedata%ntyp(jo)
         fbdata%ivqc(jo,:)    = 0
         fbdata%ivqcf(:,jo,:) = 0
         IF ( seaicedata%nqc(jo) > 10 ) THEN
            fbdata%ioqc(jo)    = 4
            fbdata%ioqcf(1,jo) = 0
            fbdata%ioqcf(2,jo) = seaicedata%nqc(jo) - 10
         ELSE
            fbdata%ioqc(jo)    = MAX(seaicedata%nqc(jo),1)
            fbdata%ioqcf(:,jo) = 0
         ENDIF
         fbdata%ipqc(jo)      = 0
         fbdata%ipqcf(:,jo)   = 0
         fbdata%itqc(jo)      = 0
         fbdata%itqcf(:,jo)   = 0
         fbdata%cdwmo(jo)     = ''
         fbdata%kindex(jo)    = seaicedata%nsfil(jo)
         IF (ln_grid_global) THEN
            fbdata%iobsi(jo,1) = seaicedata%mi(jo)
            fbdata%iobsj(jo,1) = seaicedata%mj(jo)
         ELSE
            fbdata%iobsi(jo,1) = mig(seaicedata%mi(jo))
            fbdata%iobsj(jo,1) = mjg(seaicedata%mj(jo))
         ENDIF
         CALL greg2jul( 0, &
            &           seaicedata%nmin(jo), &
            &           seaicedata%nhou(jo), &
            &           seaicedata%nday(jo), &
            &           seaicedata%nmon(jo), &
            &           seaicedata%nyea(jo), &
            &           fbdata%ptim(jo),   &
            &           krefdate = 19500101 )
         fbdata%padd(1,jo,1,1) = seaicedata%rmod(jo,1)
         fbdata%pob(1,jo,1)    = seaicedata%robs(jo,1)
         fbdata%pdep(1,jo)     = 0.0
         fbdata%idqc(1,jo)     = 0
         fbdata%idqcf(:,1,jo)  = 0
         IF ( seaicedata%nqc(jo) > 10 ) THEN
            fbdata%ivlqc(1,jo,1) = 4
            fbdata%ivlqcf(1,1,jo,1) = 0
            fbdata%ivlqcf(2,1,jo,1) = seaicedata%nqc(jo) - 10
         ELSE
            fbdata%ivlqc(1,jo,1) = MAX(seaicedata%nqc(jo),1)
            fbdata%ivlqcf(:,1,jo,1) = 0
         ENDIF
         fbdata%iobsk(1,jo,1)  = 0
         DO ja = 1, nadd
            fbdata%padd(1,jo,1+ja,1) = &
               & seaicedata%rext(jo,padd%ipoint(ja))
         END DO
         DO je = 1, next
            fbdata%pext(1,jo,je) = &
               & seaicedata%rext(jo,pext%ipoint(je))
         END DO

      END DO

      ! Write the obfbdata structure
      CALL write_obfbdata( cfname, fbdata )

      CALL dealloc_obfbdata( fbdata )

   END SUBROUTINE obs_wri_seaice

   SUBROUTINE obs_wri_vel( cprefix, profdata, k2dint, padd, pext )
      !!-----------------------------------------------------------------------
      !!
      !!                     *** ROUTINE obs_wri_p3d  ***
      !!
      !! ** Purpose : Write current (profile) observation 
      !!              related diagnostics
      !!
      !! ** Method  : NetCDF
      !! 
      !! ** Action  :
      !!
      !! History :
      !!      ! 09-01  (K. Mogensen) New feedback format routine
      !!-----------------------------------------------------------------------

      !! * Modules used

      !! * Arguments
      CHARACTER(LEN=*), INTENT(IN) :: cprefix       ! Prefix for output files
      TYPE(obs_prof), INTENT(INOUT) :: profdata     ! Full set of profile data
      INTEGER, INTENT(IN) :: k2dint                 ! Horizontal interpolation method
      TYPE(obswriinfo), OPTIONAL :: padd            ! Additional info for each variable
      TYPE(obswriinfo), OPTIONAL :: pext            ! Extra info

      !! * Local declarations
      TYPE(obfbdata) :: fbdata
      CHARACTER(LEN=40) :: cfname
      INTEGER :: ilevel
      INTEGER :: jvar
      INTEGER :: jk
      INTEGER :: ik
      INTEGER :: jo
      INTEGER :: ja
      INTEGER :: je
      INTEGER :: nadd
      INTEGER :: next
      REAL(wp) :: zpres
      REAL(wp), DIMENSION(:), ALLOCATABLE :: &
         & zu, &
         & zv

      IF ( PRESENT( padd ) ) THEN
         nadd = padd%inum
      ELSE
         nadd = 0
      ENDIF

      IF ( PRESENT( pext ) ) THEN
         next = pext%inum
      ELSE
         next = 0
      ENDIF

      CALL init_obfbdata( fbdata )

      ! Find maximum level
      ilevel = 0
      DO jvar = 1, 2
         ilevel = MAX( ilevel, MAXVAL( profdata%var(jvar)%nvlidx(:) ) )
      END DO
      CALL alloc_obfbdata( fbdata, 2, profdata%nprof, ilevel, 2, 0, .TRUE. )

      fbdata%cname(1)      = 'UVEL'
      fbdata%cname(2)      = 'VVEL'
      fbdata%coblong(1)    = 'Zonal velocity'
      fbdata%coblong(2)    = 'Meridional velocity'
      fbdata%cobunit(1)    = 'm/s'
      fbdata%cobunit(2)    = 'm/s'
      DO je = 1, next
         fbdata%cextname(je) = pext%cdname(je)
         fbdata%cextlong(je) = pext%cdlong(je,1)
         fbdata%cextunit(je) = pext%cdunit(je,1)
      END DO
      fbdata%caddname(1)   = 'Hx'
      fbdata%caddlong(1,1) = 'Model interpolated zonal velocity'
      fbdata%caddlong(1,2) = 'Model interpolated meridional velocity'
      fbdata%caddunit(1,1) = 'm/s'
      fbdata%caddunit(1,2) = 'm/s'
      fbdata%caddname(2)   = 'HxG'
      fbdata%caddlong(2,1) = 'Model interpolated zonal velocity (model grid)'
      fbdata%caddlong(2,2) = 'Model interpolated meridional velocity (model grid)'
      fbdata%caddunit(2,1) = 'm/s'
      fbdata%caddunit(2,2) = 'm/s' 
      fbdata%cgrid(1)      = 'U' 
      fbdata%cgrid(2)      = 'V'
      DO ja = 1, nadd
         fbdata%caddname(2+ja) = padd%cdname(ja)
         fbdata%caddlong(2+ja,1) = padd%cdlong(ja,1)
         fbdata%caddunit(2+ja,1) = padd%cdunit(ja,1)
      END DO

      WRITE(cfname, FMT="(A,'_fdbk_',I4.4,'.nc')") TRIM(cprefix), nproc

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*)'obs_wri_vel :'
         WRITE(numout,*)'~~~~~~~~~~~~~'
         WRITE(numout,*)'Writing velocuty feedback file : ',TRIM(cfname)
      ENDIF

      ALLOCATE( &
         & zu(profdata%nvprot(1)), &
         & zv(profdata%nvprot(2))  &
         & )
      CALL obs_rotvel( profdata, k2dint, zu, zv )

      ! Transform obs_prof data structure into obfbdata structure
      fbdata%cdjuldref = '19500101000000'
      DO jo = 1, profdata%nprof
         fbdata%plam(jo)      = profdata%rlam(jo)
         fbdata%pphi(jo)      = profdata%rphi(jo)
         WRITE(fbdata%cdtyp(jo),'(I4)') profdata%ntyp(jo)
         fbdata%ivqc(jo,:)    = profdata%ivqc(jo,:)
         fbdata%ivqcf(:,jo,:) = profdata%ivqcf(:,jo,:)
         IF ( profdata%nqc(jo) > 10 ) THEN
            fbdata%ioqc(jo)    = 4
            fbdata%ioqcf(1,jo) = profdata%nqcf(1,jo)
            fbdata%ioqcf(2,jo) = profdata%nqc(jo) - 10
         ELSE
            fbdata%ioqc(jo)    = profdata%nqc(jo)
            fbdata%ioqcf(:,jo) = profdata%nqcf(:,jo)
         ENDIF
         fbdata%ipqc(jo)      = profdata%ipqc(jo)
         fbdata%ipqcf(:,jo)   = profdata%ipqcf(:,jo)
         fbdata%itqc(jo)      = profdata%itqc(jo)
         fbdata%itqcf(:,jo)   = profdata%itqcf(:,jo)
         fbdata%cdwmo(jo)     = profdata%cwmo(jo)
         fbdata%kindex(jo)    = profdata%npfil(jo)
         DO jvar = 1, profdata%nvar
            IF (ln_grid_global) THEN
               fbdata%iobsi(jo,jvar) = profdata%mi(jo,jvar)
               fbdata%iobsj(jo,jvar) = profdata%mj(jo,jvar)
            ELSE
               fbdata%iobsi(jo,jvar) = mig(profdata%mi(jo,jvar))
               fbdata%iobsj(jo,jvar) = mjg(profdata%mj(jo,jvar))
            ENDIF
         END DO
         CALL greg2jul( 0, &
            &           profdata%nmin(jo), &
            &           profdata%nhou(jo), &
            &           profdata%nday(jo), &
            &           profdata%nmon(jo), &
            &           profdata%nyea(jo), &
            &           fbdata%ptim(jo),   &
            &           krefdate = 19500101 )
         ! Reform the profiles arrays for output
         DO jvar = 1, 2
            DO jk = profdata%npvsta(jo,jvar), profdata%npvend(jo,jvar)
               ik = profdata%var(jvar)%nvlidx(jk)
               IF ( jvar == 1 ) THEN
                  fbdata%padd(ik,jo,1,jvar) = zu(jk)
               ELSE
                  fbdata%padd(ik,jo,1,jvar) = zv(jk)
               ENDIF
               fbdata%padd(ik,jo,2,jvar) = profdata%var(jvar)%vmod(jk)
               fbdata%pob(ik,jo,jvar)    = profdata%var(jvar)%vobs(jk)
               fbdata%pdep(ik,jo)        = profdata%var(jvar)%vdep(jk)
               fbdata%idqc(ik,jo)        = profdata%var(jvar)%idqc(jk)
               fbdata%idqcf(:,ik,jo)     = profdata%var(jvar)%idqcf(:,jk)
               IF ( profdata%var(jvar)%nvqc(jk) > 10 ) THEN
                  fbdata%ivlqc(ik,jo,jvar) = 4
                  fbdata%ivlqcf(1,ik,jo,jvar) = profdata%var(jvar)%nvqcf(1,jk)
                  fbdata%ivlqcf(2,ik,jo,jvar) = profdata%var(jvar)%nvqc(jk) - 10
               ELSE
                  fbdata%ivlqc(ik,jo,jvar) = profdata%var(jvar)%nvqc(jk)
                  fbdata%ivlqcf(:,ik,jo,jvar) = profdata%var(jvar)%nvqcf(:,jk)
               ENDIF
               fbdata%iobsk(ik,jo,jvar)  = profdata%var(jvar)%mvk(jk)
               DO ja = 1, nadd
                  fbdata%padd(ik,jo,2+ja,jvar) = &
                     & profdata%var(jvar)%vext(jk,padd%ipoint(ja))
               END DO
               DO je = 1, next
                  fbdata%pext(ik,jo,je) = &
                     & profdata%var(jvar)%vext(jk,pext%ipoint(je))
               END DO
            END DO
         END DO
      END DO

      ! Write the obfbdata structure
      CALL write_obfbdata( cfname, fbdata )
      
      CALL dealloc_obfbdata( fbdata )
     
      DEALLOCATE( &
         & zu, &
         & zv  &
         & )

   END SUBROUTINE obs_wri_vel

END MODULE obs_write
