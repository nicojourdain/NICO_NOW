MODULE obcrst
#if defined key_obc
   !!=================================================================================
   !!                       ***  MODULE  obcrst  ***
   !! Ocean dynamic :  Input/Output files for restart on OBC
   !!=================================================================================

   !!---------------------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE phycst          ! physical constants
   USE obc_oce         ! ocean open boundary conditions
   USE lib_mpp         ! for mppobc
   USE in_out_manager  ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   obc_rst_read    ! routine called by obc_ini
   PUBLIC   obc_rst_write   ! routine called by step

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obcrst.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obc_rst_write ( kt )
      !!--------------------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_rst_write  ***
      !!                
      !! ** Purpose :   Write open boundary restart fields in restart.obc.output file 
      !!
      !! ** Method  :   restart.obc.output file: Direct access non formatted file.
      !!      Each nstock time step , save fields which are necessary for restart.
      !!      - This routine is called if at least the key_obc is defined. It is called
      !!        at the same time step than rstwrite.
      !!      - First record holds OBC parameters nbobc,jpieob,jpiwob,jpjnob,jpjsob and 
      !!        the OBC layout jpjed, jpjef ... for checking purposes.
      !!      - Following records hold the boundary arrays, in the order east west north
      !!        south, if they exist.
      !!      - The writing is realised by vertical slab across the boundary, for bsf, u,
      !!        v, t, and s boundary arrays. Each record hold a vertical slab.
      !!      - For mpp, this allows each processor to write only the correct informations
      !!        it hold. If a processor has no valid informations on boundary, it just 
      !!        skip the writing part (automatically).
      !!      - Special care is taken for dumping the starting point of a boundary (jpjed,
      !!        jpjwd, jpind, jpisd) because of the general definition of nje0 njw0,nin0,
      !!        nis0. This is done to avoid records to be written by 2 adjacent processors.
      !!
      !!  History :
      !!         ! 97-11 (J.M. Molines) Original code
      !!         ! 98-11 (J.M. Molines) Bug fix for adjacent processors
      !!   8.5   ! 02-10 (C. Talandier, A-M. Treguier) F90
      !!         ! 03-06 (J.M. Molines) Bug fix for adjacent processors
      !!   9.0   ! 04-02 (G. Madec)  suppression of numwob, use inum
      !!-----------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index

      !! * Local declarations
      INTEGER ::   ji, jj, jk
      INTEGER ::   inum               ! temporary logical unit
      INTEGER ::   ibloc, nreclo, jrec, jt, jb 
      INTEGER ::   jfoe, jfow, ifon, ifos
      INTEGER ::   ino0, it0
      !!-----------------------------------------------------------------------------

      ! 1. Output of restart fields (inum)
      ! ------------------------------------
 
      IF( ( mod(kt,nstock) == 0 ) .OR. ( kt == nitend ) ) THEN

         ! 1.0 Initializations
         ! -------------------
         IF(lwp) THEN
              WRITE(numout,*) ' '
              WRITE(numout,*) 'obcrst: OBC output for restart with obc_rst_write routine'
              WRITE(numout,*) '~~~~~~'
              WRITE(numout,*) '        output done in restart.obc.output file at it= ', kt, ' date= ', ndastp
         END IF

         ino0 = no
         it0  = kt
         ibloc  = 4096*4
         nreclo = ibloc*( ( ( 26 *jpk + 9 )*jpbyt -1)/ibloc + 1)
         IF(lwp) WRITE(numout,*) '             '
         IF(lwp) WRITE(numout,*) '        OBC restart file opened with nreclo = ',nreclo

         ! 1.1 Open file
         ! -------------

         CALL ctl_opn( inum, 'restart.obc.output', 'UNKNOWN', 'UNFORMATTED', 'DIRECT', nreclo, numout, lwp )
 
         ! 1.2 Write header
         ! ----------------
         WRITE (inum,REC=1) ino0,it0,nbobc,jpieob,jpiwob,jpjnob,jpjsob,     &
                              jpjed,jpjef,jpjwd,jpjwf,jpind,jpinf,jpisd,jpisf

         ! 1.3 Write east boundary array if any.
         ! -------------------------------------
         IF( lp_obc_east ) THEN
            IF( lfbceast ) THEN
               IF(lwp) THEN
                  WRITE(numout,*) ' '
                  WRITE(numout,*) '        No restart file for the fixed east OBC'
               END IF
            ELSE
               IF( jpieob /= 0 ) THEN
                  IF( nje0+njmpp-1  == jpjed .AND. nie1 >= nie0 ) THEN
            ! ... dump of jpjed if it is on this proc.
                     jrec = 2
                     jfoe = jpjed - njmpp + 1
                     PRINT *,'Narea =',narea,' write jrec =2 east'
                     WRITE(inum,REC=jrec)                                    &
                           ((( uebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                           ((( vebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                           ((( tebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                           ((( sebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
                  ENDIF
                  DO ji = nie0, nie1
                     DO jj = nje0, nje1
            ! ... only interested processors go through the following lines
            !           jfoe = jj + njmpp -1
                        jfoe = jj 
                        jrec = 2 + jj + njmpp -1 -jpjed
                        WRITE (inum,REC=jrec)                                   &
                              ((( uebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                              ((( vebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                              ((( tebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                              ((( sebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
                     END DO
                  END DO
               END IF
            END IF
         END IF
 
         ! 1.4 Write west boundary arrays if any
         ! -------------------------------------
         IF( lp_obc_west ) THEN
            IF( lfbcwest ) THEN
               IF(lwp) THEN
                  WRITE(numout,*) ' '
                  WRITE(numout,*) '        No restart file for the fixed west OBC'
               END IF
            ELSE
               IF( jpiwob /= 0 ) THEN
                  IF( njw0+njmpp+1 == jpjwd .AND. niw1 >= niw0 ) THEN
            ! ... dump of jpjwd if it is on this proc.
                     jrec = 3 + jpjef - jpjed
            !        jfow = jpjwd
                     jfow = jpjwd -njmpp + 1
                     PRINT *,'Narea =',narea,' write jrec =',jrec,' west'
                     WRITE (inum,REC=jrec)                                   &
                           ((( uwbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                           ((( vwbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                           ((( twbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                           ((( swbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
                  END IF
                  DO ji = niw0, niw1
                     DO jj = njw0, njw1
            ! ... only interested processors go through the following lines
            !           jfow = jj + njmpp -1
                        jfow = jj 
                        jrec = 3 + jpjef -jpjed + jj + njmpp -1 -jpjwd
                        WRITE (inum,REC=jrec)                                   &
                              ((( uwbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                              ((( vwbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                              ((( twbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                              ((( swbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
                     END DO
                  END DO
               END IF
            END IF
         END IF
 
         ! 1.5 Write north boundary arrays if any
         ! --------------------------------------
         IF( lp_obc_north ) THEN
            IF( lfbcnorth ) THEN
               IF(lwp) THEN
                  WRITE(numout,*) ' '
                  WRITE(numout,*) '        No restart file for the fixed north OBC'
               END IF
            ELSE
               IF( jpjnob /= 0) THEN
                  IF( nin0+nimpp-1 == jpind .AND. njn1 >= njn0 ) THEN
            ! ... dump of jpind if it is on this proc.
                     jrec = 4 + jpjef -jpjed + jpjwf -jpjwd
            !        ifon = jpind
                     ifon = jpind -nimpp +1
                     WRITE (inum,REC=jrec)                                   &
                           ((( unbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                           ((( vnbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                           ((( tnbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                           ((( snbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
                  END IF
                  DO jj = njn0, njn1
                     DO ji = nin0, nin1
            ! ... only interested processors go through the following lines
            !           ifon = ji + nimpp -1
                        ifon = ji 
                        jrec = 4 + jpjef -jpjed + jpjwf -jpjwd +ji + nimpp -1  -jpind
                        WRITE (inum,REC=jrec)                                   &
                              ((( unbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                              ((( vnbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                              ((( tnbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                              ((( snbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
                     END DO
                  END DO
               END IF
            END IF
         END IF
 
         ! 1.6 Write south boundary arrays if any
         ! --------------------------------------
         IF( lp_obc_south ) THEN
            IF( lfbcsouth ) THEN
               IF(lwp) THEN
                  WRITE(numout,*) ' '
                  WRITE(numout,*) '        No restart file for the fixed south OBC'
                  WRITE(numout,*) ' '
               END IF
            ELSE
               IF( jpjsob /= 0 ) THEN
                  IF( nis0+nimpp-1 == jpisd .AND. njs1 >= njs0 ) THEN
            ! ... dump of jpisd if it is on this proc.
                     jrec = 5 + jpjef -jpjed + jpjwf -jpjwd +jpinf -jpind
            !        ifos = jpisd
                     ifos = jpisd -nimpp + 1
                     WRITE (inum,REC=jrec)                                   &
                           ((( usbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                           ((( vsbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                           ((( tsbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                           ((( ssbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
                  END IF
                  DO jj = njs0, njs1
                     DO ji = nis0, nis1
            ! ... only interested processors go through the following lines
            !           ifos = ji + nimpp -1
                        ifos = ji 
                        jrec = 5 + jpjef -jpjed + jpjwf -jpjwd +jpinf -jpind + &
                              ji + nimpp -1 -jpisd
                        WRITE (inum,REC=jrec) &
                              ((( usbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                              ((( vsbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                              ((( tsbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                              ((( ssbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
                     END DO
                  END DO
               END IF
            END IF
         END IF
      CLOSE(inum)
      END IF

   END SUBROUTINE obc_rst_write


   SUBROUTINE obc_rst_read
      !!----------------------------------------------------------------------------
      !!                   ***  SUBROUTINE obc_rst_read  ***
      !!                   
      !! ** Purpose :   Read files for restart at open boundaries
      !!
      !! ** Method  :   Read the previous boundary arrays on unit inum
      !!      The first record indicates previous characterics
      !!
      !! History :
      !!        ! 97-11 (J.M. Molines) Original code
      !!   8.5  ! 02-10 (C. Talandier, A-M. Treguier) F90
      !!----------------------------------------------------------------------------
      !! * Local declarations
      INTEGER ::   inum = 11            ! temporary logical unit
      INTEGER ::   ji,jj,jk
      INTEGER ::   ino0,it0,nbobc0,jpieob0,jpiwob0,jpjnob0,jpjsob0
      INTEGER ::   ino1,it1,nbobc1,jpieob1,jpiwob1,jpjnob1,jpjsob1
      INTEGER ::   ied0,ief0,iwd0,iwf0,ind0,inf0,isd0,isf0
      INTEGER ::   ied1,ief1,iwd1,iwf1,ind1,inf1,isd1,isf1
      INTEGER ::   ibloc, nreclo, jrec, jt, jb
      INTEGER ::   jfoe, jfow, ifon, ifos
      !!-----------------------------------------------------------------------------

      ! 0. Initialisations
      ! ------------------
 
      ino0    = no
      it0     = nit000
      nbobc0  = nbobc
      jpieob0 = jpieob
      jpiwob0 = jpiwob
      jpjnob0 = jpjnob
      jpjsob0 = jpjsob
 
      ied0   = jpjed
      ief0   = jpjef
      iwd0   = jpjwd
      iwf0   = jpjwf
      ind0   = jpind
      inf0   = jpinf
      isd0   = jpisd
      isf0   = jpisf
 
      ibloc  = 4096*4
      nreclo = ibloc *( ( ( 26 *jpk + 9 )*jpbyt -1)/ibloc + 1)
 
      IF(lwp) THEN
         WRITE(numout,*) 'obcrst: beginning of restart with obc_rst_read routine'
         WRITE(numout,*) '~~~~~~'
         WRITE(numout,*) ' '
         WRITE(numout,*) '        The present run :'
         WRITE(numout,*) '        number job is  : ',no 
         WRITE(numout,*) '        with the time nit000 : ',nit000
         WRITE(numout,*) '        OBC restart file opened with nreclo = ',nreclo 
      END IF
 
      ! 0.1 Open files
      ! ---------------
      CALL ctl_opn( inum, 'restart.obc', 'UNKNOWN', 'UNFORMATTED', 'DIRECT', nreclo, numout, lwp )

      ! 1. Read
      ! -------
 
      ! 1.1 First record
      ! -----------------
      READ(inum,REC=1) ino1,it1,nbobc1,jpieob1,jpiwob1,jpjnob1,     &
                         jpjsob1,ied1,ief1,iwd1,iwf1,ind1,inf1,isd1,isf1
 
      IF(lwp) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) '        READ inum with number job : ',ino1,' with the time it: ',it1
         WRITE(numout,*) ' '
      END IF
 
      ! 1.2 Control of date
      ! --------------------
      IF( ( it0-it1 ) /= 1 .AND. abs(nrstdt) == 1 ) THEN
          CALL ctl_stop( '        ===>>>> : problem with nit000 for the restart',   &
               &         '        ==============',   &
               &         '        we stop in obc_rst_read routine. Verify the file or rerun with the value',   &
               &         '        0 for the control of time parameter nrstdt' )
             
      END IF
 
      ! 1.3 Control of number of open boundaries
      ! ----------------------------------------
      IF( nbobc1 /= nbobc0 ) THEN
         IF(lwp) THEN
            WRITE(numout,*) '        ===> W A R N I N G: The number of OBC have changed:'
            WRITE(numout,*) '        Last run : ',nbobc0,' obcs'
            WRITE(numout,*) '        This run : ',nbobc1,' obcs'
         END IF
      END IF
 
      ! 1.4 Control of which boundary is open
      ! -------------------------------------
      IF( lp_obc_east .AND. ( jpieob1 /= 0 ) ) THEN
         IF(lwp) THEN
            WRITE(numout,*) '         '
            WRITE(numout,*) '        East open boundary'
            IF( jpieob0 /= jpieob1 ) CALL ctl_stop( '         ==>>>> : Problem in obc_rst_read, jpieob have changed' )
         END IF
      END IF
 
      IF( lp_obc_west .AND. ( jpiwob1 /= 0 ) ) THEN
         IF(lwp) THEN
            WRITE(numout,*) '         '
            WRITE(numout,*) '        West open boundary'
            IF( jpiwob0 /= jpiwob1 ) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpiwob has changed' )
         END IF
      END IF
 
      IF( lp_obc_north .AND. ( jpjnob1 /= 0 ) ) THEN
         IF(lwp) THEN
            WRITE(numout,*) '         '
            WRITE(numout,*) '        North open boundary'
            IF( jpjnob0 /= jpjnob1 ) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpjnob has changed' )
         END IF
      END IF
 
      IF( lp_obc_south .AND. ( jpjsob1 /= 0 ) ) THEN
         IF(lwp) THEN
            WRITE(numout,*) '         '
            WRITE(numout,*) '        South open boundary'
            IF( jpjsob0 /= jpjsob1) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpjsob has changed' )
         END IF
      END IF
 
 
      ! 1.5 Control of the limit of the boundaries
      ! ------------------------------------------
      IF( lp_obc_east .AND. ( jpieob1 /= 0 ) ) THEN
         IF( ied1 /= ied0 ) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpjed has changed' )
         IF( ief1 /= ief0 ) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpjef has changed' )
      END IF

      IF( lp_obc_west .AND. ( jpiwob1 /= 0 ) ) THEN
         IF( iwd1 /= iwd0 ) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpjwd has changed' )
         IF( iwf1 /= iwf0 ) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpjwf has changed' )
      END IF
 
      IF( lp_obc_north .AND. ( jpjnob1 /= 0 ) ) THEN
         IF( ind1 /= ind0 ) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpind has changed' )
         IF( inf1 /= inf0 ) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpinf has changed' )
      END IF
 
      IF( lp_obc_south .AND. ( jpjsob1 /= 0 ) ) THEN
         IF( isd1 /= isd0 ) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpisd has changed' )
         IF( isf1 /= isf0 ) CALL ctl_stop( '        ==>>>> : Problem in obc_rst_read, jpisf has changed' )
      END IF
 
 
      ! 2. Now read the boundary arrays
      ! -------------------------------
 
      ! 2.1 Read east boundary array if any.
      ! ------------------------------------
      IF( lp_obc_east ) THEN
         IF( jpieob1 /= 0) THEN
            IF( nje0+njmpp-1 == jpjed .AND. nie1 >= nie0 ) THEN
      ! ... read of jpjed if it is on this proc.
               jrec = 2
      !        jfoe = jpjed
               jfoe = jpjed -njmpp + 1
               READ (inum,REC=jrec)                                   &
                    ((( uebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                    ((( vebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                    ((( tebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                    ((( sebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
            END IF
            DO ji = nie0, nie1
               DO jj = nje0, nje1
      ! ... only interested processors go through the following lines
      !           jfoe = jj + njmpp -1
                  jfoe = jj 
                  jrec = 2 + jj + njmpp -1 -jpjed
                  READ (inum,REC=jrec)                                   &
                       ((( uebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                       ((( vebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                       ((( tebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                       ((( sebnd(jfoe,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
               END DO
            END DO

         ELSE

            !  lp_obc_east was not TRUE previously
         END IF

      END IF
 
      ! 2.2 Read west boundary arrays if any.
      ! -------------------------------------
      IF( lp_obc_west ) THEN
         IF( jpiwob1 /= 0) THEN
            IF( njw0+njmpp-1 == jpjwd .AND. niw1 >= niw0 ) THEN
      ! ... read of jpjwd if it is on this proc.
               jrec = 3 + jpjef - jpjed
      !        jfow = jpjwd
               jfow = jpjwd -njmpp + 1
               READ (inum,REC=jrec)                                   &
                    ((( uwbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                    ((( vwbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                    ((( twbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                    ((( swbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
            END IF
            DO ji = niw0, niw1
               DO jj = njw0, njw1
      ! ... only interested processors go through the following lines
      !           jfow = jj + njmpp -1
                  jfow = jj 
                  jrec = 3 + jpjef -jpjed + jj + njmpp -1 -jpjwd
                  READ (inum,REC=jrec)                                   &
                       ((( uwbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                       ((( vwbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                       ((( twbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                       ((( swbnd(jfow,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
               END DO
            END DO

         ELSE

            !  lp_obc_west was not TRUE previously
         END IF

      END IF
 
      ! 2.3 Read north boundary arrays if any.
      ! --------------------------------------
      IF( lp_obc_north ) THEN
         IF( jpjnob1 /= 0) THEN
            IF( nin0+nimpp-1 == jpind .AND. njn1 >= njn0 ) THEN
      ! ... read of jpind if it is on this proc.
               jrec = 4 + jpjef -jpjed + jpjwf -jpjwd
      !        ifon = jpind
               ifon = jpind -nimpp +1
               READ (inum,REC=jrec)                                   &
                    ((( unbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                    ((( vnbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                    ((( tnbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), & 
                    ((( snbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
            END IF
            DO jj = njn0, njn1
               DO ji = nin0, nin1
      ! ... only interested processors go through the following lines
      !           ifon = ji + nimpp -1
                  ifon = ji 
                  jrec = 4 + jpjef -jpjed + jpjwf -jpjwd +ji + nimpp -1  -jpind
                  READ (inum,REC=jrec)                                   & 
                       ((( unbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                       ((( vnbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                       ((( tnbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                       ((( snbnd(ifon,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
               END DO
            END DO

         ELSE

           !  lp_obc_north was not TRUE previously
         END IF

      END IF
 
      ! 2.4 Read south boundary arrays if any.
      ! -------------------------------------
      IF( lp_obc_south ) THEN
         IF( jpjsob1 /= 0) THEN
            IF( nis0+nimpp-1 == jpisd .AND. njs1 >= njs0 ) THEN
      ! ... read of jpisd if it is on this proc.
               jrec = 5 + jpjef -jpjed + jpjwf -jpjwd +jpinf -jpind
      !        ifos = jpisd
               ifos = jpisd -nimpp + 1
               READ (inum,REC=jrec)                                   &
                    ((( usbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                    ((( vsbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                    ((( tsbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                    ((( ssbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
            END IF
            DO jj = njs0, njs1
               DO ji = nis0, nis1
      ! ... only interested processors go through the following lines
      !           ifos = ji + nimpp -1
                  ifos = ji 
                  jrec = 5 + jpjef -jpjed + jpjwf -jpjwd +jpinf -jpind +  &
                        ji + nimpp -1 -jpisd
                  READ (inum,REC=jrec)                                   & 
                       ((( usbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                       ((( vsbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,3),jt=1,3), &
                       ((( tsbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2), &
                       ((( ssbnd(ifos,jk,jb,jt),jk=1,jpk),jb=1,2),jt=1,2)
               END DO
            END DO
         ELSE
            !  lp_obc_south was not TRUE previously
         END IF

      END IF
      CLOSE(inum)

      IF( lk_mpp ) THEN
         IF( lp_obc_east ) THEN
            CALL mppobc(uebnd,jpjed,jpjef,jpieob  ,jpk*3*3,2,jpj, numout )
            CALL mppobc(vebnd,jpjed,jpjef,jpieob+1,jpk*3*3,2,jpj, numout )
            CALL mppobc(tebnd,jpjed,jpjef,jpieob+1,jpk*2*2,2,jpj, numout )
            CALL mppobc(sebnd,jpjed,jpjef,jpieob+1,jpk*2*2,2,jpj, numout )
         ENDIF
         IF( lp_obc_west ) THEN
            CALL mppobc(uwbnd,jpjwd,jpjwf,jpiwob,jpk*3*3,2,jpj, numout )
            CALL mppobc(vwbnd,jpjwd,jpjwf,jpiwob,jpk*3*3,2,jpj, numout )
            CALL mppobc(twbnd,jpjwd,jpjwf,jpiwob,jpk*2*2,2,jpj, numout )
            CALL mppobc(swbnd,jpjwd,jpjwf,jpiwob,jpk*2*2,2,jpj, numout )
         ENDIF
         IF( lp_obc_north ) THEN 
            CALL mppobc(unbnd,jpind,jpinf,jpjnob+1,jpk*3*3,1,jpi, numout )
            CALL mppobc(vnbnd,jpind,jpinf,jpjnob  ,jpk*3*3,1,jpi, numout )
            CALL mppobc(tnbnd,jpind,jpinf,jpjnob+1,jpk*2*2,1,jpi, numout )
            CALL mppobc(snbnd,jpind,jpinf,jpjnob+1,jpk*2*2,1,jpi, numout )
         ENDIF
         IF( lp_obc_south ) THEN
            CALL mppobc(usbnd,jpisd,jpisf,jpjsob,jpk*3*3,1,jpi, numout )
            CALL mppobc(vsbnd,jpisd,jpisf,jpjsob,jpk*3*3,1,jpi, numout )
            CALL mppobc(tsbnd,jpisd,jpisf,jpjsob,jpk*2*2,1,jpi, numout )
            CALL mppobc(ssbnd,jpisd,jpisf,jpjsob,jpk*2*2,1,jpi, numout )
         ENDIF
      ENDIF
 
   END SUBROUTINE obc_rst_read
#else
   !!=================================================================================
   !!                       ***  MODULE  obcrst  ***
   !! Ocean dynamic :  Input/Output files for restart on OBC
   !!=================================================================================
CONTAINS
   SUBROUTINE obc_rst_write( kt )           !  No Open boundary ==> empty routine
      INTEGER,INTENT(in) :: kt
      WRITE(*,*) 'obc_rst_write: You should not have seen this print! error?', kt
   END SUBROUTINE obc_rst_write
   SUBROUTINE obc_rst_read                 !  No Open boundary ==> empty routine
   END SUBROUTINE obc_rst_read
#endif

   !!=================================================================================
END MODULE obcrst
