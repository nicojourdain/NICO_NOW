MODULE trcdia
   !!======================================================================
   !!                       *** MODULE trcdia ***
   !! TOP :   Output of passive tracers
   !!======================================================================
   !! History :   OPA  !  1995-01 (M. Levy)  Original code
   !!              -   !  1998-01 (C. Levy) NETCDF format using ioipsl interface
   !!              -   !  1999-01 (M.A. Foujols) adapted for passive tracer
   !!              -   !  1999-09 (M.A. Foujols) split into three parts
   !!   NEMO      1.0  !  2005-03 (O. Aumont, A. El Moussaoui) F90
   !!                  !  2008-05 (C. Ethe re-organization)
   !!----------------------------------------------------------------------
#if defined key_top 
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !! trc_dia     : main routine of output passive tracer
   !! trcdit_wr   : outputs of concentration fields
   !! trcdii_wr   : outputs of additional 2D/3D diagnostics
   !! trcdib_wr   : outputs of biological fields
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain variables 
   USE oce_trc
   USE trc
   USE par_trc
   USE dianam    ! build name of file (routine)
   USE ioipsl    ! I/O manager
   USE iom       ! I/O manager
   USE lib_mpp   ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_dia        ! called by XXX module 

   INTEGER  ::   nit5      !: id for tracer output file
   INTEGER  ::   ndepit5   !: id for depth mesh
   INTEGER  ::   nhorit5   !: id for horizontal mesh
   INTEGER  ::   ndimt50   !: number of ocean points in index array
   INTEGER  ::   ndimt51   !: number of ocean points in index array
   REAL(wp) ::   zjulian   !: ????   not DOCTOR !
   INTEGER , ALLOCATABLE, SAVE, DIMENSION (:) ::   ndext50   !: integer arrays for ocean 3D index
   INTEGER , ALLOCATABLE, SAVE, DIMENSION (:) ::   ndext51   !: integer arrays for ocean surface index

   INTEGER  ::   nitd      !: id for additional array output file
   INTEGER  ::   ndepitd   !: id for depth mesh
   INTEGER  ::   nhoritd   !: id for horizontal mesh

   INTEGER  ::   nitb        !:         id.         for additional array output file
   INTEGER  ::   ndepitb   !:  id for depth mesh
   INTEGER  ::   nhoritb   !:  id for horizontal mesh

   !! * Substitutions
#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcdia.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_dia( kt )  
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_dia  ***
      !!
      !! ** Purpose :   output passive tracers fields 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt    ! ocean time-step
      !
      INTEGER             ::  ierr   ! local integer
      !!---------------------------------------------------------------------
      !
      IF( kt == nittrc000 )  THEN
         ALLOCATE( ndext50(jpij*jpk), ndext51(jpij), STAT=ierr )
         IF( ierr > 0 ) THEN
            CALL ctl_stop( 'STOP', 'trc_diat: unable to allocate arrays' )  ;   RETURN
         ENDIF
      ENDIF
      !
      IF( .NOT.lk_iomput ) THEN
                          CALL trcdit_wr( kt )      ! outputs for tracer concentration
         IF( ln_diatrc )  CALL trcdii_wr( kt )      ! outputs for additional arrays
         IF( ln_diabio )  CALL trcdib_wr( kt )      ! outputs for biological trends
      ENDIF
      !
   END SUBROUTINE trc_dia


   SUBROUTINE trcdit_wr( kt )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trcdit_wr  ***
      !!
      !! ** Purpose :   Standard output of passive tracer : concentration fields
      !!
      !! ** Method  :   At the beginning of the first time step (nittrc000), define all
      !!             the NETCDF files and fields for concentration of passive tracer
      !!
      !!        At each time step call histdef to compute the mean if necessary
      !!        Each nwritetrc time step, output the instantaneous or mean fields
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! ocean time-step
      !
      INTEGER ::   jn
      LOGICAL ::   ll_print = .FALSE.
      CHARACTER (len=40) :: clhstnam, clop
      INTEGER ::   inum = 11             ! temporary logical unit
      CHARACTER (len=20) :: cltra, cltrau
      CHARACTER (len=80) :: cltral
      REAL(wp) :: zsto, zout, zdt
      INTEGER  :: iimi, iima, ijmi, ijma, ipk, it, itmod, iiter
      !!----------------------------------------------------------------------

      ! Initialisation
      ! --------------

      ! local variable for debugging
      ll_print = .FALSE.                  ! change it to true for more control print
      ll_print = ll_print .AND. lwp

      ! Define frequency of output and means
      zdt = rdt
      IF( ln_mskland )   THEN   ;   clop = "only(x)"   ! put 1.e+20 on land (very expensive!!)
      ELSE                      ;   clop = "x"         ! no use of the mask value (require less cpu time)
      ENDIF
# if defined key_diainstant
      zsto = nn_writetrc * rdt
      clop = "inst("//TRIM(clop)//")"
# else
      zsto = zdt
      clop = "ave("//TRIM(clop)//")"
# endif
      zout = nn_writetrc * zdt

      ! Define indices of the horizontal output zoom and vertical limit storage
      iimi = 1      ;      iima = jpi
      ijmi = 1      ;      ijma = jpj
      ipk = jpk

      ! define time axis
      itmod = kt - nittrc000 + 1
      it    = kt
      iiter = ( nittrc000 - 1 ) / nn_dttrc

      ! Define NETCDF files and fields at beginning of first time step
      ! --------------------------------------------------------------

      IF(ll_print)WRITE(numout,*)'trcdit_wr kt=',kt
      
      IF( kt == nittrc000 ) THEN

         IF(lwp) THEN                   ! control print
            WRITE(numout,*)
            WRITE(numout,*) '    frequency of outputs for passive tracers nn_writetrc = ', nn_writetrc
            DO jn = 1, jptra
               IF( ln_trc_wri(jn) )  WRITE(numout,*) ' ouput tracer nb : ', jn, '    short name : ', ctrcnm(jn) 
            END DO
            WRITE(numout,*) ' '
         ENDIF

         ! Compute julian date from starting date of the run
         CALL ymds2ju( nyear, nmonth, nday, rdt, zjulian )
         zjulian = zjulian - adatrj   !   set calendar origin to the beginning of the experiment
         IF(lwp)WRITE(numout,*)' '  
         IF(lwp)WRITE(numout,*)' Date 0 used :', nittrc000                         &
            &                 ,' YEAR ', nyear, ' MONTH ', nmonth, ' DAY ', nday   &
            &                 ,'Julian day : ', zjulian  
  
         IF(lwp) WRITE(numout,*) ' indexes of zoom = ', iimi, iima, ijmi, ijma,  &
            &                    ' limit storage in depth = ', ipk

         IF( lk_offline .AND. lwp ) THEN
            CALL dia_nam( clhstnam, nn_writetrc,' ' )
            CALL ctl_opn( inum, 'date.file', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', 1, numout, lwp, narea )
            WRITE(inum,*) clhstnam
            CLOSE(inum)
         ENDIF

         ! Define the NETCDF files for passive tracer concentration
         CALL dia_nam( clhstnam, nn_writetrc, 'ptrc_T' )
         IF(lwp)WRITE(numout,*)" Name of NETCDF file ", clhstnam

         ! Horizontal grid : glamt and gphit
         CALL histbeg( clhstnam, jpi, glamt, jpj, gphit,     &
            &          iimi, iima-iimi+1, ijmi, ijma-ijmi+1,         & 
            &          iiter, zjulian, zdt, nhorit5, nit5 , domain_id=nidom, snc4chunks=snc4set)

         ! Vertical grid for tracer : gdept
         CALL histvert( nit5, 'deptht', 'Vertical T levels', 'm', ipk, gdept_0, ndepit5)

         ! Index of ocean points in 3D and 2D (surface)
         CALL wheneq( jpi*jpj*ipk, tmask, 1, 1., ndext50, ndimt50 )
         CALL wheneq( jpi*jpj    , tmask, 1, 1., ndext51, ndimt51 )

         ! Declare all the output fields as NETCDF variables
         DO jn = 1, jptra
            IF( ln_trc_wri(jn) ) THEN
               cltra  = TRIM( ctrcnm(jn) )   ! short title for tracer
               cltral = TRIM( ctrcln(jn) )   ! long title for tracer
               cltrau = TRIM( ctrcun(jn) )   ! UNIT for tracer
               CALL histdef( nit5, cltra, cltral, cltrau, jpi, jpj, nhorit5,  &
                  &          ipk, 1, ipk,  ndepit5, 32, clop, zsto, zout ) 
            ENDIF
         END DO

         ! end netcdf files header
         CALL histend( nit5, snc4set )
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'End of NetCDF Initialization in trcdit_wr'
         IF( ll_print )   CALL FLUSH(numout )

      ENDIF

      ! Start writing the tracer concentrations
      ! ---------------------------------------

      IF( lwp .AND. MOD( itmod, nn_writetrc ) == 0 ) THEN
         WRITE(numout,*) 'trcdit_wr : write NetCDF passive tracer concentrations at ', kt, 'time-step'
         WRITE(numout,*) '~~~~~~~~~ '
      ENDIF

      DO jn = 1, jptra
         cltra  = TRIM( ctrcnm(jn) )   ! short title for tracer
         IF( ln_trc_wri(jn) ) CALL histwrite( nit5, cltra, it, trn(:,:,:,jn), ndimt50, ndext50 )
      END DO

      ! close the file 
      ! --------------
      IF( kt == nitend )   CALL histclo( nit5 )
      !
   END SUBROUTINE trcdit_wr

   SUBROUTINE trcdii_wr( kt )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trcdii_wr  ***
      !!
      !! ** Purpose :   output of passive tracer : additional 2D and 3D arrays
      !!
      !! ** Method  :   At the beginning of the first time step (nittrc000), define all
      !!             the NETCDF files and fields for concentration of passive tracer
      !!
      !!        At each time step call histdef to compute the mean if necessary
      !!        Each nn_writedia time step, output the instantaneous or mean fields
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! ocean time-step
      !!
      LOGICAL ::   ll_print = .FALSE.
      CHARACTER (len=40) ::   clhstnam, clop
      CHARACTER (len=20) ::   cltra, cltrau
      CHARACTER (len=80) ::   cltral
      INTEGER  ::   jl
      INTEGER  ::   iimi, iima, ijmi, ijma, ipk, it, itmod, iiter
      REAL(wp) ::   zsto, zout, zdt
      !!----------------------------------------------------------------------

      ! Initialisation
      ! --------------
      
      ! local variable for debugging
      ll_print = .FALSE.
      ll_print = ll_print .AND. lwp
      !
      ! Define frequency of output and means
      zdt = rdt
      IF( ln_mskland )   THEN   ;   clop = "only(x)"   ! put 1.e+20 on land (very expensive!!)
      ELSE                      ;   clop = "x"         ! no use of the mask value (require less cpu time)
      ENDIF
#  if defined key_diainstant
      zsto = nn_writedia * zdt
      clop = "inst("//TRIM(clop)//")"
#  else
      zsto = zdt
      clop = "ave("//TRIM(clop)//")"
#  endif
      zout = nn_writedia * zdt

      ! Define indices of the horizontal output zoom and vertical limit storage
      iimi = 1      ;      iima = jpi
      ijmi = 1      ;      ijma = jpj
      ipk = jpk

      ! define time axis
      itmod = kt - nittrc000 + 1
      it    = kt
      iiter = ( nittrc000 - 1 ) / nn_dttrc

      ! 1. Define NETCDF files and fields at beginning of first time step
      ! -----------------------------------------------------------------

      IF( ll_print ) WRITE(numout,*) 'trcdii_wr kt=', kt

      IF( kt == nittrc000 ) THEN

         ! Define the NETCDF files for additional arrays : 2D or 3D

         ! Define the T grid file for tracer auxiliary files

         CALL dia_nam( clhstnam, nn_writedia, 'diad_T' )
         IF(lwp) WRITE(numout,*) " Name of NETCDF file ", clhstnam

         ! Define a netcdf FILE for 2d and 3d arrays

         CALL histbeg( clhstnam, jpi, glamt, jpj, gphit,             &
            &          iimi, iima-iimi+1, ijmi, ijma-ijmi+1,         &
            &          iiter, zjulian, zdt, nhoritd, nitd , domain_id=nidom, snc4chunks=snc4set )

         ! Vertical grid for 2d and 3d arrays

         CALL histvert( nitd, 'deptht', 'Vertical T levels','m', ipk, gdept_0, ndepitd)

         ! Declare all the output fields as NETCDF variables

         ! more 3D horizontal arrays
         DO jl = 1, jpdia3d
            cltra  = TRIM( ctrc3d(jl) )   ! short title for 3D diagnostic
            cltral = TRIM( ctrc3l(jl) )  ! long title for 3D diagnostic
            cltrau = TRIM( ctrc3u(jl) )  ! UNIT for 3D diagnostic
            CALL histdef( nitd, cltra, cltral, cltrau, jpi, jpj, nhoritd,   &
               &          ipk, 1, ipk,  ndepitd, 32, clop, zsto, zout )
         END DO

         ! more 2D horizontal arrays
         DO jl = 1, jpdia2d
            cltra  = TRIM( ctrc2d(jl) )   ! short title for 2D diagnostic
            cltral = TRIM( ctrc2l(jl) )  ! long title for 2D diagnostic
            cltrau = TRIM( ctrc2u(jl) )  ! UNIT for 2D diagnostic
            CALL histdef( nitd, cltra, cltral, cltrau, jpi, jpj, nhoritd,  &
               &          1, 1, 1,  -99, 32, clop, zsto, zout )
         END DO

         ! TODO: more 2D vertical sections arrays : I or J indice fixed

         ! CLOSE netcdf Files
         CALL histend( nitd, snc4set )

         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'End of NetCDF Initialization in trcdii_wr'
         IF( ll_print )   CALL FLUSH(numout )
         !
      ENDIF

      ! 2. Start writing data
      ! ---------------------

      IF( lwp .AND. MOD( itmod, nn_writedia ) == 0 ) THEN
         WRITE(numout,*) 'trcdii_wr : write NetCDF additional arrays at ', kt, 'time-step'
         WRITE(numout,*) '~~~~~~ '
      ENDIF

      ! more 3D horizontal arrays
      DO jl = 1, jpdia3d
         cltra  = TRIM( ctrc3d(jl) )   ! short title for 3D diagnostic
         CALL histwrite( nitd, cltra, it, trc3d(:,:,:,jl), ndimt50 ,ndext50)
      END DO

      ! more 2D horizontal arrays
      DO jl = 1, jpdia2d
         cltra  = TRIM( ctrc2d(jl) )   ! short title for 2D diagnostic
         CALL histwrite(nitd, cltra, it, trc2d(:,:,jl), ndimt51  ,ndext51)
      END DO

      ! Closing all files
      ! -----------------
      IF( kt == nitend )   CALL histclo(nitd)
      !

   END SUBROUTINE trcdii_wr

   SUBROUTINE trcdib_wr( kt )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trcdib_wr  ***
      !!
      !! ** Purpose :   output of passive tracer : biological fields
      !!
      !! ** Method  :   At the beginning of the first time step (nittrc000), define all
      !!             the NETCDF files and fields for concentration of passive tracer
      !!
      !!        At each time step call histdef to compute the mean if necessary
      !!        Each nn_writebio time step, output the instantaneous or mean fields
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt          ! ocean time-step
      !!
      LOGICAL ::   ll_print = .FALSE.
      CHARACTER (len=40) ::   clhstnam, clop
      CHARACTER (len=20) ::   cltra, cltrau
      CHARACTER (len=80) ::   cltral
      INTEGER  ::   ji, jj, jk, jl
      INTEGER  ::   iimi, iima, ijmi, ijma, ipk, it, itmod, iiter
      REAL(wp) ::   zsto, zout, zdt
      !!----------------------------------------------------------------------

      ! Initialisation
      ! --------------
      
      ! local variable for debugging
      ll_print = .FALSE.
      ll_print = ll_print .AND. lwp

      ! Define frequency of output and means
      zdt = rdt
      IF( ln_mskland )   THEN   ;   clop = "only(x)"   ! put 1.e+20 on land (very expensive!!)
      ELSE                      ;   clop = "x"         ! no use of the mask value (require less cpu time)
      ENDIF
#        if defined key_diainstant
      zsto = nn_writebio * zdt
      clop = "inst("//TRIM(clop)//")"
#        else
      zsto = zdt
      clop = "ave("//TRIM(clop)//")"
#        endif
      zout = nn_writebio * zdt

      ! Define indices of the horizontal output zoom and vertical limit storage
      iimi = 1      ;      iima = jpi
      ijmi = 1      ;      ijma = jpj
      ipk = jpk

      ! define time axis
      itmod = kt - nittrc000 + 1
      it    = kt
      iiter = ( nittrc000 - 1 ) / nn_dttrc

      ! Define NETCDF files and fields at beginning of first time step
      ! --------------------------------------------------------------

      IF(ll_print) WRITE(numout,*)'trcdib_wr kt=',kt

      IF( kt == nittrc000 ) THEN

         ! Define the NETCDF files for biological trends

         CALL dia_nam(clhstnam,nn_writebio,'biolog')
         IF(lwp)WRITE(numout,*) " Name of NETCDF file for biological trends ", clhstnam
         ! Horizontal grid : glamt and gphit
         CALL histbeg( clhstnam, jpi, glamt, jpj, gphit,      &
            &    iimi, iima-iimi+1, ijmi, ijma-ijmi+1,          &
            &    iiter, zjulian, zdt, nhoritb, nitb , domain_id=nidom, snc4chunks=snc4set )
         ! Vertical grid for biological trends
         CALL histvert(nitb, 'deptht', 'Vertical T levels', 'm', ipk, gdept_0, ndepitb)

         ! Declare all the output fields as NETCDF variables
         ! biological trends
         DO jl = 1, jpdiabio
            cltra  = TRIM( ctrbio(jl) )   ! short title for biological diagnostic
            cltral = TRIM( ctrbil(jl) )  ! long title for biological diagnostic
            cltrau = TRIM( ctrbiu(jl) )  ! UNIT for biological diagnostic
            CALL histdef( nitb, cltra, cltral, cltrau, jpi, jpj, nhoritb,  &
               &         ipk, 1, ipk,  ndepitb, 32, clop, zsto, zout)
         END DO

         ! CLOSE netcdf Files
          CALL histend( nitb, snc4set )

         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'End of NetCDF Initialization in trcdib_wr'
         IF(ll_print) CALL FLUSH(numout )
         !
      ENDIF

      ! Start writing data
      ! ------------------

      ! biological trends
      IF( lwp .AND. MOD( itmod, nn_writebio ) == 0 ) THEN
         WRITE(numout,*) 'trcdit_wr : write NetCDF biological trends at ', kt, 'time-step'
         WRITE(numout,*) '~~~~~~ '
      ENDIF

      DO jl = 1, jpdiabio
         cltra  = TRIM( ctrbio(jl) )   ! short title for biological diagnostic
         CALL histwrite(nitb, cltra, it, trbio(:,:,:,jl), ndimt50,ndext50)
      END DO

      ! Closing all files
      ! -----------------
      IF( kt == nitend )   CALL histclo( nitb )
      !
   END SUBROUTINE trcdib_wr

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_dia( kt )                      ! Empty routine   
      INTEGER, INTENT(in) :: kt
   END SUBROUTINE trc_dia   
#endif

   !!======================================================================
END MODULE trcdia
