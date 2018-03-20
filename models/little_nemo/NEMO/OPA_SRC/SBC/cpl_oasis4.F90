MODULE cpl_oasis4
   !!======================================================================
   !!                    ***  MODULE cpl_oasis  ***
   !! Coupled O/A : coupled ocean-atmosphere case using OASIS4
   !!=====================================================================
   !! History :   
   !!   9.0  !  2004-06  (R. Redler, NEC Laboratories Europe, St Augustin, Germany) Original code
   !!    -   !  2004-11  (R. Redler, NEC Laboratories Europe; N. Keenlyside, W. Park, IFM-GEOMAR, Kiel, Germany) revision
   !!    -   !  2004-11  (V. Gayler, MPI M&D) Grid writing
   !!    -   !  2005-08  (R. Redler, W. Park) frld initialization, paral(2) revision
   !!    -   !  2005-09  (R. Redler) extended to allow for communication over root only
   !!    -   !  2006-01  (W. Park) modification of physical part
   !!    -   !  2006-02  (R. Redler, W. Park) buffer array fix for root exchange
   !!    -   !  2010-10  (E. Maisonnave and S. Masson) complete rewrite
   !!----------------------------------------------------------------------
#if defined key_oasis4
   !!----------------------------------------------------------------------
   !!   'key_oasis4'                    coupled Ocean/Atmosphere via OASIS4
   !!----------------------------------------------------------------------
   !!   cpl_prism_init     : initialization of coupled mode communication
   !!   cpl_prism_define   : definition of grid and fields
   !!   cpl_prism_snd      : snd out fields in coupled mode
   !!   cpl_prism_rcv      : receive fields in coupled mode
   !!   cpl_prism_update_time : update date sent to Oasis
   !!   cpl_prism_finalize : finalize the coupled mode communication
   !!----------------------------------------------------------------------
   USE prism            ! OASIS4 prism module
   USE par_oce          ! ocean parameters
   USE dom_oce          ! ocean space and time domain
   USE domwri           ! ocean space and time domain
   USE in_out_manager   ! I/O manager
   USE lbclnk           ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC cpl_prism_init
   PUBLIC cpl_prism_define
   PUBLIC cpl_prism_snd
   PUBLIC cpl_prism_rcv
   PUBLIC cpl_prism_update_time
   PUBLIC cpl_prism_finalize
   
!   LOGICAL, PUBLIC, PARAMETER :: lk_cpl = .TRUE.    ! coupled flag
   INTEGER                    :: ncomp_id           ! id returned by prism_init_comp
   INTEGER                    :: nerror             ! return error code
   INTEGER, PUBLIC            :: OASIS_Rcv  = 1     ! return code if received field
   INTEGER, PUBLIC            :: OASIS_idle = 0     ! return code if nothing done by oasis

   INTEGER, PARAMETER :: nmaxfld=40    ! Maximum number of coupling fields
   
   TYPE, PUBLIC ::   FLD_CPL            ! Type for coupling field information
      LOGICAL            ::   laction   ! To be coupled or not
      CHARACTER(len = 8) ::   clname    ! Name of the coupling field   
      CHARACTER(len = 1) ::   clgrid    ! Grid type  
      REAL(wp)           ::   nsgn      ! Control of the sign change
      INTEGER            ::   nid       ! Id of the field
   END TYPE FLD_CPL

   TYPE(FLD_CPL), DIMENSION(nmaxfld), PUBLIC :: srcv, ssnd   ! Coupling fields

   REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: exfld  ! Temporary buffer for receiving

   TYPE(PRISM_Time_struct), PUBLIC    :: date            ! date info for send operation
   TYPE(PRISM_Time_struct), PUBLIC    :: date_bound(2)   ! date info for send operation

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: cpl_oasis4.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE cpl_prism_init( kl_comm ) 
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_prism_init  ***
      !!
      !! ** Purpose :   Initialize coupled mode communication for ocean
      !!    exchange between AGCM, OGCM and COUPLER. (OASIS4 software)
      !!
      !! ** Method  :   OASIS4 MPI communication 
      !!--------------------------------------------------------------------
      INTEGER, INTENT(out) ::   kl_comm   ! local communicator of the model
      !!--------------------------------------------------------------------
      
      CALL prism_init( 'nemo', nerror )

      !------------------------------------------------------------------
      ! 2nd Initialize the PRISM system for the component
      !------------------------------------------------------------------
      CALL prism_init_comp( ncomp_id, 'oceanx', nerror )
      IF( nerror /= PRISM_Success )   CALL prism_abort( ncomp_id, 'cpl_prism_init', 'Failure in prism_init_comp' )

      !------------------------------------------------------------------
      ! 3rd Get an MPI communicator fr OPA local communication
      !------------------------------------------------------------------
      CALL prism_get_localcomm( ncomp_id, kl_comm, nerror )
      IF( nerror /= PRISM_Success )   CALL prism_abort( ncomp_id, 'cpl_prism_init', 'Failure in prism_get_localcomm' )
      !
   END SUBROUTINE cpl_prism_init


   SUBROUTINE cpl_prism_define( krcv, ksnd )
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_prism_define  ***
      !!
      !! ** Purpose :   Define grid and field information for ocean
      !!    exchange between AGCM, OGCM and COUPLER. (OASIS4 software)
      !!
      !! ** Method  :   OASIS4 MPI communication 
      !!--------------------------------------------------------------------
      INTEGER, INTENT(in) :: krcv, ksnd     ! Number of received and sent coupling fields
      !
      INTEGER, DIMENSION(4)      :: igrid     ! ids returned by prism_def_grid
      INTEGER, DIMENSION(4)      :: iptid     ! ids returned by prism_set_points
      INTEGER, DIMENSION(4)      :: imskid    ! ids returned by prism_set_mask
      INTEGER, DIMENSION(4)      :: iishift   ! 
      INTEGER, DIMENSION(4)      :: ijshift   ! 
      INTEGER, DIMENSION(4)      :: iioff     ! 
      INTEGER, DIMENSION(4)      :: ijoff     ! 
      INTEGER, DIMENSION(4)      :: itmp      ! 
      INTEGER, DIMENSION(1,3)    :: iextent   ! 
      INTEGER, DIMENSION(1,3)    :: ioffset   ! 

      INTEGER                    :: ishape(2,3)    ! shape of arrays passed to PSMILe
      INTEGER                    :: data_type      ! data type of transients

      LOGICAL                    :: new_points
      LOGICAL                    :: new_mask
      LOGICAL, ALLOCATABLE, SAVE :: llmask(:,:,:) ! jpi,jpj,1

      INTEGER                    :: ji, jj, jg, jc   ! local loop indicees
      INTEGER                    :: ii, ij           ! index
      INTEGER, DIMENSION(1)      :: ind              ! index

      CHARACTER(len=32)          :: clpt_name     ! name of the grid points
      CHARACTER(len=7)           :: cltxt 
      CHARACTER(len=1), DIMENSION(4) :: clgrd = (/ 'T','U','V','F' /)     ! name of the grid points

      TYPE(PRISM_Time_struct)    :: tmpdate
      INTEGER                    :: idate_incr      ! date increment
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zlon, zlat
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zclo, zcla
      !!--------------------------------------------------------------------
      
      CALL wrk_alloc( jpi,jpj, zlon, zlat )
      CALL wrk_alloc( jpi,jpj,jpk, zclo, zcla )

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'cpl_prism_define : initialization in coupled ocean/atmosphere case'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~~~~'
      IF(lwp) WRITE(numout,*)

      !
      ! ... Allocate memory for data exchange
      !
      ALLOCATE( exfld(nlei-nldi+1, nlej-nldj+1, 1), stat = nerror )
      IF ( nerror > 0 ) THEN
         CALL prism_abort( ncomp_id, 'cpl_prism_define', 'Failure in allocating exfld' )
         RETURN
      ENDIF

      IF(.not. ALLOCATED(mask))THEN
         ALLOCATE(llmask(jpi,jpj,1), Stat=ji)
         IF(ji /= 0)THEN
            CALL prism_abort( ncomp_id, 'cpl_prism_define', 'Failure in allocating llmask' )
            RETURN
         END IF
      END IF

      ! -----------------------------------------------------------------
      ! ... Define the shape of the valid region without the halo and overlaps between cpus
      !     For serial configuration (key_mpp_mpi not being active)
      !     nl* is set to the global values 1 and jp*glo.
      ! -----------------------------------------------------------------

      ishape(:,1) = (/ 1, nlei-nldi+1 /)
      ishape(:,2) = (/ 1, nlej-nldj+1 /)
      ishape(:,3) = (/ 1,           1 /)
          
      DO ji = 1, 4
         CALL prism_def_grid( igrid(ji), 'orca'//clgrd(ji), ncomp_id, ishape, PRISM_irrlonlat_regvrt, nerror )
         IF( nerror /= PRISM_Success )   CALL prism_abort (ncomp_id, 'cpl_prism_define',   &
            &                                                        'Failure in prism_def_grid of '//clgrd(jg)//'-point' )
      END DO
      
      ! -----------------------------------------------------------------
      ! ... Define the partition 
      ! -----------------------------------------------------------------
      
      iextent(1,:) = (/    nlei-nldi+1,    nlej-nldj+1, 1 /)
      ioffset(1,:) = (/ nldi-1+nimpp-1, nldj-1+njmpp-1, 0 /)
      
      DO ji = 1, 4
         CALL prism_def_partition( igrid(ji), 1, ioffset, iextent, nerror )
         IF( nerror /= PRISM_Success )   CALL prism_abort (ncomp_id, 'cpl_prism_define',   &
            &                                                        'Failure in prism_def_partition of '//clgrd(jg)//'-point' )
      END DO

      ! -----------------------------------------------------------------
      ! ... Define the elements, i.e. specify the corner points for each
      !     volume element. In case OPA runs on level coordinates (regular
      !     in the vertical) we only need to give the 4 horizontal corners
      !     for a volume element plus the vertical position of the upper
      !     and lower face. Nevertheless the volume element has 8 corners.
      ! -----------------------------------------------------------------
      
      iioff(:) = (/0,1,0,1/)
      ijoff(:) = (/0,0,1,1/) 
      iishift(:) = (/0,1,1,0/)
      ijshift(:) = (/0,0,1,1/)

      DO jg = 1, 4    ! ... the t,u,v,f-points

         cltxt = clgrd(jg)//'-point'
         
         ! -----------------------------------------------------------------
         ! ... Convert OPA masks to logicals and define the masks
         ! -----------------------------------------------------------------
         SELECT CASE( jg ) 
         CASE(1)   ;   llmask(:,:,1) = ( tmask(:,:,1)  ) == 1.
         CASE(2)   ;   llmask(:,:,1) = ( umask(:,:,1)  ) == 1.
         CASE(3)   ;   llmask(:,:,1) = ( vmask(:,:,1)  ) == 1.
         CASE(4)   ;   llmask(:,:,1) = ( fmask(:,:,1)  ) == 1.
!         CASE(1)   ;   llmask(:,:,1) = ( tmask(:,:,1) * dom_uniq('T') ) == 1.
!         CASE(2)   ;   llmask(:,:,1) = ( umask(:,:,1) * dom_uniq('U') ) == 1.
!         CASE(3)   ;   llmask(:,:,1) = ( vmask(:,:,1) * dom_uniq('V') ) == 1.
!         CASE(4)   ;   llmask(:,:,1) = ( fmask(:,:,1) * dom_uniq('F') ) == 1.
         END SELECT
         CALL prism_set_mask( imskid(jg), igrid(jg), ishape, llmask(nldi:nlei, nldj:nlej, 1), .TRUE., nerror )
         IF( nerror /= PRISM_Success )   CALL prism_abort( ncomp_id, 'cpl_prism_define', 'Failure in prism_set_mask for '//cltxt )

         ! -----------------------------------------------------------------
         ! ... Define the corners
         ! -----------------------------------------------------------------
         SELECT CASE( jg ) 
         CASE(1)   ;   zlon(:,:) = glamf(:,:)   ;   zlat(:,:) = gphif(:,:)
         CASE(2)   ;   zlon(:,:) = glamv(:,:)   ;   zlat(:,:) = gphiv(:,:)
         CASE(3)   ;   zlon(:,:) = glamu(:,:)   ;   zlat(:,:) = gphiu(:,:) 
         CASE(4)   ;   zlon(:,:) = glamt(:,:)   ;   zlat(:,:) = gphit(:,:)
         END SELECT

         DO jc = 1, 4   ! corner number (anti-clockwise, starting from the bottom left corner)
            DO jj = 2, jpjm1
               DO ji = 2, jpim1   ! NO vector opt.
                  ii = ji-1 + iioff(jg) + iishift(jc)
                  ij = jj-1 + ijoff(jg) + ijshift(jc)
                  zclo(ji,jj,jc) = zlon(ii,ij)
                  zcla(ji,jj,jc) = zlat(ii,ij)
               END DO
            END DO
            CALL lbc_lnk( zclo(:,:,jc), clgrd(jg), 1. )   ;   CALL lbc_lnk( zcla(:,:,jc), clgrd(jg), 1. )
         END DO

         CALL prism_set_corners( igrid(jg), 8, ishape, zclo(nldi:nlei, nldj:nlej,:),   &
            &                                          zcla(nldi:nlei, nldj:nlej,:), RESHAPE( (/-1.,1./), (/1,2/) ), nerror )
         IF( nerror /= PRISM_Success )   CALL prism_abort( ncomp_id, 'cpl_prism_define', 'Failure in prism_set_corners of '//cltxt )    

         ! -----------------------------------------------------------------
         ! ... Define the center points
         ! -----------------------------------------------------------------
         SELECT CASE( jg ) 
         CASE(1)   ;   zlon(:,:) = glamt(:,:)   ;   zlat(:,:) = gphit(:,:)
         CASE(2)   ;   zlon(:,:) = glamu(:,:)   ;   zlat(:,:) = gphiu(:,:)
         CASE(3)   ;   zlon(:,:) = glamv(:,:)   ;   zlat(:,:) = gphiv(:,:)
         CASE(4)   ;   zlon(:,:) = glamf(:,:)   ;   zlat(:,:) = gphif(:,:)
         END SELECT

         CALL prism_set_points ( iptid(jg), cltxt, igrid(jg), ishape, zlon(nldi:nlei, nldj:nlej),   &
         &                                                            zlat(nldi:nlei, nldj:nlej), (/0./), .TRUE., nerror )
         IF( nerror /= PRISM_Success )   CALL prism_abort ( ncomp_id, 'cpl_prism_define', 'Failure in prism_set_points '//cltxt )

      END DO

      ! ... Announce send variables. 
      !
      DO ji = 1, ksnd
         IF ( ssnd(ji)%laction ) THEN 
            
            itmp(:) = 0
            WHERE( clgrd == ssnd(ji)%clgrid  ) itmp = 1
            ind(:) = maxloc( itmp )
            WRITE(6,*) ' grid for field ', ind(1), ssnd(ji)%clname
             ind(1) = 1

            CALL prism_def_var( ssnd(ji)%nid, ssnd(ji)%clname, igrid(ind(1)), iptid(ind(1)),  imskid(ind(1)), (/ 3, 0/),   &
               &                ishape, PRISM_Double_Precision, nerror )
            IF ( nerror /= PRISM_Success )   CALL prism_abort( ssnd(ji)%nid, 'cpl_prism_define',   &
               &                                               'Failure in prism_def_var for '//TRIM(ssnd(ji)%clname))

         ENDIF
      END DO
      !
      ! ... Announce received variables. 
      !
      DO ji = 1, krcv
         IF ( srcv(ji)%laction ) THEN 

            itmp(:) = 0
            WHERE( clgrd == srcv(ji)%clgrid  ) itmp = 1
            ind(:) = maxloc( itmp )
            WRITE(6,*) ' grid for field ', ind(1), srcv(ji)%clname
             ind(1) = 1
 
            CALL prism_def_var( srcv(ji)%nid, srcv(ji)%clname, igrid(ind(1)), iptid(ind(1)), imskid(ind(1)), (/ 3, 0/),   &
               &                ishape, PRISM_Double_Precision, nerror )
            IF ( nerror /= PRISM_Success )   CALL prism_abort( srcv(ji)%nid, 'cpl_prism_define',   &
               &                                               'Failure in prism_def_var for '//TRIM(srcv(ji)%clname))

         ENDIF
      END DO
      
      !------------------------------------------------------------------
      ! End of definition phase
      !------------------------------------------------------------------
      
      CALL prism_enddef( nerror )
      IF ( nerror /= PRISM_Success )   CALL prism_abort ( ncomp_id, 'cpl_prism_define', 'Failure in prism_enddef')
      
      CALL wrk_dealloc( jpi,jpj, zlon, zlat )
      CALL wrk_dealloc( jpi,jpj,jpk, zclo, zcla )
      !
   END SUBROUTINE cpl_prism_define
   
   
   SUBROUTINE cpl_prism_snd( kid, kstep, pdata, kinfo )
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_prism_snd  ***
      !!
      !! ** Purpose : - At each coupling time-step,this routine sends fields
      !!      like sst or ice cover to the coupler or remote application.
      !!----------------------------------------------------------------------
      INTEGER                 , INTENT(in   ) ::   kid     ! variable intex in the array
      INTEGER                 , INTENT(  out) ::   kinfo   ! OASIS4 info argument
      INTEGER                 , INTENT(in   ) ::   kstep   ! ocean time-step in seconds
      REAL(wp), DIMENSION(:,:), INTENT(in   ) ::   pdata
      !!--------------------------------------------------------------------
      !
      ! snd data to OASIS4
      !
      exfld(:,:,1) = pdata(nldi:nlei, nldj:nlej)
      CALL prism_put( ssnd(kid)%nid, date, date_bound, exfld, kinfo, nerror )
      IF ( nerror /= PRISM_Success )   CALL prism_abort( ssnd(kid)%nid, 'cpl_prism_snd',   &
         &                                               'Failure in prism_put for '//TRIM(ssnd(kid)%clname) )

      IF( ln_ctl ) THEN        
         IF ( kinfo >= PRISM_Cpl     .OR. kinfo == PRISM_Rst .OR.   &
            & kinfo == PRISM_RstTimeop ) THEN
            WRITE(numout,*) '****************'
            WRITE(numout,*) 'prism_put: Outgoing ', ssnd(kid)%clname
            WRITE(numout,*) 'prism_put: ivarid ', ssnd(kid)%nid
            WRITE(numout,*) 'prism_put:  kstep ', kstep
            WRITE(numout,*) 'prism_put:   info ', kinfo
            WRITE(numout,*) '     - Minimum value is ', MINVAL(pdata)
            WRITE(numout,*) '     - Maximum value is ', MAXVAL(pdata)
            WRITE(numout,*) '     -     Sum value is ', SUM(pdata)
            WRITE(numout,*) '****************'
         ENDIF
      ENDIF
      !
   END SUBROUTINE cpl_prism_snd


   SUBROUTINE cpl_prism_rcv( kid, kstep, pdata, kinfo )
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_prism_rcv  ***
      !!
      !! ** Purpose : - At each coupling time-step,this routine receives fields
      !!      like stresses and fluxes from the coupler or remote application.
      !!----------------------------------------------------------------------
      INTEGER                 , INTENT(in   ) ::   kid     ! variable intex in the array
      INTEGER                 , INTENT(in   ) ::   kstep   ! ocean time-step in seconds
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   pdata   ! IN to keep the value if nothing is done
      INTEGER                 , INTENT(  out) ::   kinfo   ! OASIS4 info argument
      !
      LOGICAL                :: llaction
      !!--------------------------------------------------------------------
      !
      ! receive local data from OASIS4 on every process
      !
      CALL prism_get( srcv(kid)%nid, date, date_bound, exfld, kinfo, nerror )         
      IF ( nerror /= PRISM_Success )   CALL prism_abort( srcv(kid)%nid, 'cpl_prism_rcv',   &
         &                                               'Failure in prism_get for '//TRIM(srcv(kid)%clname) )

      WRITE(numout,*) 'prism_get: Incoming ', srcv(kid)%clname
      call flush(numout)
      llaction = .false.
      IF( kinfo == PRISM_Cpl )  llaction = .TRUE.

      IF ( ln_ctl )   WRITE(numout,*) "llaction, kinfo, kstep, ivarid: " , llaction, kinfo, kstep, srcv(kid)%nid

      IF ( llaction ) THEN

         kinfo = OASIS_Rcv
         pdata(nldi:nlei, nldj:nlej) = exfld(:,:,1)
         
         !--- Fill the overlap areas and extra hallows (mpp)
         !--- check periodicity conditions (all cases)
         CALL lbc_lnk( pdata, srcv(kid)%clgrid, srcv(kid)%nsgn )   
         
         IF ( ln_ctl ) THEN        
            WRITE(numout,*) '****************'
            WRITE(numout,*) 'prism_get: Incoming ', srcv(kid)%clname
            WRITE(numout,*) 'prism_get: ivarid '  , srcv(kid)%nid
            WRITE(numout,*) 'prism_get:   kstep', kstep
            WRITE(numout,*) 'prism_get:   info ', kinfo
            WRITE(numout,*) '     - Minimum value is ', MINVAL(pdata)
            WRITE(numout,*) '     - Maximum value is ', MAXVAL(pdata)
            WRITE(numout,*) '     -     Sum value is ', SUM(pdata)
            WRITE(numout,*) '****************'
         ENDIF

      ELSE
         kinfo = OASIS_idle     
      ENDIF
      !
   END SUBROUTINE cpl_prism_rcv


   SUBROUTINE cpl_prism_finalize
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_prism_finalize  ***
      !!
      !! ** Purpose : - Finalizes the coupling. If MPI_init has not been
      !!      called explicitly before cpl_prism_init it will also close
      !!      MPI communication.
      !!----------------------------------------------------------------------
      !
      DEALLOCATE(exfld)
      CALL prism_terminate ( nerror )         
      !
   END SUBROUTINE cpl_prism_finalize


   SUBROUTINE cpl_prism_update_time(kt)
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_prism_update_time  ***
      !!
      !! ** Purpose : - Increment date with model timestep
      !!                called explicitly at the end of each timestep
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean model time step index

      TYPE(PRISM_Time_struct) ::   tmpdate
      INTEGER                 ::   idate_incr   ! date increment
      !!----------------------------------------------------------------------

      IF( kt == nit000 ) THEN      ! Define the actual date 
         !
         ! date is determined by adding days since beginning of the run to the corresponding initial date.
         ! Note that OPA internal info about the start date of the experiment is bypassed.
         ! Instead we rely sololy on the info provided by the SCC.xml file.
         !
         date = PRISM_Jobstart_date
         !
         !
         ! lower/upper bound is determined by adding half a time step
         !
         idate_incr = 0.5 * NINT ( rdttra(1) )
         tmpdate = date   ;   CALL PRISM_calc_newdate ( tmpdate, -idate_incr, nerror )   ;   date_bound(1) = tmpdate
         tmpdate = date   ;   CALL PRISM_calc_newdate ( tmpdate,  idate_incr, nerror )   ;   date_bound(2) = tmpdate
         !
      ELSE      ! Date update
         !
         idate_incr  = rdttra(1)
         CALL PRISM_calc_newdate( date, idate_incr, nerror )
         date_bound(1) = date_bound(2)
         tmpdate = date_bound(2)
         CALL PRISM_calc_newdate( tmpdate, idate_incr, nerror )
         date_bound(2) = tmpdate
         !
      END IF
      !
   END SUBROUTINE cpl_prism_update_time

#endif

   !!=====================================================================
END MODULE cpl_oasis4
