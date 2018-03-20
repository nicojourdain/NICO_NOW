MODULE diadct
  !!=====================================================================
  !!                       ***  MODULE  diadct  ***
  !! Ocean diagnostics: Compute the transport trough a sec.
  !!===============================================================
  !! History : 
  !!
  !!         original  : 02/99 (Y Drillet)
  !!         addition  : 10/01 (Y Drillet, R Bourdalle Badie)
  !!                   : 10/05 (M Laborie) F90
  !!         addition  : 04/07 (G Garric) Ice sections
  !!         bugfix    : 04/07 (C Bricaud) test on sec%nb_point
  !!                                      initialisation of ztransp1,ztransp2,...
  !!         nemo_v_3_4: 09/2011 (C Bricaud)
  !!
  !!
  !!----------------------------------------------------------------------
#if defined key_diadct
  !!----------------------------------------------------------------------
  !!   'key_diadct' :
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   dia_dct      :  compute the transport through a sec.
  !!   dia_dct_init :  read namelist.
  !!   readsec      :  read sections description and pathway
  !!   removepoints :  remove points which are common to 2 procs
  !!   transport    :  Compute transport for each sections
  !!   dia_dct_wri  :  write tranports results in ascii files
  !!   interp       :  compute Temperature/Salinity/density on U-point or V-point
  !!   
  !!----------------------------------------------------------------------
  !! * Modules used
  USE oce             ! ocean dynamics and tracers
  USE dom_oce         ! ocean space and time domain
  USE phycst          ! physical constants
  USE in_out_manager  ! I/O manager
  USE daymod          ! calendar
  USE dianam          ! build name of file
  USE lib_mpp         ! distributed memory computing library
#if defined key_lim2
  USE ice_2
#endif
#if defined key_lim3
  USE ice_3
#endif
  USE domvvl
  USE timing          ! preformance summary
  USE wrk_nemo        ! working arrays

  IMPLICIT NONE
  PRIVATE

  !! * Routine accessibility
  PUBLIC   dia_dct     ! routine called by step.F90
  PUBLIC   dia_dct_init! routine called by opa.F90
  PRIVATE  readsec
  PRIVATE  removepoints
  PRIVATE  transport
  PRIVATE  dia_dct_wri

#include "domzgr_substitute.h90"

  !! * Shared module variables
  LOGICAL, PUBLIC, PARAMETER ::   lk_diadct = .TRUE.   !: model-data diagnostics flag

  !! * Module variables
  INTEGER :: nn_dct      = 1     ! Frequency of computation
  INTEGER :: nn_dctwri   = 1     ! Frequency of output
  INTEGER :: nn_secdebug = 0     ! Number of the section to debug
   
  INTEGER, PARAMETER :: nb_class_max  = 10
  INTEGER, PARAMETER :: nb_sec_max    = 150
  INTEGER, PARAMETER :: nb_point_max  = 2000
  INTEGER, PARAMETER :: nb_type_class = 14
  INTEGER            :: nb_sec 

  TYPE POINT_SECTION
     INTEGER :: I,J
  END TYPE POINT_SECTION

  TYPE COORD_SECTION
     REAL(wp) :: lon,lat
  END TYPE COORD_SECTION

  TYPE SECTION
     CHARACTER(len=60)                            :: name              ! name of the sec
     LOGICAL                                      :: llstrpond         ! true if you want the computation of salt and
                                                                       ! heat transports
     LOGICAL                                      :: ll_ice_section    ! ice surface and ice volume computation
     LOGICAL                                      :: ll_date_line      ! = T if the section crosses the date-line
     TYPE(COORD_SECTION), DIMENSION(2)            :: coordSec          ! longitude and latitude of the extremities of the sec
     INTEGER                                      :: nb_class          ! number of boundaries for density classes
     INTEGER, DIMENSION(nb_point_max)             :: direction         ! vector direction of the point in the section
     CHARACTER(len=40),DIMENSION(nb_class_max)    :: classname         ! caracteristics of the class
     REAL(wp), DIMENSION(nb_class_max)            :: zsigi           ,&! in-situ   density classes    (99 if you don't want)
                                                     zsigp           ,&! potential density classes    (99 if you don't want)
                                                     zsal            ,&! salinity classes   (99 if you don't want)
                                                     ztem            ,&! temperature classes(99 if you don't want)
                                                     zlay              ! level classes      (99 if you don't want)
     REAL(wp), DIMENSION(nb_type_class,nb_class_max)  :: transport     ! transport output
     REAL(wp)                                         :: slopeSection  ! slope of the section
     INTEGER                                          :: nb_point      ! number of points in the section
     TYPE(POINT_SECTION),DIMENSION(nb_point_max)      :: listPoint     ! list of points in the sections
  END TYPE SECTION

  TYPE(SECTION),DIMENSION(nb_sec_max) :: secs ! Array of sections
 
 
CONTAINS

  SUBROUTINE dia_dct_init
     !!---------------------------------------------------------------------
     !!               ***  ROUTINE diadct  ***  
     !!
     !!  ** Purpose: Read the namelist parametres
     !!              Open output files
     !!
     !!---------------------------------------------------------------------
     NAMELIST/namdct/nn_dct,nn_dctwri,nn_secdebug

     IF( nn_timing == 1 )   CALL timing_start('dia_dct_init')

     !read namelist
     REWIND( numnam )
     READ  ( numnam, namdct )

     IF( lwp ) THEN
        WRITE(numout,*) " "
        WRITE(numout,*) "diadct_init: compute transports through sections "
        WRITE(numout,*) "~~~~~~~~~~~~~~~~~~~~~"
        WRITE(numout,*) "       Frequency of computation: nn_dct    = ",nn_dct
        WRITE(numout,*) "       Frequency of write:       nn_dctwri = ",nn_dctwri

        IF      ( nn_secdebug .GE. 1 .AND. nn_secdebug .LE. nb_sec_max )THEN
                                            WRITE(numout,*)"       Debug section number: ", nn_secdebug 
        ELSE IF ( nn_secdebug ==  0 )THEN ; WRITE(numout,*)"       No section to debug"
        ELSE IF ( nn_secdebug == -1 )THEN ; WRITE(numout,*)"       Debug all sections"
        ELSE                              ; WRITE(numout,*)"       Wrong value for nn_secdebug : ",nn_secdebug
        ENDIF

        IF(nn_dct .GE. nn_dctwri .AND. MOD(nn_dct,nn_dctwri) .NE. 0)  &
          &  CALL ctl_stop( 'diadct: nn_dct should be smaller and a multiple of nn_dctwri' )

     ENDIF

     !Read section_ijglobal.diadct
     CALL readsec

     !open output file
     IF( lwp ) THEN
        CALL ctl_opn( numdct_vol,  'volume_transport', 'NEW', 'FORMATTED', 'SEQUENTIAL', -1, numout,  .FALSE. )
        CALL ctl_opn( numdct_heat, 'heat_transport'  , 'NEW', 'FORMATTED', 'SEQUENTIAL', -1, numout,  .FALSE. )
        CALL ctl_opn( numdct_salt, 'salt_transport'  , 'NEW', 'FORMATTED', 'SEQUENTIAL', -1, numout,  .FALSE. )
     ENDIF

     IF( nn_timing == 1 )   CALL timing_stop('dia_dct_init')
     !
  END SUBROUTINE dia_dct_init
 
 
  SUBROUTINE dia_dct(kt)
     !!---------------------------------------------------------------------
     !!               ***  ROUTINE diadct  ***  
     !!
     !!  ** Purpose: Compute sections tranport and write it in numdct file
     !!---------------------------------------------------------------------
     !! * Arguments
     INTEGER,INTENT(IN)        ::kt

     !! * Local variables
     INTEGER             :: jsec,            &! loop on sections
                            iost,            &! error for opening fileout
                            itotal            ! nb_sec_max*nb_type_class*nb_class_max
     LOGICAL             :: lldebug =.FALSE.  ! debug a section  
     CHARACTER(len=160)  :: clfileout         ! fileout name

     
     INTEGER , DIMENSION(1)             :: ish   ! tmp array for mpp_sum
     INTEGER , DIMENSION(3)             :: ish2  !   "
     REAL(wp), POINTER, DIMENSION(:)    :: zwork !   "  
     REAL(wp), POINTER, DIMENSION(:,:,:):: zsum  !   "

     !!---------------------------------------------------------------------    
     IF( nn_timing == 1 )   CALL timing_start('dia_dct')

     IF( lk_mpp )THEN
        itotal = nb_sec_max*nb_type_class*nb_class_max
        CALL wrk_alloc( itotal                                , zwork ) 
        CALL wrk_alloc( nb_sec_max,nb_type_class,nb_class_max , zsum  )
     ENDIF    
 
     IF( lwp .AND. kt==nit000+nn_dct-1 ) THEN
         WRITE(numout,*) " "
         WRITE(numout,*) "diadct: compute transport"
         WRITE(numout,*) "~~~~~~~~~~~~~~~~~~~~~~~~~"
         WRITE(numout,*) "nb_sec = ",nb_sec
     ENDIF

 
     ! Compute transport and write only at nn_dctwri
     IF( MOD(kt,nn_dct)==0 ) THEN 

        DO jsec=1,nb_sec

           !debug this section computing ?
           lldebug=.FALSE.
           IF( (jsec==nn_secdebug .OR. nn_secdebug==-1) .AND.  kt==nit000+nn_dct-1 .AND. lwp ) lldebug=.TRUE. 

           !Compute transport through section  
           CALL transport(secs(jsec),lldebug) 

        ENDDO
             
        IF( MOD(kt,nn_dctwri)==0 )THEN

           IF( lwp .AND. kt==nit000+nn_dctwri-1 )WRITE(numout,*)"      diadct: write at kt = ",kt         
  
           !Sum on all procs 
           IF( lk_mpp )THEN
              ish(1)  =  nb_sec_max*nb_type_class*nb_class_max 
              ish2    = (/nb_sec_max,nb_type_class,nb_class_max/)
              DO jsec=1,nb_sec ; zsum(jsec,:,:) = secs(jsec)%transport(:,:) ; ENDDO
              zwork(:)= RESHAPE(zsum(:,:,:), ish )
              CALL mpp_sum(zwork, ish(1))
              zsum(:,:,:)= RESHAPE(zwork,ish2)
              DO jsec=1,nb_sec ; secs(jsec)%transport(:,:) = zsum(jsec,:,:) ; ENDDO
           ENDIF

           !Write the transport
           DO jsec=1,nb_sec

              IF( lwp )CALL dia_dct_wri(kt,jsec,secs(jsec))
            
              !nullify transports values after writing
              secs(jsec)%transport(:,:)=0.  

           ENDDO

        ENDIF 

     ENDIF

     IF( lk_mpp )THEN
        itotal = nb_sec_max*nb_type_class*nb_class_max
        CALL wrk_dealloc( itotal                                , zwork ) 
        CALL wrk_dealloc( nb_sec_max,nb_type_class,nb_class_max , zsum  )
     ENDIF    

     IF( nn_timing == 1 )   CALL timing_stop('dia_dct')
     !
  END SUBROUTINE dia_dct

  SUBROUTINE readsec 
     !!---------------------------------------------------------------------
     !!               ***  ROUTINE readsec  ***
     !!
     !!  ** Purpose:
     !!            Read a binary file(section_ijglobal.diadct) 
     !!            generated by the tools "NEMOGCM/TOOLS/SECTIONS_DIADCT"
     !!
     !!
     !!---------------------------------------------------------------------
     !! * Local variables
     INTEGER :: iptglo , iptloc                               ! Global and local number of points for a section
     INTEGER :: isec, iiglo, ijglo, iiloc, ijloc,iost,i1 ,i2  ! temporary  integer
     INTEGER :: jsec, jpt                                     ! dummy loop indices
                                                              ! heat/salt tranport is actived

     INTEGER, DIMENSION(2) :: icoord 
     CHARACTER(len=160)    :: clname                          !filename
     CHARACTER(len=200)    :: cltmp
     CHARACTER(len=200)    :: clformat                        !automatic format
     TYPE(POINT_SECTION),DIMENSION(nb_point_max)  ::coordtemp !contains listpoints coordinates 
                                                              !read in the file
     INTEGER, POINTER, DIMENSION(:) :: directemp              !contains listpoints directions
                                                              !read in the files
     LOGICAL :: llbon                                       ,&!local logical
                lldebug                                       !debug the section
     !!-------------------------------------------------------------------------------------
     CALL wrk_alloc( nb_point_max, directemp )

     !open input file
     !---------------
     CALL ctl_opn( numdct_in, 'section_ijglobal.diadct', 'OLD', 'UNFORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
 
     !---------------
     !Read input file
     !---------------
     
     DO jsec=1,nb_sec_max      !loop on the nb_sec sections

        IF ( lwp .AND. ( jsec==nn_secdebug .OR. nn_secdebug==-1 ) ) &
           & WRITE(numout,*)'debuging for section number: ',jsec 

        !initialization
        !---------------
        secs(jsec)%name=''
        secs(jsec)%llstrpond    = .FALSE.  ; secs(jsec)%ll_ice_section = .FALSE.
        secs(jsec)%ll_date_line = .FALSE.  ; secs(jsec)%nb_class       = 0
        secs(jsec)%zsigi        = 99._wp   ; secs(jsec)%zsigp          = 99._wp
        secs(jsec)%zsal         = 99._wp   ; secs(jsec)%ztem           = 99._wp
        secs(jsec)%zlay         = 99._wp         
        secs(jsec)%transport    =  0._wp   ; secs(jsec)%nb_point       = 0

        !read section's number / name / computing choices / classes / slopeSection / points number
        !-----------------------------------------------------------------------------------------
        READ(numdct_in,iostat=iost)isec
        IF (iost .NE. 0 )EXIT !end of file 
        WRITE(cltmp,'(a,i4.4,a,i4.4)')'diadct: read sections : Problem of section number: isec= ',isec,' and jsec= ',jsec
        IF( jsec .NE. isec )  CALL ctl_stop( cltmp )

        IF( lwp .AND. ( jsec==nn_secdebug .OR. nn_secdebug==-1 ) )WRITE(numout,*)"isec ",isec 

        READ(numdct_in)secs(jsec)%name
        READ(numdct_in)secs(jsec)%llstrpond
        READ(numdct_in)secs(jsec)%ll_ice_section
        READ(numdct_in)secs(jsec)%ll_date_line
        READ(numdct_in)secs(jsec)%coordSec
        READ(numdct_in)secs(jsec)%nb_class
        READ(numdct_in)secs(jsec)%zsigi
        READ(numdct_in)secs(jsec)%zsigp
        READ(numdct_in)secs(jsec)%zsal
        READ(numdct_in)secs(jsec)%ztem
        READ(numdct_in)secs(jsec)%zlay
        READ(numdct_in)secs(jsec)%slopeSection
        READ(numdct_in)iptglo

        !debug
        !-----

        IF( lwp .AND. ( jsec==nn_secdebug .OR. nn_secdebug==-1 ) )THEN
          
            WRITE(clformat,'(a,i2,a)') '(A40,', nb_class_max,'(f8.3,1X))' 

            WRITE(numout,*)       "   Section name :                       ",TRIM(secs(jsec)%name)
            WRITE(numout,*)       "      Compute heat and salt transport ? ",secs(jsec)%llstrpond
            WRITE(numout,*)       "      Compute ice transport ?           ",secs(jsec)%ll_ice_section
            WRITE(numout,*)       "      Section crosses date-line ?       ",secs(jsec)%ll_date_line
            WRITE(numout,*)       "      Slope section :                   ",secs(jsec)%slopeSection
            WRITE(numout,*)       "      Number of points in the section:  ",iptglo
            WRITE(numout,*)       "      Number of classes                 ",secs(jsec)%nb_class
            WRITE(numout,clformat)"      Insitu density classes :          ",secs(jsec)%zsigi
            WRITE(numout,clformat)"      Potential density classes :       ",secs(jsec)%zsigp
            WRITE(numout,clformat)"      Salinity classes :                ",secs(jsec)%zsal
            WRITE(numout,clformat)"      Temperature classes :             ",secs(jsec)%ztem
            WRITE(numout,clformat)"      Depth classes :                   ",secs(jsec)%zlay
        ENDIF               

        IF( iptglo .NE. 0 )THEN
             
           !read points'coordinates and directions 
           !--------------------------------------
           coordtemp(:) = POINT_SECTION(0,0) !list of points read
           directemp(:) = 0                  !value of directions of each points
           DO jpt=1,iptglo
              READ(numdct_in)i1,i2
              coordtemp(jpt)%I = i1 
              coordtemp(jpt)%J = i2
           ENDDO
           READ(numdct_in)directemp(1:iptglo)
    
           !debug
           !-----
           IF( lwp .AND. ( jsec==nn_secdebug .OR. nn_secdebug==-1 ) )THEN
              WRITE(numout,*)"      List of points in global domain:"
              DO jpt=1,iptglo
                 WRITE(numout,*)'        # I J ',jpt,coordtemp(jpt),directemp(jpt)
              ENDDO                  
           ENDIF
 
           !Now each proc selects only points that are in its domain:
           !--------------------------------------------------------
           iptloc = 0                    !initialize number of points selected
           DO jpt=1,iptglo               !loop on listpoint read in the file
                    
              iiglo=coordtemp(jpt)%I          ! global coordinates of the point
              ijglo=coordtemp(jpt)%J          !  " 

              IF( iiglo==jpidta .AND. nimpp==1 ) iiglo = 2

              iiloc=iiglo-jpizoom+1-nimpp+1   ! local coordinates of the point
              ijloc=ijglo-jpjzoom+1-njmpp+1   !  "

              !verify if the point is on the local domain:(1,nlei)*(1,nlej)
              IF( iiloc .GE. 1 .AND. iiloc .LE. nlei .AND. &
                  ijloc .GE. 1 .AND. ijloc .LE. nlej       )THEN
                 iptloc = iptloc + 1                                                 ! count local points
                 secs(jsec)%listPoint(iptloc) = POINT_SECTION(mi0(iiglo),mj0(ijglo)) ! store local coordinates
                 secs(jsec)%direction(iptloc) = directemp(jpt)                       ! store local direction
              ENDIF

           ENDDO
     
           secs(jsec)%nb_point=iptloc !store number of section's points

           !debug
           !-----
           IF(   lwp .AND. ( jsec==nn_secdebug .OR. nn_secdebug==-1 ) )THEN
              WRITE(numout,*)"      List of points selected by the proc:"
              DO jpt = 1,iptloc
                 iiglo = secs(jsec)%listPoint(jpt)%I + jpizoom - 1 + nimpp - 1
                 ijglo = secs(jsec)%listPoint(jpt)%J + jpjzoom - 1 + njmpp - 1
                 WRITE(numout,*)'         # I J : ',iiglo,ijglo
              ENDDO
           ENDIF

              IF(jsec==nn_secdebug .AND. secs(jsec)%nb_point .NE. 0)THEN
              DO jpt = 1,iptloc
                 iiglo = secs(jsec)%listPoint(jpt)%I + jpizoom - 1 + nimpp - 1
                 ijglo = secs(jsec)%listPoint(jpt)%J + jpjzoom - 1 + njmpp - 1
              ENDDO
              ENDIF

           !remove redundant points between processors
           !------------------------------------------
           lldebug = .FALSE. ; IF ( (jsec==nn_secdebug .OR. nn_secdebug==-1) .AND. lwp ) lldebug = .TRUE.
           IF( iptloc .NE. 0 )THEN
              CALL removepoints(secs(jsec),'I','top_list',lldebug)
              CALL removepoints(secs(jsec),'I','bot_list',lldebug)
              CALL removepoints(secs(jsec),'J','top_list',lldebug)
              CALL removepoints(secs(jsec),'J','bot_list',lldebug)
           ENDIF
           IF(jsec==nn_secdebug .AND. secs(jsec)%nb_point .NE. 0)THEN
              DO jpt = 1,secs(jsec)%nb_point
                 iiglo = secs(jsec)%listPoint(jpt)%I + jpizoom - 1 + nimpp - 1
                 ijglo = secs(jsec)%listPoint(jpt)%J + jpjzoom - 1 + njmpp - 1
              ENDDO
           ENDIF

           !debug
           !-----
           IF( lwp .AND. ( jsec==nn_secdebug .OR. nn_secdebug==-1 ) )THEN
              WRITE(numout,*)"      List of points after removepoints:"
              iptloc = secs(jsec)%nb_point
              DO jpt = 1,iptloc
                 iiglo = secs(jsec)%listPoint(jpt)%I + jpizoom - 1 + nimpp - 1
                 ijglo = secs(jsec)%listPoint(jpt)%J + jpjzoom - 1 + njmpp - 1
                 WRITE(numout,*)'         # I J : ',iiglo,ijglo
              ENDDO
           ENDIF

        ELSE  ! iptglo = 0
           IF( lwp .AND. ( jsec==nn_secdebug .OR. nn_secdebug==-1 ) )&
              WRITE(numout,*)'   No points for this section.'
        ENDIF

     ENDDO !end of the loop on jsec
 
     nb_sec = jsec-1   !number of section read in the file

     CALL wrk_dealloc( nb_point_max, directemp )
     !
  END SUBROUTINE readsec

  SUBROUTINE removepoints(sec,cdind,cdextr,ld_debug)
     !!---------------------------------------------------------------------------
     !!             *** function removepoints
     !!
     !!   ** Purpose ::
     !!              remove points which are common to 2 procs
     !!
     !!
     !----------------------------------------------------------------------------
     !! * arguments
     TYPE(SECTION),INTENT(INOUT) :: sec
     CHARACTER(len=1),INTENT(IN) :: cdind   ! = 'I'/'J'
     CHARACTER(len=8),INTENT(IN) :: cdextr  ! = 'top_list'/'bot_list'
     LOGICAL,INTENT(IN)          :: ld_debug                     

     !! * Local variables
     INTEGER :: iextr         ,& !extremity of listpoint that we verify
                iind          ,& !coord     of listpoint that we verify
                itest         ,& !indice value of the side of the domain 
                                 !where points could be redundant
                isgn          ,& ! isgn= 1 : scan listpoint from start to end
                                 ! isgn=-1 : scan listpoint from end to start 
                istart,iend      !first and last points selected in listpoint
     INTEGER :: jpoint           !loop on list points
     INTEGER, POINTER, DIMENSION(:)   :: idirec !contains temporary sec%direction
     INTEGER, POINTER, DIMENSION(:,:) :: icoord !contains temporary sec%listpoint
     !----------------------------------------------------------------------------
     CALL wrk_alloc(    nb_point_max, idirec )
     CALL wrk_alloc( 2, nb_point_max, icoord )

     IF( ld_debug )WRITE(numout,*)'      -------------------------'
     IF( ld_debug )WRITE(numout,*)'      removepoints in listpoint'

     !iextr=extremity of list_point that we verify
     IF      ( cdextr=='bot_list' )THEN ; iextr=1            ; isgn=1
     ELSE IF ( cdextr=='top_list' )THEN ; iextr=sec%nb_point ; isgn=-1
     ELSE    ; CALL ctl_stop("removepoints :Wrong value for cdextr")
     ENDIF
 
     !which coordinate shall we verify ?
     IF      ( cdind=='I' )THEN   ; itest=nlei ; iind=1
     ELSE IF ( cdind=='J' )THEN   ; itest=nlej ; iind=2
     ELSE    ; CALL ctl_stop("removepoints :Wrong value for cdind") 
     ENDIF

     IF( ld_debug )THEN
        WRITE(numout,*)'      case: coord/list extr/domain side'
        WRITE(numout,*)'      ', cdind,' ',cdextr,' ',itest
        WRITE(numout,*)'      Actual number of points: ',sec%nb_point
     ENDIF

     icoord(1,1:nb_point_max) = sec%listPoint%I
     icoord(2,1:nb_point_max) = sec%listPoint%J
     idirec                   = sec%direction
     sec%listPoint            = POINT_SECTION(0,0)
     sec%direction            = 0

     jpoint=iextr+isgn
     DO WHILE( jpoint .GE. 1 .AND. jpoint .LE. sec%nb_point )
         IF( icoord( iind,jpoint-isgn ) == itest .AND. icoord( iind,jpoint ) == itest )THEN ; jpoint=jpoint+isgn
         ELSE                                                                               ; EXIT
         ENDIF
     ENDDO 

     IF( cdextr=='bot_list')THEN ; istart=jpoint-1 ; iend=sec%nb_point
     ELSE                        ; istart=1        ; iend=jpoint+1
     ENDIF

     sec%listPoint(1:1+iend-istart)%I = icoord(1,istart:iend)
     sec%listPoint(1:1+iend-istart)%J = icoord(2,istart:iend)
     sec%direction(1:1+iend-istart)   = idirec(istart:iend)
     sec%nb_point                     = iend-istart+1
     
     IF( ld_debug )THEN
        WRITE(numout,*)'      Number of points after removepoints :',sec%nb_point
        WRITE(numout,*)'      sec%direction after removepoints :',sec%direction(1:sec%nb_point)
     ENDIF

     CALL wrk_dealloc(    nb_point_max, idirec )
     CALL wrk_dealloc( 2, nb_point_max, icoord )
  END SUBROUTINE removepoints

  SUBROUTINE transport(sec,ld_debug)
     !!-------------------------------------------------------------------------------------------
     !!                     ***  ROUTINE transport  ***
     !!
     !!  ** Purpose : Compute the transport through a section
     !!
     !!  ** Method  :Transport through a given section is equal to the sum of transports
     !!              computed on each proc.
     !!              On each proc,transport is equal to the sum of transport computed through
     !!	             segments linking each point of sec%listPoint  with the next one.   
     !!
     !!              !BE carefull :          
     !!              one section is a sum of segments
     !!              one segment is defined by 2 consectuve points in sec%listPoint
     !!              all points of sec%listPoint are positioned on the F-point of the cell. 
     !! 
     !!              There are several loops:                 
     !!              loop on the density/temperature/salinity/level classes
     !!              loop on the segment between 2 nodes
     !!              loop on the level jk
     !!              test on the density/temperature/salinity/level
     !!
     !! ** Output: sec%transport: volume/mass/ice/heat/salt transport in the 2 directions
     !!
     !!
     !!-------------------------------------------------------------------------------------------
     !! * Arguments
     TYPE(SECTION),INTENT(INOUT) :: sec
     LOGICAL      ,INTENT(IN)    :: ld_debug
    
     !! * Local variables
     INTEGER             :: jk,jseg,jclass,   &!loop on level/segment/classes 
                            isgnu  , isgnv     !
     INTEGER :: ii, ij ! local integer
     REAL(wp):: zumid        , zvmid        ,&!U/V velocity on a cell segment
                zumid_ice    , zvmid_ice    ,&!U/V ice velocity
                zTnorm                      ,&!transport of velocity through one cell's sides
                ztransp1     , ztransp2     ,&!total        transport in directions 1 and 2
                ztemp1       , ztemp2       ,&!temperature  transport     "
                zrhoi1       , zrhoi2       ,&!mass         transport     "
                zrhop1       , zrhop2       ,&!mass         transport     "
                zsal1        , zsal2        ,&!salinity     transport     "
                zice_vol_pos , zice_vol_neg ,&!volume  ice  transport     "
                zice_surf_pos, zice_surf_neg  !surface ice  transport     "
     REAL(wp):: ztn, zsn, zrhoi, zrhop, zsshn, zfsdep ! temperature/salinity/ssh/potential density /depth at u/v point

     TYPE(POINT_SECTION) :: k
     REAL(wp), POINTER, DIMENSION(:,:):: zsum ! 2D work array
     !!--------------------------------------------------------
     CALL wrk_alloc( nb_type_class , nb_class_max , zsum   )

     IF( ld_debug )WRITE(numout,*)'      Compute transport'

     !----------------!
     ! INITIALIZATION !
     !----------------!
     zsum    = 0._wp
     zice_surf_neg = 0._wp ; zice_surf_pos = 0._wp
     zice_vol_pos  = 0._wp ; zice_vol_neg  = 0._wp

     !---------------------------!
     !  COMPUTE TRANSPORT        !
     !---------------------------!
     IF(sec%nb_point .NE. 0)THEN   

        !----------------------------------------------------------------------------------------------------
        !Compute sign for velocities:
        !
        !convention:
        !   non horizontal section: direction + is toward left hand of section
        !       horizontal section: direction + is toward north of section
        !
        !
        !       slopeSection < 0     slopeSection > 0       slopeSection=inf            slopeSection=0
        !       ----------------      -----------------     ---------------             --------------
        !
        !   isgnv=1         direction +      
        !  ______         _____             ______                                                   
        !        |           //|            |                  |                         direction +   
        !        | isgnu=1  // |            |isgnu=1           |isgnu=1                     /|\
        !        |_______  //         ______|    \\            | ---\                        |
        !               |             | isgnv=-1  \\ |         | ---/ direction +       ____________
        !               |             |          __\\|         |                    
        !               |             |     direction +        |                      isgnv=1                                 
        !                                                      
        !----------------------------------------------------------------------------------------------------
        isgnu = 1
        IF( sec%slopeSection .GT. 0 ) THEN  ; isgnv = -1 
        ELSE                                ; isgnv =  1
        ENDIF
        IF( sec%slopeSection .GE. 9999. )     isgnv =  1

        IF( ld_debug )write(numout,*)"sec%slopeSection isgnu isgnv ",sec%slopeSection,isgnu,isgnv

        !--------------------------------------!
        ! LOOP ON THE SEGMENT BETWEEN 2 NODES  !
        !--------------------------------------!
        DO jseg=1,MAX(sec%nb_point-1,0)
              
           !-------------------------------------------------------------------------------------------
           ! Select the appropriate coordinate for computing the velocity of the segment
           !
           !                      CASE(0)                                    Case (2)
           !                      -------                                    --------
           !  listPoint(jseg)                 listPoint(jseg+1)       listPoint(jseg)  F(i,j)      
           !      F(i,j)----------V(i+1,j)-------F(i+1,j)                               |
           !                                                                            |
           !                                                                            |
           !                                                                            |
           !                      Case (3)                                            U(i,j)
           !                      --------                                              |
           !                                                                            |
           !  listPoint(jseg+1) F(i,j+1)                                                |
           !                        |                                                   |
           !                        |                                                   |
           !                        |                                 listPoint(jseg+1) F(i,j-1)
           !                        |                                            
           !                        |                                            
           !                     U(i,j+1)                                            
           !                        |                                       Case(1)     
           !                        |                                       ------      
           !                        |                                            
           !                        |                 listPoint(jseg+1)             listPoint(jseg)                           
           !                        |                 F(i-1,j)-----------V(i,j) -------f(jseg)                           
           ! listPoint(jseg)     F(i,j)
           ! 
           !-------------------------------------------------------------------------------------------

           SELECT CASE( sec%direction(jseg) )
           CASE(0)  ;   k = sec%listPoint(jseg)
           CASE(1)  ;   k = POINT_SECTION(sec%listPoint(jseg)%I+1,sec%listPoint(jseg)%J)
           CASE(2)  ;   k = sec%listPoint(jseg)
           CASE(3)  ;   k = POINT_SECTION(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J+1)
           END SELECT

           !-------------------------------
           !  LOOP ON THE DENSITY CLASSES |
           !-------------------------------
           !The computation is made for each density class
           DO jclass=1,MAX(1,sec%nb_class-1)

              ztransp1=0._wp ; zrhoi1=0._wp ; zrhop1=0._wp ; ztemp1=0._wp ;zsal1=0._wp
              ztransp2=0._wp ; zrhoi2=0._wp ; zrhop2=0._wp ; ztemp2=0._wp ;zsal2=0._wp
    
              !---------------------------|
              !     LOOP ON THE LEVEL     |
              !---------------------------|
              !Sum of the transport on the vertical 
              DO jk=1,jpk
                    

                 ! compute temparature, salinity, insitu & potential density, ssh and depth at U/V point
                 SELECT CASE( sec%direction(jseg) )
                 CASE(0,1)
                    ztn   = interp(k%I,k%J,jk,'V',tsn(:,:,:,jp_tem) )
                    zsn   = interp(k%I,k%J,jk,'V',tsn(:,:,:,jp_sal) )
                    zrhop = interp(k%I,k%J,jk,'V',rhop)
                    zrhoi = interp(k%I,k%J,jk,'V',rhd*rau0+rau0)
                    zsshn =  0.5*( sshn(k%I,k%J)    + sshn(k%I,k%J+1)    ) * vmask(k%I,k%J,1)
                 CASE(2,3)
                    ztn   = interp(k%I,k%J,jk,'U',tsn(:,:,:,jp_tem) )
                    zsn   = interp(k%I,k%J,jk,'U',tsn(:,:,:,jp_sal) )
                    zrhop = interp(k%I,k%J,jk,'U',rhop)
                    zrhoi = interp(k%I,k%J,jk,'U',rhd*rau0+rau0)
                    zsshn =  0.5*( sshn(k%I,k%J)    + sshn(k%I+1,k%J)    ) * umask(k%I,k%J,1) 
                 END SELECT

                 zfsdep= gdept(k%I,k%J,jk)
 
                 !----------------------------------------------!
                 !TEST ON THE DENSITY/SALINITY/TEMPERATURE/LEVEL! 
                 !----------------------------------------------!
 
                 IF ( (    ((( zrhop .GE. (sec%zsigp(jclass)+1000.  )) .AND.    &
                           (   zrhop .LE. (sec%zsigp(jclass+1)+1000. ))) .OR.    &
                           ( sec%zsigp(jclass) .EQ. 99.)) .AND.                 &
                           ((( zrhoi .GE. (sec%zsigi(jclass) + 1000.  )) .AND.    &
                           (   zrhoi .LE. (sec%zsigi(jclass+1)+1000. ))) .OR.    &
                           ( sec%zsigi(jclass) .EQ. 99.)) .AND.                 &
                           ((( zsn .GT. sec%zsal(jclass)) .AND.                &
                           (   zsn .LE. sec%zsal(jclass+1))) .OR.              &
                           ( sec%zsal(jclass) .EQ. 99.)) .AND.                 &
                           ((( ztn .GE. sec%ztem(jclass)) .AND.                &
                           (   ztn .LE. sec%ztem(jclass+1))) .OR.              &
                           ( sec%ztem(jclass) .EQ.99.)) .AND.                  &
                           ((( zfsdep .GE. sec%zlay(jclass)) .AND.            &
                           (   zfsdep .LE. sec%zlay(jclass+1))) .OR.          &
                           ( sec%zlay(jclass) .EQ. 99. ))))   THEN


                    !compute velocity with the correct direction
                    SELECT CASE( sec%direction(jseg) )
                    CASE(0,1)  
                       zumid=0.
                       zvmid=isgnv*vn(k%I,k%J,jk)*vmask(k%I,k%J,jk)
                    CASE(2,3)
                       zumid=isgnu*un(k%I,k%J,jk)*umask(k%I,k%J,jk)
                       zvmid=0.
                    END SELECT

                    !velocity* cell's length * cell's thickness
                    zTnorm=zumid*e2u(k%I,k%J)*  fse3u(k%I,k%J,jk)+     &
                           zvmid*e1v(k%I,k%J)*  fse3v(k%I,k%J,jk)

#if ! defined key_vvl
                    !add transport due to free surface
                    IF( jk==1 )THEN
                       zTnorm = zTnorm + zumid* e2u(k%I,k%J) * zsshn * umask(k%I,k%J,jk) + &
                                         zvmid* e1v(k%I,k%J) * zsshn * vmask(k%I,k%J,jk)
                    ENDIF
#endif
                    !COMPUTE TRANSPORT 
                    !zTnorm=transport through one cell for one class
                    !ztransp1 or ztransp2=transport through one cell i
                    !                     for one class for one direction
                    IF( zTnorm .GE. 0 )THEN

                       ztransp1=zTnorm+ztransp1
 
                       IF ( sec%llstrpond ) THEN
                          ztemp1 = ztemp1  + zTnorm * ztn 
                          zsal1  = zsal1   + zTnorm * zsn
                          zrhoi1 = zrhoi1  + zTnorm * zrhoi
                          zrhop1 = zrhop1  + zTnorm * zrhop
                       ENDIF

                    ELSE

                       ztransp2=(zTnorm)+ztransp2

                       IF ( sec%llstrpond ) THEN
                          ztemp2 = ztemp2  + zTnorm * ztn 
                          zsal2  = zsal2   + zTnorm * zsn
                          zrhoi2 = zrhoi2  + zTnorm * zrhoi
                          zrhop2 = zrhop2  + zTnorm * zrhop
                       ENDIF
                    ENDIF
 
            
                 ENDIF ! end of density test
              ENDDO!end of loop on the level

              !ZSUM=TRANSPORT FOR EACH CLASSES FOR THE  DIRECTIONS
              !---------------------------------------------------
              zsum(1,jclass)     = zsum(1,jclass)+ztransp1
              zsum(2,jclass)     = zsum(2,jclass)+ztransp2
              IF( sec%llstrpond )THEN
                 zsum(3 ,jclass) = zsum( 3,jclass)+zrhoi1
                 zsum(4 ,jclass) = zsum( 4,jclass)+zrhoi2
                 zsum(5 ,jclass) = zsum( 5,jclass)+zrhop1
                 zsum(6 ,jclass) = zsum( 6,jclass)+zrhop2
                 zsum(7 ,jclass) = zsum( 7,jclass)+ztemp1
                 zsum(8 ,jclass) = zsum( 8,jclass)+ztemp2
                 zsum(9 ,jclass) = zsum( 9,jclass)+zsal1
                 zsum(10,jclass) = zsum(10,jclass)+zsal2
              ENDIF
   
           ENDDO !end of loop on the density classes

#if defined key_lim2 || defined key_lim3

           !ICE CASE    
           !------------
           IF( sec%ll_ice_section )THEN
              SELECT CASE (sec%direction(jseg))
              CASE(0)
                 zumid_ice = 0
                 zvmid_ice =  isgnv*0.5*(v_ice(k%I,k%J+1)+v_ice(k%I+1,k%J+1))
              CASE(1)
                 zumid_ice = 0
                 zvmid_ice =  isgnv*0.5*(v_ice(k%I,k%J+1)+v_ice(k%I+1,k%J+1))
              CASE(2)
                 zvmid_ice = 0
                 zumid_ice =  isgnu*0.5*(u_ice(k%I+1,k%J)+u_ice(k%I+1,k%J+1))
              CASE(3)
                 zvmid_ice = 0
                 zumid_ice =  isgnu*0.5*(u_ice(k%I+1,k%J)+u_ice(k%I+1,k%J+1))
              END SELECT
   
              zTnorm=zumid_ice*e2u(k%I,k%J)+zvmid_ice*e1v(k%I,k%J)
   
              IF( zTnorm .GE. 0)THEN
                 zice_vol_pos = (zTnorm)*   &
                                      (1.0 - frld(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J))  &
                                     *(hsnif(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J) +  &
                                       hicif(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J)) &
                                      +zice_vol_pos
                 zice_surf_pos = (zTnorm)*   &
                                       (1.0 -  frld(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J))  &
                                      +zice_surf_pos
              ELSE
                 zice_vol_neg=(zTnorm)*   &
                                   (1.0 - frld(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J))  &
                                  *(hsnif(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J) +  &
                                    hicif(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J)) &
                                  +zice_vol_neg
                 zice_surf_neg=(zTnorm)*   &
                                    (1.0 - frld(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J))  &
                                     +zice_surf_neg
              ENDIF
   
              zsum(11,1) = zsum(11,1)+zice_vol_pos
              zsum(12,1) = zsum(12,1)+zice_vol_neg
              zsum(13,1) = zsum(13,1)+zice_surf_pos
              zsum(14,1) = zsum(14,1)+zice_surf_neg
   
           ENDIF !end of ice case
#endif
 
        ENDDO !end of loop on the segment


     ELSE  !if sec%nb_point =0
        zsum(1:2,:)=0.
        IF (sec%llstrpond) zsum(3:10,:)=0.
        zsum( 11:14,:)=0.
     ENDIF   !end of sec%nb_point =0 case

     !-------------------------------|
     !FINISH COMPUTING TRANSPORTS    |
     !-------------------------------|
     DO jclass=1,MAX(1,sec%nb_class-1)
        sec%transport(1,jclass)=sec%transport(1,jclass)+zsum(1,jclass)*1.E-6
        sec%transport(2,jclass)=sec%transport(2,jclass)+zsum(2,jclass)*1.E-6
        IF( sec%llstrpond ) THEN
           IF( zsum(1,jclass) .NE. 0._wp ) THEN
              sec%transport( 3,jclass) = sec%transport( 3,jclass) + zsum( 3,jclass)/zsum(1,jclass)
              sec%transport( 5,jclass) = sec%transport( 5,jclass) + zsum( 5,jclass)/zsum(1,jclass)
              sec%transport( 7,jclass) = sec%transport( 7,jclass) + zsum( 7,jclass)
              sec%transport( 9,jclass) = sec%transport( 9,jclass) + zsum( 9,jclass)
           ENDIF
           IF( zsum(2,jclass) .NE. 0._wp )THEN
              sec%transport( 4,jclass) = sec%transport( 4,jclass) + zsum( 4,jclass)/zsum(2,jclass)
              sec%transport( 6,jclass) = sec%transport( 6,jclass) + zsum( 6,jclass)/zsum(2,jclass)
              sec%transport( 8,jclass) = sec%transport( 8,jclass) + zsum( 8,jclass)
              sec%transport(10,jclass) = sec%transport(10,jclass) + zsum(10,jclass)
           ENDIF
        ELSE
           sec%transport( 3,jclass) = 0._wp
           sec%transport( 4,jclass) = 0._wp
           sec%transport( 5,jclass) = 0._wp
           sec%transport( 6,jclass) = 0._wp
           sec%transport( 7,jclass) = 0._wp
           sec%transport( 8,jclass) = 0._wp
           sec%transport(10,jclass) = 0._wp
        ENDIF
     ENDDO   

     IF( sec%ll_ice_section ) THEN
        sec%transport( 9,1)=sec%transport( 9,1)+zsum( 9,1)*1.E-6
        sec%transport(10,1)=sec%transport(10,1)+zsum(10,1)*1.E-6
        sec%transport(11,1)=sec%transport(11,1)+zsum(11,1)*1.E-6
        sec%transport(12,1)=sec%transport(12,1)+zsum(12,1)*1.E-6
     ENDIF

     CALL wrk_dealloc( nb_type_class , nb_class_max , zsum   )
     !
  END SUBROUTINE transport
  
  SUBROUTINE dia_dct_wri(kt,ksec,sec)
     !!-------------------------------------------------------------
     !! Write transport output in numdct 
     !! 
     !! Purpose: Write  transports in ascii files
     !! 
     !! Method:
     !!        1. Write volume transports in "volume_transport"
     !!           Unit: Sv : area * Velocity / 1.e6 
     !! 
     !!        2. Write heat transports in "heat_transport"
     !!           Unit: Peta W : area * Velocity * T * rhau * Cp / 1.e15
     !! 
     !!        3. Write salt transports in "salt_transport"
     !!           Unit: 10^9 g m^3 / s : area * Velocity * S / 1.e6
     !!
     !!------------------------------------------------------------- 
     !!arguments
     INTEGER, INTENT(IN)          :: kt          ! time-step
     TYPE(SECTION), INTENT(INOUT) :: sec         ! section to write   
     INTEGER ,INTENT(IN)          :: ksec        ! section number

     !!local declarations
     INTEGER               :: jcl,ji             ! Dummy loop
     CHARACTER(len=2)      :: classe             ! Classname 
     REAL(wp)              :: zbnd1,zbnd2        ! Class bounds
     REAL(wp)              :: zslope             ! section's slope coeff
     !
     REAL(wp), POINTER, DIMENSION(:):: zsumclass ! 1D workspace 
     !!------------------------------------------------------------- 
     CALL wrk_alloc(nb_type_class , zsumclass )  

     zsumclass(:)=0._wp
     zslope = sec%slopeSection       

 
     DO jcl=1,MAX(1,sec%nb_class-1)

        ! Mean computation
        sec%transport(:,jcl)=sec%transport(:,jcl)/(nn_dctwri/nn_dct)
        classe   = 'N       '
        zbnd1   = 0._wp
        zbnd2   = 0._wp
        zsumclass(1:nb_type_class)=zsumclass(1:nb_type_class)+sec%transport(1:nb_type_class,jcl)

   
        !insitu density classes transports
        IF( ( sec%zsigi(jcl)   .NE. 99._wp ) .AND. &
            ( sec%zsigi(jcl+1) .NE. 99._wp )       )THEN
           classe = 'DI       '
           zbnd1 = sec%zsigi(jcl)
           zbnd2 = sec%zsigi(jcl+1)
        ENDIF
        !potential density classes transports
        IF( ( sec%zsigp(jcl)   .NE. 99._wp ) .AND. &
            ( sec%zsigp(jcl+1) .NE. 99._wp )       )THEN
           classe = 'DP      '
           zbnd1 = sec%zsigp(jcl)
           zbnd2 = sec%zsigp(jcl+1)
        ENDIF
        !depth classes transports
        IF( ( sec%zlay(jcl)    .NE. 99._wp ) .AND. &
            ( sec%zlay(jcl+1)  .NE. 99._wp )       )THEN 
           classe = 'Z       '
           zbnd1 = sec%zlay(jcl)
           zbnd2 = sec%zlay(jcl+1)
        ENDIF
        !salinity classes transports
        IF( ( sec%zsal(jcl) .NE. 99._wp    ) .AND. &
            ( sec%zsal(jcl+1) .NE. 99._wp  )       )THEN
           classe = 'S       '
           zbnd1 = sec%zsal(jcl)
           zbnd2 = sec%zsal(jcl+1)   
        ENDIF
        !temperature classes transports
        IF( ( sec%ztem(jcl) .NE. 99._wp     ) .AND. &
            ( sec%ztem(jcl+1) .NE. 99._wp     )       ) THEN
           classe = 'T       '
           zbnd1 = sec%ztem(jcl)
           zbnd2 = sec%ztem(jcl+1)
        ENDIF
                  
        !write volume transport per class
        WRITE(numdct_vol,118) ndastp,kt,ksec,sec%name,zslope, &
                              jcl,classe,zbnd1,zbnd2,&
                              sec%transport(1,jcl),sec%transport(2,jcl), &
                              sec%transport(1,jcl)+sec%transport(2,jcl)

        IF( sec%llstrpond )THEN

           !write heat transport per class:
           WRITE(numdct_heat,119) ndastp,kt,ksec,sec%name,zslope,  &
                              jcl,classe,zbnd1,zbnd2,&
                              sec%transport(7,jcl)*1000._wp*rcp/1.e15,sec%transport(8,jcl)*1000._wp*rcp/1.e15, &
                              ( sec%transport(7,jcl)+sec%transport(8,jcl) )*1000._wp*rcp/1.e15
           !write salt transport per class
           WRITE(numdct_salt,119) ndastp,kt,ksec,sec%name,zslope,  &
                              jcl,classe,zbnd1,zbnd2,&
                              sec%transport(9,jcl)*1000._wp/1.e9,sec%transport(10,jcl)*1000._wp/1.e9,&
                              (sec%transport(9,jcl)+sec%transport(10,jcl))*1000._wp/1.e9
        ENDIF

     ENDDO

     zbnd1 = 0._wp
     zbnd2 = 0._wp
     jcl=0

     !write total volume transport
     WRITE(numdct_vol,118) ndastp,kt,ksec,sec%name,zslope, &
                           jcl,"total",zbnd1,zbnd2,&
                           zsumclass(1),zsumclass(2),zsumclass(1)+zsumclass(2)

     IF( sec%llstrpond )THEN

        !write total heat transport
        WRITE(numdct_heat,119) ndastp,kt,ksec,sec%name,zslope, &
                           jcl,"total",zbnd1,zbnd2,&
                           zsumclass(7)* 1000._wp*rcp/1.e15,zsumclass(8)* 1000._wp*rcp/1.e15,&
                           (zsumclass(7)+zsumclass(8) )* 1000._wp*rcp/1.e15
        !write total salt transport
        WRITE(numdct_salt,119) ndastp,kt,ksec,sec%name,zslope, &
                           jcl,"total",zbnd1,zbnd2,&
                           zsumclass(9)*1000._wp/1.e9,zsumclass(10)*1000._wp/1.e9,&
                           (zsumclass(9)+zsumclass(10))*1000._wp/1.e9
     ENDIF

      
     IF ( sec%ll_ice_section) THEN
        !write total ice volume transport
        WRITE(numdct_vol,118) ndastp,kt,ksec,sec%name,zslope,&
                              jcl,"ice_vol",zbnd1,zbnd2,&
                              sec%transport(9,1),sec%transport(10,1),&
                              sec%transport(9,1)+sec%transport(10,1)
        !write total ice surface transport
        WRITE(numdct_vol,118) ndastp,kt,ksec,sec%name,zslope,&
                              jcl,"ice_surf",zbnd1,zbnd2,&
                              sec%transport(11,1),sec%transport(12,1), &
                              sec%transport(11,1)+sec%transport(12,1) 
     ENDIF
                                              
118 FORMAT(I8,1X,I8,1X,I4,1X,A30,1X,f9.2,1X,I4,3X,A8,1X,2F12.4,5X,3F12.4)
119 FORMAT(I8,1X,I8,1X,I4,1X,A30,1X,f9.2,1X,I4,3X,A8,1X,2F12.4,5X,3E15.6)

     CALL wrk_dealloc(nb_type_class , zsumclass )  
  END SUBROUTINE dia_dct_wri

  FUNCTION interp(ki, kj, kk, cd_point, ptab)
  !!----------------------------------------------------------------------
  !!
  !!   Purpose: compute Temperature/Salinity/density at U-point or V-point
  !!   --------
  !!
  !!   Method:
  !!   ------
  !!
  !!   ====> full step and partial step
  !! 
  !!
  !!    |    I          |    I+1           |    Z=Temperature/Salinity/density at U-poinT
  !!    |               |                  |
  !!  ----------------------------------------  1. Veritcale interpolation: compute zbis
  !!    |               |                  |       interpolation between ptab(I,J,K) and ptab(I,J,K+1)
  !!    |               |                  |       zbis = 
  !!    |               |                  |      [ e3w(I+1,J,K)*ptab(I,J,K) + ( e3w(I,J,K) - e3w(I+1,J,K) ) * ptab(I,J,K-1) ]
  !!    |               |                  |      /[ e3w(I+1,J,K) + e3w(I,J,K) - e3w(I+1,J,K) ] 
  !!    |               |                  | 
  !!    |               |                  |    2. Horizontal interpolation: compute value at U/V point
  !!K-1 | ptab(I,J,K-1) |                  |       interpolation between zbis and ptab(I+1,J,K)  
  !!    |     .         |                  |
  !!    |     .         |                  |       interp = ( 0.5*zet2*zbis + 0.5*zet1*ptab(I+1,J,K) )/(0.5*zet2+0.5*zet1) 
  !!    |     .         |                  |
  !!  ------------------------------------------
  !!    |     .         |                  |
  !!    |     .         |                  |
  !!    |     .         |                  |
  !!K   |    zbis.......U...ptab(I+1,J,K)  |
  !!    |     .         |                  |
  !!    | ptab(I,J,K)   |                  |
  !!    |               |------------------|
  !!    |               | partials         |
  !!    |               |  steps           |
  !!  -------------------------------------------
  !!    <----zet1------><----zet2--------->
  !!
  !!
  !!   ====>  s-coordinate
  !!     
  !!    |                |                  |   1. Compute distance between T1 and U points: SQRT( zdep1^2 + (0.5 * zet1 )^2
  !!    |                |                  |      Compute distance between T2 and U points: SQRT( zdep2^2 + (0.5 * zet2 )^2
  !!    |                | ptab(I+1,J,K)    | 
  !!    |                |      T2          |   2. Interpolation between  T1 and T2 values at U point 
  !!    |                |      ^           |    
  !!    |                |      | zdep2     |    
  !!    |                |      |           |    
  !!    |       ^        U      v           |
  !!    |       |        |                  |
  !!    |       | zdep1  |                  |    
  !!    |       v        |                  |
  !!    |      T1        |                  |
  !!    | ptab(I,J,K)    |                  | 
  !!    |                |                  | 
  !!    |                |                  | 
  !!
  !!    <----zet1--------><----zet2--------->
  !!
  !!----------------------------------------------------------------------
  !*arguments
  INTEGER, INTENT(IN)                          :: ki, kj, kk   ! coordinate of point
  CHARACTER(len=1), INTENT(IN)                 :: cd_point     ! type of point (U, V)
  REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(IN) :: ptab         ! variable to compute at (ki, kj, kk )
  REAL(wp)                                     :: interp       ! interpolated variable 

  !*local declations
  INTEGER :: ii1, ij1, ii2, ij2                                ! local integer
  REAL(wp):: ze3t, zfse3, zwgt1, zwgt2, zbis, zdepu            ! local real
  REAL(wp):: zet1, zet2                                        ! weight for interpolation 
  REAL(wp):: zdep1,zdep2                                       ! differences of depth
  !!----------------------------------------------------------------------

  IF( cd_point=='U' )THEN 
     ii1 = ki    ; ij1 = kj 
     ii2 = ki+1  ; ij2 = kj 

     zet1=e1t(ii1,ij1)
     zet2=e1t(ii2,ij2)

  ELSE ! cd_point=='V' 
     ii1 = ki    ; ij1 = kj 
     ii2 = ki    ; ij2 = kj+1  

     zet1=e2t(ii1,ij1)
     zet2=e2t(ii2,ij2)

  ENDIF

  IF( ln_sco )THEN   ! s-coordinate case

     zdepu = ( fsdept(ii1,ij1,kk) +  fsdept(ii2,ij2,kk) ) /2 
     zdep1 = fsdept(ii1,ij1,kk) - zdepu
     zdep2 = fsdept(ii2,ij2,kk) - zdepu

     !weights
     zwgt1 = SQRT( ( 0.5 * zet1 ) * ( 0.5 * zet1 ) + ( zdep1 * zdep1 ) )
     zwgt2 = SQRT( ( 0.5 * zet2 ) * ( 0.5 * zet2 ) + ( zdep2 * zdep2 ) )
  
     ! result
     interp = umask(ii1,ij1,kk) * ( zwgt2 *  ptab(ii1,ij1,kk) + zwgt1 *  ptab(ii1,ij1,kk) ) / ( zwgt2 + zwgt1 )   


  ELSE       ! full step or partial step case 

#if defined key_vvl

     ze3t  = fse3t_n(ii2,ij2,kk) - fse3t_n(ii1,ij1,kk) 
     zwgt1 = ( fse3w_n(ii2,ij2,kk) - fse3w_n(ii1,ij1,kk) ) / fse3w_n(ii2,ij2,kk)
     zwgt2 = ( fse3w_n(ii1,ij1,kk) - fse3w_n(ii2,ij2,kk) ) / fse3w_n(ii1,ij1,kk)

#else

     ze3t  = fse3t(ii2,ij2,kk)   - fse3t(ii1,ij1,kk) 
     zwgt1 = ( fse3w(ii2,ij2,kk) - fse3w(ii1,ij1,kk) ) / fse3w(ii2,ij2,kk)
     zwgt2 = ( fse3w(ii1,ij1,kk) - fse3w(ii2,ij2,kk) ) / fse3w(ii1,ij1,kk)

#endif

     IF(kk .NE. 1)THEN

        IF( ze3t >= 0. )THEN 
           !zbis
           zbis = ptab(ii2,ij2,kk) + zwgt1 * ( ptab(ii2,ij2,kk-1) - ptab(ii2,ij2,kk) ) 
           ! result
            interp = umask(ii1,ij1,kk) * ( zet2 * ptab(ii1,ij1,kk) + zet1 * zbis )/( zet1 + zet2 )
        ELSE
           !zbis
           zbis = ptab(ii1,ij1,kk) + zwgt2 * ( ptab(ii1,ij1,kk-1) - ptab(ii1,ij2,kk) )
           ! result
           interp = umask(ii1,ij1,kk) * ( zet2 * zbis + zet1 * ptab(ii2,ij2,kk) )/( zet1 + zet2 )
        ENDIF    

     ELSE
        interp = umask(ii1,ij1,kk) * (  zet2 * ptab(ii1,ij1,kk) + zet1 * ptab(ii2,ij2,kk) )/( zet1 + zet2 )
     ENDIF

  ENDIF


  END FUNCTION interp

#else
   !!----------------------------------------------------------------------
   !!   Default option :                                       Dummy module
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_diadct = .FALSE.    !: diamht flag
   PUBLIC 
CONTAINS

   SUBROUTINE dia_dct_init          ! Dummy routine
      WRITE(*,*) 'dia_dct_init: You should not have seen this print! error?', kt
   END SUBROUTINE dia_dct_init

   SUBROUTINE dia_dct( kt )           ! Dummy routine
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index
      WRITE(*,*) 'dia_dct: You should not have seen this print! error?', kt
   END SUBROUTINE dia_dct
#endif

END MODULE diadct
