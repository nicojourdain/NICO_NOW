PROGRAM mpp_optimiz_nc
 !!---------------------------------------------------------------------
 !!
 !!                       PROGRAM MPP_OPTIMIZ_NC
 !!                     ***********************
 !!
 !!  PURPOSE :
 !!  ---------
 !!              This program is build to optimize the domain beakdown into
 !!              subdomain for mpp computing.
 !!              Once the grid size, and the land/sea mask is known, it looks
 !!              for all the possibilities within a range of setting parameters
 !!              and determine the optimal.
 !!
 !!              Optimization is done with respect to the maximum number of
 !!              sea processors and to the maximum numbers of procs (jprocx)
 !!                     
 !!              Optional optimization can be performed takink into account
 !!              the maximum available processor memory ppmcal. This is
 !!              activated if jpmen =1
 !!
 !! history:
 !! --------
 !!       original  : 95-12 (Imbard M) for OPA8.1, CLIPPER
 !!       f90       : 03-06 (Molines JM), namelist as input
 !!                 : 05-05 (Molines JM), bathy in ncdf
 !!----------------------------------------------------------------------
 !! * modules used
  USE netcdf

  IMPLICIT NONE

  INTEGER ::  jprocx=250   !: maximum number of proc. (Read from namelist)
  INTEGER ::  jpmem=0      !: memory constraint (1) or no constraint (0)
     !                     !  (use 1 with caution as the memory size of 
     !                     !   the code lays on OPA 8.1 estimates ...)
     !
  INTEGER          ::  &
       jpk    = 46  ,    & !: vertical levels (namelist)
       jpiglo = 1442,    & !: I-size of the model (namelist)
       jpjglo = 1021,    & !: J-size of the model (namelist)
       jpidta = 1442,    & !: I-size of the data file (namelist)
       jpjdta = 1021,   &  !: J-size of the data files (namelist)
       nizoom = 1 ,     &  !: I zoom indicator (namelist)
       njzoom = 1 ,     &  !: J zoom indicatori (namelist)
       numnam = 4          !: logical unit for the namelist
  NAMELIST /namspace/ jpk,jpiglo,jpjglo,jpidta,jpjdta,nizoom,njzoom
  NAMELIST /namproc/ jprocx, jpmem

  INTEGER ::  jpnix ,jpnjx  
  !
  INTEGER,PARAMETER :: jpreci=1 ,jprecj=1
  !
  ! Following variables are used only if jpmem=1
  REAL(KIND=4) ::  ppmpt ,   &
       ppmcal = 225000000., &  !: maximum memory of one processor for a given machine (in 8 byte words)
       ppmin  = 0.4,         & !: minimum ratio to fill the memory
       ppmax  = 0.9            !: maximum ration to fill the memory
  ! Aleph
  !     PARAMETER(ppmcal= 16000000.)
  !Brodie
  !     PARAMETER(ppmcal=250000000.)
  ! Uqbar
  !     PARAMETER(ppmcal=3750000000.)
  ! Zahir
  !     PARAMETER(ppmcal=225000000.)

  CHARACTER(LEN=80) :: cbathy, &       !: File name of the netcdf bathymetry (namelist)
      &                clvar           !: Variable name in netcdf for the bathy to be read
  LOGICAL ::  ln_zps=.false.           !: Logical flag for partial cells.
  NAMELIST /namfile/ cbathy, ln_zps
  NAMELIST /namparam/ ppmcal, ppmin, ppmax
  !
  INTEGER :: iumout = 1
  INTEGER :: ji,jj,jn,jni,jnj,jni2,jnj2
  INTEGER :: iumbat,ifreq,il1,il2
  INTEGER :: ii,iim,ij,ijm,imoy,iost,iresti,irestj,isurf,ivide
  INTEGER :: iilb,ijlb,ireci,irecj,in
  INTEGER :: ipi,ipj
  INTEGER :: inf10,inf30,inf50,iptx,isw
  INTEGER :: iii,iij,iiii,iijj,iimoy,iinf10,iinf30,iinf50
  !
  INTEGER,DIMENSION(:,:),ALLOCATABLE     ::  ibathy    ! jpidta -jpjdta
  INTEGER,DIMENSION(:,:),ALLOCATABLE     ::  ippdi, ippdj ,iidom, ijdom
  !
  REAL(KIND=4)                           ::  zmin,zmax,zper,zmem
  REAL(KIND=4)                           ::  zzmin,zzmax,zperx
  REAL(KIND=4),DIMENSION(:,:),ALLOCATABLE  ::  zmask ,&  ! jpiglo -jpjglo
      &                                        zdta      ! jpidta -jpjdta

 ! CDF stuff
  INTEGER :: ncid, ivarid, istatus
  LOGICAL ::  llbon=.false.
  !
  ! 0. Initialisation
  ! -----------------
  OPEN(numnam,FILE='namelist')
  REWIND(numnam)
  READ(numnam,namspace)

  REWIND(numnam)
  READ(numnam,namfile)

  REWIND(numnam)
  READ(numnam,namparam)

  REWIND(numnam)
  READ(numnam,namproc)

  ! estimated  code size expressed in number of 3D arrays (valid for OPA8.1)
  ppmpt = 55.+73./jpk
  jpnix = jprocx ; jpnjx=jprocx

  ALLOCATE ( ibathy(jpidta,jpjdta), zmask(jpiglo,jpjglo),zdta(jpidta,jpjdta) )
  ALLOCATE (ippdi(jpnix,jpnjx), ippdj(jpnix,jpnjx) )
  ALLOCATE (iidom(jpnix,jpnjx), ijdom(jpnix,jpnjx) )

  OPEN(iumout,FILE='processor.layout')
  WRITE(iumout,*)
  WRITE(iumout,*) ' optimisation de la partition'
  WRITE(iumout,*) ' ----------------------------'
  WRITE(iumout,*)
  !
  ! * Read cdf bathy file
  !
         IF ( ln_zps ) THEN        ! partial steps
            clvar = 'Bathymetry'
         ELSE 
            clvar = 'Bathy_level'  ! full steps
         ENDIF

         INQUIRE( FILE=cbathy, EXIST=llbon )
      IF( llbon ) THEN
            istatus=NF90_OPEN(cbathy,NF90_NOWRITE,ncid)
            istatus=NF90_INQ_VARID(ncid,clvar,ivarid)
            istatus=NF90_GET_VAR(ncid,ivarid,zdta)
            istatus=NF90_CLOSE(ncid)
      ELSE
          PRINT *,' File missing : ', trim(cbathy)
          STOP
      ENDIF
  ibathy(:,:)=zdta(:,:)

  !
  ! Building the mask
  DO jj=1,jpjglo
     DO ji=1,jpiglo
        zmask(ji,jj) = float(ibathy(ji+nizoom - 1,jj+njzoom -1))
     END DO
  END DO

  DO jj=1,jpjglo
     DO ji=1,jpiglo
        zmask(ji,jj)=  min(REAL(1.,kind=4),max(REAL(0.,kind=4),zmask(ji,jj)))  ! Old vector coding rule ...
     END DO
  END DO
  !
  !  Main loop on processors
  ! ------------------------
  iii=1 ; iij=1
  iiii=jpiglo ; iijj=jpjglo
  iptx=0
  iimoy=0
  zzmin=0. ; zzmax=0.
  iinf10=0 ; iinf30=0 ; iinf50=0
  zperx=1.
  in=0
  DO jni=1,jpnix
     DO jnj=1,jpnjx
        !
        ! Limitation ob the maxumun number of PE's
        IF(jni*jnj >  jprocx) goto 1000
        !
        ! Partition
        ipi=(jpiglo-2*jpreci + (jni-1))/jni + 2*jpreci
        ipj=(jpjglo-2*jprecj + (jnj-1))/jnj + 2*jprecj
        !
        ! Memory optimization ?
        isw=0
        zmem=ppmpt*ipi*ipj*jpk
        IF(zmem > ppmcal) go to 1000
        IF(jpmem == 1) THEN
           IF(zmem.GT.ppmax*ppmcal.OR.zmem.LT.ppmin*ppmcal) isw=1
        ENDIF
        IF(isw.EQ.1) go to 1000
        in=in+1
        !
        WRITE(iumout,*) '--> nombre de processeurs ',jni*jnj
        WRITE(iumout,*) ' '
        WRITE(iumout,*) " jpni=",jni ," jpnj=",jnj
        WRITE(iumout,*) " jpi= ",ipi ," jpj= ",ipj
        zper=(jni*jnj*ipi*ipj)/float(jpiglo*jpjglo)
        WRITE(iumout,*) " rapport jpnij*domain/global domain ",zper
        !
        ! Coin en bas a gauche de chaque processeur
        !
        iilb=1
        ijlb=1
        ireci=2*jpreci
        irecj=2*jprecj
        iresti = MOD ( jpiglo - ireci , jni )
        irestj = MOD ( jpjglo - irecj , jnj )
        !
        IF (iresti.EQ.0) iresti = jni
        DO jj=1,jnj
           DO ji=1,iresti
              ippdi(ji,jj) = ipi
           END DO
           DO ji=iresti+1,jni
              ippdi(ji,jj) = ipi -1
           END DO
        END DO
        IF (irestj.EQ.0) irestj = jnj
        DO ji=1,jni
           DO jj=1,irestj
              ippdj(ji,jj) = ipj
           END DO
           DO jj=irestj+1,jnj
              ippdj(ji,jj) = ipj -1
           END DO
        END DO
        DO jj=1,jnj
           DO ji=1,jni
              iidom(ji,jj)=iilb
              ijdom(ji,jj)=ijlb
           END DO
        END DO
        WRITE(iumout,*) " iresti=",iresti," irestj=",irestj
        !
        !  2. Boucle sur les processeurs
        ! ------------------------------
        !
        ivide=0
        imoy=0
        zmin=1.e+20
        zmax=-1.e+20
        inf10=0
        inf30=0
        inf50=0
        !
        DO jni2=1,jni
           DO jnj2=1,jnj

              IF(jni.GT.1)THEN
                 DO jj=1,jnj
                    DO ji=2,jni
                       iidom(ji,jj)=iidom(ji-1,jj)+ippdi(ji-1,jj)-ireci
                    END DO
                 END DO
                 iilb=iidom(jni2,jnj2)
              ENDIF
              IF(jnj.GT.1)THEN
                 DO jj=2,jnj
                    DO ji=1,jni
                       ijdom(ji,jj)=ijdom(ji,jj-1)+ippdj(ji,jj-1)-irecj
                    END DO
                 END DO
                 ijlb=ijdom(jni2,jnj2)
              ENDIF
              isurf=0
              DO jj=1+jprecj,ippdj(jni2,jnj2)-jprecj
                 DO  ji=1+jpreci,ippdi(jni2,jnj2)-jpreci
                    IF(zmask(ji+iilb-1,jj+ijlb-1).EQ.1.) isurf=isurf+1
                 END DO
              END DO
              IF(isurf.EQ.0) THEN
                 ivide=ivide+1
              ELSE
                 imoy=imoy+isurf
              ENDIF
              zper=float(isurf)/float(ipi*ipj)
              IF(zmin.GT.zper.AND.isurf.NE.0) zmin=zper
              IF(zmax.LT.zper.AND.isurf.NE.0) zmax=zper
              IF(zper.LT.0.1.AND.isurf.NE.0) inf10=inf10+1
              IF(zper.LT.0.3.AND.isurf.NE.0) inf30=inf30+1
              IF(zper.LT.0.5.AND.isurf.NE.0) inf50=inf50+1
              !
              !
              ! 3. Fin de boucle sur les processeurs, impression
              ! ------------------------------------------------
              !
           END DO
        END DO
        WRITE(iumout,*) ' nombre de processeurs       ',jni*jnj
        WRITE(iumout,*) ' nombre de processeurs mer   ',jni*jnj-ivide
        WRITE(iumout,*) ' nombre de processeurs terre ',ivide
        WRITE(iumout,*) ' moyenne de recouvrement     ',float(imoy)/float(jni*jnj-ivide)/float(ipi*ipj)
        WRITE(iumout,*) ' minimum de recouvrement     ',zmin
        WRITE(iumout,*) ' maximum de recouvrement     ',zmax
        WRITE(iumout,*) ' nb de p recouvrement < 10 % ',inf10
        WRITE(iumout,*) ' nb de p      10 < nb < 30 % ',inf30-inf10
        WRITE(iumout,*) ' nb de p      30 < nb < 50 % ',inf50-inf10 -inf30
        WRITE(iumout,*) ' nombre de points integres   ', (jni*jnj-ivide)*ipi*ipj
        WRITE(iumout,*) ' nbr de pts supplementaires  ', (jni*jnj-ivide)*ipi*ipj-jpiglo*jpjglo
        zper=float((jni*jnj-ivide))*float(ipi*ipj)/float(jpiglo*jpjglo)
        WRITE(iumout,*) ' % sup                       ',zper
        WRITE(iumout,*)
        ! 
        ! 
        ! 4. Recherche de l optimum
        ! -------------------------
        !
        IF(ivide.GT.iptx) THEN
           iii=jni
           iij=jnj
           iiii=ipi
           iijj=ipj
           iptx=ivide
           iimoy=imoy
           zzmin=zmin
           zzmax=zmax
           iinf10=inf10
           iinf30=inf30
           iinf50=inf50
           zperx=zper
        ELSE IF(ivide.EQ.iptx.AND.zperx.LT.zper) THEN
           iii=jni
           iij=jnj
           iiii=ipi
           iijj=ipj
           iimoy=imoy
           zzmin=zmin
           zzmax=zmax
           iinf10=inf10
           iinf30=inf30
           iinf50=inf50
           zperx=zper
        ENDIF
        !
        ! 5. Fin de boucle sur le nombre de processeurs
        ! ---------------------------------------------
        !
      1000 continue
     END DO
  END DO
  !
  !
  ! 6. Affichage resultat
  ! ---------------------
  !
  IF(in.EQ.0) THEN
     WRITE(iumout,*) ' le choix n'' a pas pu etre fait '
     WRITE(iumout,*)
     WRITE(iumout,*) 'le nombre de processeurs maximum est insuffisant'
     STOP 
  ENDIF
  WRITE(iumout,*) ' choix optimum'
  WRITE(iumout,*) ' ============='
  WRITE(iumout,*) 
  WRITE(iumout,*) '--> nombre de processeurs ',iii*iij
  WRITE(iumout,*) ' '
  WRITE(iumout,*) " jpni=",iii ," jpnj=",iij
  WRITE(iumout,*) " jpi= ",iiii ," jpj= ",iijj
  WRITE(iumout,*) 
  WRITE(iumout,*) ' nombre de processeurs mer   ',iii*iij-iptx
  WRITE(iumout,*) ' nombre de processeurs terre ',iptx
  WRITE(iumout,*) ' moyenne de recouvrement     ',float(iimoy)/float(iii*iij-iptx)/float(iiii*iijj)
  WRITE(iumout,*) ' minimum de recouvrement     ',zzmin
  WRITE(iumout,*) ' maximum de recouvrement     ',zzmax
  WRITE(iumout,*) ' nb de p recouvrement < 10 % ',iinf10
  WRITE(iumout,*) ' nb de p      10 < nb < 30 % ',iinf30-iinf10
  WRITE(iumout,*) ' nb de p      30 < nb < 50 % ',iinf50-iinf10 -iinf30
  WRITE(iumout,*) ' nombre de points integres   ', (iii*iij-iptx)*iiii*iijj
  WRITE(iumout,*) ' nbr de pts supplementaires  ', (iii*iij-iptx)*iiii*iijj-jpiglo*jpjglo
  WRITE(iumout,*) ' % sup                       ',zperx
  WRITE(iumout,*)
  CLOSE(iumout)
  !
  !
  !
  STOP
END PROGRAM mpp_optimiz_nc
