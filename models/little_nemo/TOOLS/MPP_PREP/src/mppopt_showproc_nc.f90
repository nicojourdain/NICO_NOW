PROGRAM mppopt_showproc_nc
 !!---------------------------------------------------------------------
 !!
 !!                       PROGRAM MPP_showproc_nc
 !!                     ***********************
 !!
 !!  PURPOSE :
 !!  ---------
 !!              Build a ascii file (suitable for the overlay function of
 !!              chart) holding the chosen domain decomposition, formely
 !!              determined by mpp_opatimize_nc.
 !!              It takes the same namelist than  mpp_optimize_nc, with
 !!              the jpni, jpnj given in the namelist (NAMKEEP)
 !!              
 !!
 !!              The output file is called from a root name in the namelist
 !!              (covdta) with the jpni, jpnj, and jpnij added to the name.
 !!   MODIFICATIONS:
 !!   --------------
 !!       original  : 95-12 (Imbard M)
 !!       modif pour chart : 98-12 ( J.M. Molines)
 !!             26/05/2005 : English documentation (partial ..) JMM
 !!----------------------------------------------------------------------
  !
  USE netcdf
  IMPLICIT NONE
  !
  INTEGER ::  jprocx=250
  !
  INTEGER :: jpmem=0
  !
  ! les dimensions du modele
  !
  INTEGER :: jpk,jpiglo,jpjglo, jpidta, jpjdta
  NAMELIST /namspace/ jpk,jpiglo,jpjglo, jpidta, jpjdta,nizoom,njzoom
  NAMELIST /namproc/ jprocx, jpmem

  INTEGER :: jpni,jpnj, jpnij
  CHARACTER(LEN=80) :: covdta, cdum
  NAMELIST /namkeep/ jpni,jpnj,covdta

  CHARACTER(LEN=80) ::  cbathy
  LOGICAL :: ln_zps=.false.
  NAMELIST /namfile / cbathy, ln_zps
  !!
  ! quelques parametres
  !
  INTEGER ::  jpnix,jpnjx
  !
  INTEGER,PARAMETER ::  jpreci=1,jprecj=1
  !
  ! les dimensions de la memoire du modele et du calculateur (T3E)
  !
  REAL(KIND=4) ::  ppmpt ,   &
       ppmcal = 1000000000., &
       ppmin  = 0.4,         &
       ppmax  = 1.0
  ! Aleph
  !     PARAMETER(ppmcal=16000000.)
  !Brodie
  !     PARAMETER(ppmcal=250000000.)
  ! Uqbar
  !     PARAMETER(ppmcal=3750000000.)
  ! Zahir
  !     PARAMETER(ppmcal=1000000000.)
  NAMELIST /namparam/ ppmcal, ppmin, ppmax


  !
  INTEGER,PARAMETER ::  iumout=8, numnam=4, iumbat=11
  INTEGER           :: ji,jj,jn,jni,jnj,jni2,jnj2
  INTEGER           ::  ifreq,il1,il2
  INTEGER           :: ii,iim,ij,ijm,imoy,iost,iresti,irestj,isurf,ivide
  INTEGER           :: iilb,ijlb,ireci,irecj,in
  INTEGER           :: ipi,ipj
  INTEGER           :: inf10,inf30,inf50,iptx,isw
  INTEGER           :: iii,iij,iiii,iijj,iimoy,iinf10,iinf30,iinf50
  !
  INTEGER,DIMENSION(:,:),ALLOCATABLE     ::  ibathy    ! jpidta -jpjdta
  INTEGER,DIMENSION(:,:),ALLOCATABLE     ::  ippdi, ippdj ,iidom, ijdom
  INTEGER,DIMENSION(:)  ,ALLOCATABLE     :: nlei, nldi,nlej,nldj,ICOUNT
  INTEGER,DIMENSION(:)  ,ALLOCATABLE     :: nleiv, nldiv,nlejv,nldjv
  INTEGER           :: jjc, nizoom, njzoom
  !
  REAL(KIND=4) :: zmin,zmax,zper,zmem
  REAL(KIND=4) :: zzmin,zzmax,zperx
  REAL(KIND=4),DIMENSION(:,:),ALLOCATABLE  ::  zmask, zdta, &   ! jpiglo -jpjglo
                   zlamt, zphit
  REAL(KIND=4),DIMENSION(:),ALLOCATABLE  ::  zdept   ! jpk
  LOGICAL :: llbon, lwp=.true.
  CHARACTER(LEN=80) ::  clvar
  INTEGER :: numout=6, itime, ipk, istep,  inum
  REAL(KIND=4) :: zdt, zdate0
  ! CDF stuff
  INTEGER :: ncid, ivarid, istatus

  !
  !
  !
  ! 0. Initialisation
  ! -----------------
  !
  OPEN(numnam,FILE='namelist')

  REWIND(numnam)
  READ(numnam,namspace)
  ALLOCATE ( ibathy(jpidta,jpjdta), zmask(jpiglo,jpjglo) ,zdta(jpidta,jpjdta))
  ALLOCATE ( zlamt(jpidta,jpjdta), zphit(jpidta,jpjdta))

  REWIND(numnam)
  READ(numnam,namparam)

  REWIND(numnam)
  READ(numnam,namproc)

  ppmpt = 55.+73./jpk
  jpnix = jprocx ; jpnjx=jprocx

  ALLOCATE (ippdi(jpnix,jpnjx), ippdj(jpnix,jpnjx) )
  ALLOCATE (iidom(jpnix,jpnjx), ijdom(jpnix,jpnjx) )
  ALLOCATE (nlei(jprocx), nldi(jprocx) )
  ALLOCATE (nlej(jprocx), nldj(jprocx) )
! empty processors
  ALLOCATE (nleiv(jprocx), nldiv(jprocx) )
  ALLOCATE (nlejv(jprocx), nldjv(jprocx) )
  ALLOCATE (ICOUNT(jprocx), zdept(jpk) )

  REWIND(numnam)
  READ(numnam,namfile) 

  REWIND(numnam)
  READ(numnam,namkeep)

  WRITE(iumout,*)
  WRITE(iumout,*) ' optimisation de la partition'
  WRITE(iumout,*) ' ----------------------------'
  WRITE(iumout,*)

  !
  ! Lecture de la bathymetrie
  !
      ! open the file
         IF ( ln_zps ) THEN
            clvar = 'Bathymetry'

         ELSE
            clvar = 'Bathy_level'
         ENDIF

         INQUIRE( FILE=cbathy, EXIST=llbon )
      IF( llbon ) THEN
            istatus=NF90_OPEN(cbathy,NF90_NOWRITE,ncid)
            istatus=NF90_INQ_VARID(ncid,clvar,ivarid)
            istatus=NF90_GET_VAR(ncid,ivarid,zdta)
            istatus=NF90_CLOSE(ncid)
      ELSE
          WRITE(numout,*)'    mppini_2 : unable to read the file', cbathy
      ENDIF

      ! land/sea mask over the global/zoom domain

!     imask(:,:)=1
!     WHERE ( zdta(jpizoom:jpiglo+jpizoom-1, jpjzoom:jpjglo+jpjzoom-1) == 0.e0 ) imask = 0
      ibathy(:,:)=zdta(:,:)

  DO jj=1,jpjglo
     DO ji=1,jpiglo
        zmask(ji,jj) = float(ibathy(ji+nizoom -1,jj+njzoom -1))
     END DO
  END DO
  DO jj=1,jpjglo
     DO ji=1,jpiglo
        zmask(ji,jj)= min(REAL(1.,kind=4),max(REAL(0.,kind=4),zmask(ji,jj)))
     END DO
  END DO
  print *,'Nombre de pts mer :', sum(zmask)
  !
  !
  !  1. Boucle sur le nombre de processeurs
  ! ---------------------------------------
  !
  iii=1
  iij=1
  iiii=jpiglo
  iijj=jpjglo
  iptx=0
  iimoy=0
  zzmin=0.
  zzmax=0.
  iinf10=0
  iinf30=0
  iinf50=0
  zperx=1.
  in=0
! Next loop corresponds to just 1 case, which is the one kept from mpp_optimize.
  DO jni=jpni,jpni
     DO jnj=jpnj,jpnj
        !
        ! Limitation nombre de pe
        !
        IF(jni*jnj.GT.jprocx) go to 1000
        !
        ! Partition
        !
        ipi=(jpiglo-2*jpreci + (jni-1))/jni + 2*jpreci
        ipj=(jpjglo-2*jprecj + (jnj-1))/jnj + 2*jprecj
        !
        ! Optimisation memoire ?
        !
        isw=0
        zmem=ppmpt*ipi*ipj*jpk + jpiglo*jpjglo
        IF(zmem.GT.ppmcal) go to 1000
        IF(jpmem.EQ.1) THEN
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
        jjc=0
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
                 nldiv(ivide)=jpreci+iilb
                 nleiv(ivide)=iilb+ippdi(jni2,jnj2)-1-jpreci
                 nldjv(ivide)=jprecj+ijlb
                 nlejv(ivide)=ijlb+ippdj(jni2,jnj2)-1-jprecj
              ELSE
                 imoy=imoy+isurf
                 jjc=jjc+1
                 icount(jjc)=isurf 
                 nldi(jjc)=jpreci+iilb
                 nlei(jjc)=iilb+ippdi(jni2,jnj2)-1-jpreci
                 nldj(jjc)=jprecj+ijlb
                 nlej(jjc)=ijlb+ippdj(jni2,jnj2)-1-jprecj
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
        WRITE(iumout,*) ' nb de p      30 < nb < 50 % ',inf50-inf10-inf30
        WRITE(iumout,*) ' nombre de points integres   ',(jni*jnj-ivide)*ipi*ipj
        WRITE(iumout,*) ' nbr de pts supplementaires  ',(jni*jnj-ivide)*ipi*ipj-jpiglo*jpjglo
        zper=float((jni*jnj-ivide))*float(ipi*ipj)/float(jpiglo*jpjglo)
        WRITE(iumout,*) ' % sup                       ',zper
        WRITE(iumout,*)
        WRITE(iumout,*) ' PROCESSORS WITH LESS THAN 100 WATER POINTS'

        WRITE(cdum,'(a,1h-,i3.3,1hx,i3.3,1h_,i3.3)') TRIM(covdta),jpni,jpnj,jni*jnj -ivide
        OPEN (10,file=cdum)
        WRITE(10,'(a,i5)')'#',jni*jnj -ivide
        DO jjc=1,jni*jnj-ivide
           WRITE(10,'(a,i5)')'#',jjc
           WRITE(10,'(2i5)')nldi(jjc)-1+nizoom-1,nldj(jjc)-1+njzoom -1
           WRITE(10,'(2i5)')nlei(jjc)+1+nizoom-1,nldj(jjc)-1+njzoom -1
           WRITE(10,'(2i5)')nlei(jjc)+1+nizoom-1,nlej(jjc)+1+njzoom -1
           WRITE(10,'(2i5)')nldi(jjc)-1+nizoom-1,nlej(jjc)+1+njzoom -1
           WRITE(10,'(2i5)')nldi(jjc)-1+nizoom-1,nldj(jjc)-1+njzoom -1
           WRITE(10,'(2i5)') 9999,9999 
           IF (icount(jjc).LT.100) THEN
              WRITE(iumout,*)' proc ji=',jjc,' water points:', icount(jjc)
              WRITE(iumout,*) ' ji from ',nldi(jjc), ' to :',nlei(jjc)
              WRITE(iumout,*) ' jj /  mask value for all ji'
              DO jj=nldj(jjc),nlej(jjc)
                 WRITE(iumout,900) jj,(INT(zmask(ji,jj)),ji=nldi(jjc),nlei(jjc))
              ENDDO
900           FORMAT(1x,i4,1x,9(10i1,1x))
           ENDIF
        ENDDO
        WRITE(10,'(a,i5)')'# vides:',ivide
        DO jjc=1,ivide
           WRITE(10,'(a,i5)')'# vide ',jjc
           WRITE(10,'(2i5)')nldiv(jjc)-1+nizoom-1,nldjv(jjc)-1+njzoom -1
           WRITE(10,'(2i5)')nleiv(jjc)+1+nizoom-1,nldjv(jjc)-1+njzoom -1
           WRITE(10,'(2i5)')nleiv(jjc)+1+nizoom-1,nlejv(jjc)+1+njzoom -1
           WRITE(10,'(2i5)')nldiv(jjc)-1+nizoom-1,nlejv(jjc)+1+njzoom -1
           WRITE(10,'(2i5)')nldiv(jjc)-1+nizoom-1,nldjv(jjc)-1+njzoom -1
           WRITE(10,'(2i5)')nleiv(jjc)+1+nizoom-1,nlejv(jjc)+1+njzoom -1
           WRITE(10,'(2i5)')nldiv(jjc)-1+nizoom-1,nlejv(jjc)+1+njzoom -1
           WRITE(10,'(2i5)')nleiv(jjc)+1+nizoom-1,nldjv(jjc)-1+njzoom -1
           WRITE(10,'(2i5)') 9999,9999
        END DO

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
1000    CONTINUE
     END DO
  END DO
  !
  !
  ! 6. Affichage resultat
  ! ---------------------
  !
  IF(in.EQ.0) THEN
     WRITE(iumout,*) ' le choix n a pas pu etre fait '
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
  WRITE(iumout,*) ' nb de p      30 < nb < 50 % ',iinf50-iinf10-iinf30
  WRITE(iumout,*) ' nombre de points integres   ',(iii*iij-iptx)*iiii*iijj
  WRITE(iumout,*) ' nbr de pts supplementaires  ',(iii*iij-iptx)*iiii*iijj-jpiglo*jpjglo
  WRITE(iumout,*) ' % sup                       ',zperx
  WRITE(iumout,*)
  !
  !
  !
  STOP
END PROGRAM mppopt_showproc_nc
