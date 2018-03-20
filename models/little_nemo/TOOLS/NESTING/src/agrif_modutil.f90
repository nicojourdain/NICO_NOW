!
MODULE agrif_modutil
  !
CONTAINS
  !
  !************************************************************************
  ! 									*
  ! MODULE  AGRIF_MODUTIL							*
  !									*
  ! module containing subroutine used for : 				*
  !   - unrolling 2D arrays to 1D arrays (required for SCRIP package use)	*
  !   - convert 1D arrays to 2D arrays (required for SCRIP package use)	*
  !   - remapping process (use SCRIP remapping matrix)			*
  !									*
  !************************************************************************
  ! 
  !***********************************************************    
  SUBROUTINE ssort (x, nb)
    !***********************************************************
    !
    IMPLICIT NONE

    INTEGER :: nb
    REAL*8, DIMENSION(:) :: x
    REAL*8 :: temp
    INTEGER ji,jj,jmax,itemp
    !      
    jmax=nb-1
    !
    !
    DO ji=1,nb-1
       temp=HUGE(1)

       DO jj=1,jmax

          IF(X(jj).LE.X(jj+1)) THEN 
             temp=X(jj)
             X(jj)=X(jj+1)
             X(jj+1)=temp
          ENDIF

       ENDDO

       IF(temp.EQ.HUGE(1)) RETURN
       jmax=jmax-1
    ENDDO

    RETURN
  END SUBROUTINE ssort
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !************************************************************************
  !   SUBROUTINE 1Dto2D
  !************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE tab1Dto2D(tab1D,tab2D,nx,ny)
    !
    IMPLICIT NONE   
    !
    REAL*8,DIMENSION(:) :: tab1D
    REAL*8,DIMENSION(:,:) :: tab2D
    !          
    INTEGER :: xpos,ypos
    INTEGER :: nx,ny   
    INTEGER :: i      
    !      
    xpos=0
    ypos=1
    !      
    DO i=1,nx*ny
       xpos=xpos+1
       IF(xpos.GT.nx) THEN
          xpos=1
          ypos=ypos+1
       ENDIF
       tab2D(ypos,xpos)=tab1D(i)
    END DO
    !
  END SUBROUTINE tab1Dto2D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !************************************************************************
  !   SUBROUTINE tab2Dto1D
  !************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE tab2Dto1D(tab2D,tab1D)
    !
    IMPLICIT NONE 
    !       
    REAL*8,DIMENSION(:,:) :: tab2D
    REAL*8,DIMENSION(:) :: tab1D
    !          
    INTEGER :: xpos,ypos
    INTEGER :: nx,ny
    INTEGER :: i
    !
    nx = SIZE(tab2D,2)
    ny = SIZE(tab2D,1)
    !      
    xpos = 0
    ypos = 1
    DO i = 1,nx*ny
       xpos = xpos + 1
       IF(xpos.GT.nx) THEN
          xpos = 1
          ypos = ypos + 1
       END IF
       tab1D(i) = tab2D(ypos,xpos)
    END DO
    !
  END SUBROUTINE tab2Dto1D


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !************************************************************************
  !   SUBROUTINE tab2Dto1D logical
  !************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE logtab2Dto1D(tab2D,tab1D)
    !
    IMPLICIT NONE 
    !       
    LOGICAL,DIMENSION(:,:) :: tab2D
    LOGICAL,DIMENSION(:) :: tab1D
    !          
    INTEGER :: xpos,ypos
    INTEGER :: nx,ny
    INTEGER :: i
    !
    nx = SIZE(tab2D,2)
    ny = SIZE(tab2D,1)
    !      
    xpos = 0
    ypos = 1
    DO i = 1,nx*ny
       xpos = xpos + 1
       IF(xpos.GT.nx) THEN
          xpos = 1
          ypos = ypos + 1
       END IF
       tab1D(i) = tab2D(ypos,xpos)
    END DO
    !
  END SUBROUTINE logtab2Dto1D
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !************************************************************************
  !   SUBROUTINE 1Dto2D
  !************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE logtab1Dto2D(tab1D,tab2D,nx,ny)
    !
    IMPLICIT NONE   
    !
    LOGICAL,DIMENSION(:) :: tab1D
    LOGICAL,DIMENSION(:,:) :: tab2D
    !          
    INTEGER :: xpos,ypos
    INTEGER :: nx,ny   
    INTEGER :: i      
    !      
    xpos=0
    ypos=1
    !      
    DO i=1,nx*ny
       xpos=xpos+1
       IF(xpos.GT.nx) THEN
          xpos=1
          ypos=ypos+1
       ENDIF
       tab2D(ypos,xpos)=tab1D(i)
    END DO
    !
  END SUBROUTINE logtab1Dto2D

  !**************************************************************
  !   subroutine make_remap
  !**************************************************************            
  !
  SUBROUTINE make_remap(tabin,tabout,nxfin,nyfin,matrix,src_add,dst_add) 
    !      
    IMPLICIT NONE
    !      
    REAL*8, DIMENSION(:,:)  :: tabin 
    REAL*8, DIMENSION(:,:) :: tabout 
    REAL*8, POINTER, DIMENSION(:,:) :: tabtemp    
    INTEGER,DIMENSION(:) :: src_add,dst_add
    INTEGER :: nxfin,nyfin      
    REAL*8, POINTER, DIMENSION(:) :: var1D,var_interp1D
    REAL*8,DIMENSION(:,:) :: matrix 
    INTEGER :: num_links,i
    ! 
    ALLOCATE(var1D(SIZE(tabin,1)*SIZE(tabin,2)))     
    CALL tab2Dto1D(tabin,var1D)
    !      
    ALLOCATE(var_interp1D(nxfin*nyfin))
    var_interp1D = 0.0
    num_links = SIZE(dst_add)
    !
    DO i = 1,num_links
       var_interp1D(dst_add(i)) = var_interp1D(dst_add(i)) &
            + matrix(1,i)*var1D(src_add(i))
    END DO
    !
    ALLOCATE(tabtemp(SIZE(tabout,1),SIZE(tabout,2)))
    !
    CALL tab1Dto2D(var_interp1D,tabtemp,nyfin,nxfin)
    !
    tabout = tabtemp
    !
    DEALLOCATE(var_interp1D,var1D,tabtemp)  
    ! 
  END SUBROUTINE make_remap
  !          
  !
  !**************************************************************
  !   end subroutine make_remap
  !**************************************************************          
  !
  !
  !**************************************************************
  !   subroutine make_bicubic_remap
  !**************************************************************            
  !
  SUBROUTINE make_bicubic_remap(tabin,masksrc,tabout,nxfin,nyfin,matrix,src_add,dst_add) 
    !      
    IMPLICIT NONE
    !      
    REAL*8, DIMENSION(:,:)  :: tabin
    LOGICAL, DIMENSION(:,:)  :: masksrc
    LOGICAL, POINTER, DIMENSION(:)  :: grid1_mask
    REAL*8, DIMENSION(:,:) :: tabout     
    INTEGER,DIMENSION(:) :: src_add,dst_add
    INTEGER :: nxfin,nyfin      
    REAL*8, POINTER, DIMENSION(:) :: var1D,var_interp1D,gradi,gradj,gradij,deriv1,deriv2
    REAL*8,DIMENSION(:,:) :: matrix 
    INTEGER :: num_links,i,j,nx,ny,n,ip1,im1,jp1,jm1
    INTEGER :: in,is,ie,iw,ine,inw,ise,isw
    REAL*8 :: delew,delns
    !
    nx = SIZE(tabin,1)
    ny = SIZE(tabin,2)
    ALLOCATE(gradi(nx*ny),gradj(nx*ny),gradij(nx*ny),deriv1(nx*ny),deriv2(nx*ny))
    ALLOCATE(var1D(nx*ny),grid1_mask(nx*ny))
    !           
    CALL tab2Dto1D(tabin,var1D)
    CALL logtab2Dto1D(masksrc,grid1_mask)     
    !
    gradi  = 0.0
    gradj  = 0.0
    gradij = 0.0
    !
    DO n = 1,nx*ny

       IF( grid1_mask(n) ) THEN                        
          !                     
          delew = 0.5      
          delns = 0.5

          j = (n-1)/ny + 1
          i = n - (j-1)*ny
          !
          ip1 = i+1
          im1 = i-1
          jp1 = j+1
          jm1 = j-1      
          !
          IF (ip1 > ny) ip1 = ip1 - ny

          IF (im1 < 1 ) im1 = ny

          IF (jp1 > nx) THEN
             jp1 = j
             delns = 1.
          ENDIF

          IF (jm1 < 1 ) THEN
             jm1 = j
             delns = 1.
          ENDIF
          !
          in  = (jp1-1)*ny + i
          is  = (jm1-1)*ny + i
          ie  = (j  -1)*ny + ip1
          iw  = (j  -1)*ny + im1
          !
          ine = (jp1-1)*ny + ip1
          inw = (jp1-1)*ny + im1
          ise = (jm1-1)*ny + ip1
          isw = (jm1-1)*ny + im1
          !
          !*** compute i-gradient

          IF (.NOT. grid1_mask(ie)) THEN
             ie = n
             delew = 1.
          ENDIF
          !            
          IF (.NOT. grid1_mask(iw)) THEN
             iw = n
             delew = 1.
          ENDIF
          !
          gradi(n) = delew*(var1D(ie) - var1D(iw))
          !
          !*** compute j-gradient

          IF (.NOT. grid1_mask(in)) THEN
             in = n
             delns = 1.
          ENDIF
          !                      
          IF (.NOT. grid1_mask(is)) THEN
             is = n
             delns = 1.
          ENDIF
          !
          gradj(n) = delns*(var1D(in) - var1D(is))                    
          !
          !*** compute ij-gradient

          delew = 0.5

          IF (jp1 == j .OR. jm1 == j) THEN
             delns = 1.
          ELSE 
             delns = 0.5
          ENDIF
          !
          IF (.NOT. grid1_mask(ine)) THEN
             IF (in /= n) THEN
                ine = in
                delew = 1.
             ELSE IF (ie /= n) THEN
                ine = ie
                inw = iw
                IF (inw == n) delew = 1.
                delns = 1.
             ELSE
                ine = n
                inw = iw
                delew = 1
                delns = 1
             ENDIF
          ENDIF
          !
          IF (.NOT. grid1_mask(inw)) THEN
             IF (in /= n) THEN
                inw = in
                delew = 1.
             ELSE IF (iw /= n) THEN
                inw = iw
                ine = ie
                IF (ie == n) delew = 1.
                delns = 1.
             ELSE
                inw = n
                ine = ie
                delew = 1.
                delns = 1.
             ENDIF
          ENDIF
          !
          deriv1(n) = delew*(var1D(ine)-var1D(inw))                    
          !
          IF (.NOT. grid1_mask(ise)) THEN
             IF (is /= n) THEN
                ise = is
                delew = 1.
             ELSE IF (ie /= n) THEN
                ise = ie
                isw = iw
                IF (isw == n) delew = 1.
                delns = 1.
             ELSE
                ise = n
                isw = iw
                delew = 1.
                delns = 1.
             ENDIF
          ENDIF
          !
          IF (.NOT. grid1_mask(isw)) THEN
             IF (is /= n) THEN
                isw = is
                delew = 1.
             ELSE IF (iw /= n) THEN
                isw = iw
                ise = ie
                IF (ie == n) delew = 1.
                delns = 1.
             ELSE
                isw = n
                ise = ie
                delew = 1.
                delns = 1.
             ENDIF
          ENDIF

          deriv2(n) = delew*(var1D(ise) - var1D(isw))
          gradij(n) = delns*(deriv1(n) - deriv2(n))
       ENDIF
    END DO
    !
    DEALLOCATE(deriv1,deriv2,grid1_mask)

    !       
    ALLOCATE(var_interp1D(nxfin*nyfin))
    var_interp1D = 0.0
    num_links = SIZE(dst_add)
    !      
    DO i = 1,num_links
       !        
       var_interp1D(dst_add(i)) = var_interp1D(dst_add(i))        +      &
            matrix(1,i)*var1D(src_add(i))   +      &
            matrix(2,i)*gradi(src_add(i))   +      &
            matrix(3,i)*gradj(src_add(i))   +      & 
            matrix(4,i)*gradij(src_add(i))
    END DO
    !
    DEALLOCATE(gradi,gradj,gradij,var1D)
    !
    CALL tab1Dto2D(var_interp1D,tabout,nyfin,nxfin)
    !      
    DEALLOCATE(var_interp1D)       
    ! 
  END SUBROUTINE make_bicubic_remap
  !          
  !
  !**************************************************************
  !   end subroutine make_bicubic_remap
  !**************************************************************          
  !
  !
END MODULE agrif_modutil
