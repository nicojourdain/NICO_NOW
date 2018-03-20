
MODULE agrif_opa_interp
   !!======================================================================
   !!                   ***  MODULE  agrif_opa_interp  ***
   !! AGRIF: interpolation package
   !!======================================================================
   !! History :  2.0  !  2002-06  (XXX)  Original cade
   !!             -   !  2005-11  (XXX) 
   !!            3.2  !  2009-04  (R. Benshila) 
   !!----------------------------------------------------------------------
#if defined key_agrif && ! defined key_offline
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!   NOT 'key_offline'                               NO off-line tracers
   !!----------------------------------------------------------------------
   !!   Agrif_tra     :
   !!   Agrif_dyn     : 
   !!   interpu       :
   !!   interpv       :
   !!----------------------------------------------------------------------
   USE par_oce
   USE oce
   USE dom_oce      
   USE sol_oce
   USE agrif_oce
   USE phycst
   USE in_out_manager
   USE agrif_opa_sponge
   USE lib_mpp
   USE wrk_nemo  
   USE zdf_oce        ! vertical physics: ocean variables

   IMPLICIT NONE
   PRIVATE
    
   PUBLIC   Agrif_tra, Agrif_dyn, Agrif_ssh, Agrif_tke
   PUBLIC   interpu, interpv, interpe3t, interpavt, interpavm, interpavmu, interpavmv 

#  include "domzgr_substitute.h90"  
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/NST 3.3 , NEMO Consortium (2010)
   !! $Id: agrif_opa_interp.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

   CONTAINS
   
   SUBROUTINE Agrif_tra( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_Tra  ***
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt
      !!
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::   zrhox , alpha1, alpha2, alpha3
      REAL(wp) ::   alpha4, alpha5, alpha6, alpha7
      REAL(wp), POINTER, DIMENSION(:,:,:,:) :: ztsa
      !!----------------------------------------------------------------------
      !
      IF( Agrif_Root() )   RETURN

      IF( kt == nit000 ) CALL Agrif_e3t

      CALL wrk_alloc( jpi, jpj, jpk, jpts, ztsa ) 

      Agrif_SpecialValue    = 0.e0
      Agrif_UseSpecialValue = .TRUE.
      ztsa(:,:,:,:) = 0.e0

      CALL Agrif_Bc_variable( ztsa, tsn_id, procname=interptsn )
      Agrif_UseSpecialValue = .FALSE.

      zrhox = Agrif_Rhox()     ! if = 3 :

      alpha1 = ( zrhox - 1. ) * 0.5    ! (3-1)/2 = 1
      alpha2 = 1. - alpha1             ! 0

      alpha3 = ( zrhox - 1. ) / ( zrhox + 1. )  ! (3-1)/(3+1) = 0.5
      alpha4 = 1. - alpha3                      ! 0.5

      alpha6 = 2. * ( zrhox - 1. ) / ( zrhox + 1. )   ! 2*(3-1)/(3+1) = 1
      alpha7 =    - ( zrhox - 1. ) / ( zrhox + 3. )   ! - (3-1)/(3+3) = -1/3
      alpha5 = 1. - alpha6 - alpha7                   ! 1-1+1/3 = 1/3

      IF( nbondi == 1 .OR. nbondi == 2 ) THEN

         DO jn = 1, jpts
            tsa(nlci,:,:,jn) = alpha1 * ztsa(nlci,:,:,jn) + alpha2 * ztsa(nlci-1,:,:,jn)
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  IF( umask(nlci-2,jj,jk) == 0.e0 ) THEN
                     tsa(nlci-1,jj,jk,jn) = tsa(nlci,jj,jk,jn) * tmask(nlci-1,jj,jk)
                  ELSE
                     tsa(nlci-1,jj,jk,jn)=(alpha4*tsa(nlci,jj,jk,jn)+alpha3*tsa(nlci-2,jj,jk,jn))*tmask(nlci-1,jj,jk)
                     IF( un(nlci-2,jj,jk) > 0.e0 ) THEN
                        tsa(nlci-1,jj,jk,jn)=( alpha6*tsa(nlci-2,jj,jk,jn)+alpha5*tsa(nlci,jj,jk,jn)  &
                           &                 + alpha7*tsa(nlci-3,jj,jk,jn) ) * tmask(nlci-1,jj,jk)
                     ENDIF
                  ENDIF
               END DO
            END DO
         ENDDO
      ENDIF

      IF( nbondj == 1 .OR. nbondj == 2 ) THEN

         DO jn = 1, jpts
            tsa(:,nlcj,:,jn) = alpha1 * ztsa(:,nlcj,:,jn) + alpha2 * ztsa(:,nlcj-1,:,jn)
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  IF( vmask(ji,nlcj-2,jk) == 0.e0 ) THEN
                     tsa(ji,nlcj-1,jk,jn) = tsa(ji,nlcj,jk,jn) * tmask(ji,nlcj-1,jk)
                  ELSE
                     tsa(ji,nlcj-1,jk,jn)=(alpha4*tsa(ji,nlcj,jk,jn)+alpha3*tsa(ji,nlcj-2,jk,jn))*tmask(ji,nlcj-1,jk)        
                     IF (vn(ji,nlcj-2,jk) > 0.e0 ) THEN
                        tsa(ji,nlcj-1,jk,jn)=( alpha6*tsa(ji,nlcj-2,jk,jn)+alpha5*tsa(ji,nlcj,jk,jn)  &
                           &                 + alpha7*tsa(ji,nlcj-3,jk,jn) ) * tmask(ji,nlcj-1,jk)
                     ENDIF
                  ENDIF
               END DO
            END DO
         ENDDO 
      ENDIF

      IF( nbondi == -1 .OR. nbondi == 2 ) THEN   ! west
         DO jn = 1, jpts
            tsa(1,:,:,jn) = alpha1 * ztsa(1,:,:,jn) + alpha2 * ztsa(2,:,:,jn)  ! tsa(1,:,:,jn) = ztsa(1,:,:,jn)
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  IF( umask(2,jj,jk) == 0.e0 ) THEN
                     tsa(2,jj,jk,jn) = tsa(1,jj,jk,jn) * tmask(2,jj,jk)
                  ELSE
                     tsa(2,jj,jk,jn)=(alpha4*tsa(1,jj,jk,jn)+alpha3*tsa(3,jj,jk,jn))*tmask(2,jj,jk) ! tsa1(2) = (tsa(1)+tsa(3))/2  
                     IF( un(2,jj,jk) < 0.e0 ) THEN ! if outgoing current: tsa1(2) = tsa1(3)+1/3*tsa1(1)-1/3*tsa(4)
                        tsa(2,jj,jk,jn)=(alpha6*tsa(3,jj,jk,jn)+alpha5*tsa(1,jj,jk,jn)+alpha7*tsa(4,jj,jk,jn))*tmask(2,jj,jk)
                     ENDIF
                  ENDIF
               END DO
            END DO
         END DO
      ENDIF

      IF( nbondj == -1 .OR. nbondj == 2 ) THEN
         DO jn = 1, jpts
            tsa(:,1,:,jn) = alpha1 * ztsa(:,1,:,jn) + alpha2 * ztsa(:,2,:,jn)
            DO jk=1,jpk      
               DO ji=1,jpi
                  IF( vmask(ji,2,jk) == 0.e0 ) THEN
                     tsa(ji,2,jk,jn)=tsa(ji,1,jk,jn) * tmask(ji,2,jk)
                  ELSE
                     tsa(ji,2,jk,jn)=(alpha4*tsa(ji,1,jk,jn)+alpha3*tsa(ji,3,jk,jn))*tmask(ji,2,jk)
                     IF( vn(ji,2,jk) < 0.e0 ) THEN
                        tsa(ji,2,jk,jn)=(alpha6*tsa(ji,3,jk,jn)+alpha5*tsa(ji,1,jk,jn)+alpha7*tsa(ji,4,jk,jn))*tmask(ji,2,jk)
                     ENDIF
                  ENDIF
               END DO
            END DO
         ENDDO
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, jpk, jpts, ztsa ) 
      !
   END SUBROUTINE Agrif_tra


   SUBROUTINE Agrif_dyn( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_DYN  ***
      !!----------------------------------------------------------------------  
      !! 
      INTEGER, INTENT(in) ::   kt
      !!
      INTEGER :: ji,jj,jk
      REAL(wp) :: timeref
      REAL(wp) :: z2dt, znugdt
      REAL(wp) :: zrhox, rhoy
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zua, zva
      REAL(wp), POINTER, DIMENSION(:,:) :: spgv1, spgu1, zua2d, zva2d
      !!----------------------------------------------------------------------  

      IF( Agrif_Root() )   RETURN

      CALL wrk_alloc( jpi, jpj, spgv1, spgu1, zua2d, zva2d )
      CALL wrk_alloc( jpi, jpj, jpk, zua, zva )

      zrhox = Agrif_Rhox()
      rhoy = Agrif_Rhoy()

      timeref = 1.

      ! time step: leap-frog
      z2dt = 2. * rdt
      ! time step: Euler if restart from rest
      IF( neuler == 0 .AND. kt == nit000 ) z2dt = rdt
      ! coefficients
      znugdt =  grav * z2dt    

      Agrif_SpecialValue=0.
      Agrif_UseSpecialValue = ln_spc_dyn

      zua = 0.
      zva = 0.
      CALL Agrif_Bc_variable(zua,un_id,procname=interpu)   ! zua = zonal tansport at now time: e2u*e3u*un
      CALL Agrif_Bc_variable(zva,vn_id,procname=interpv)
      zua2d = 0.
      zva2d = 0.

      Agrif_SpecialValue=0.
      Agrif_UseSpecialValue = ln_spc_dyn
      CALL Agrif_Bc_variable(zua2d,e1u_id,calledweight=1.,procname=interpu2d)! zua2d = zonal gradient of temporal derivative of eta
      CALL Agrif_Bc_variable(zva2d,e2v_id,calledweight=1.,procname=interpv2d)
      Agrif_UseSpecialValue = .FALSE.


      IF((nbondi == -1).OR.(nbondi == 2)) THEN   ! west

         DO jj=1,jpj
            laplacu(2,jj) = timeref * (zua2d(2,jj)/(rhoy*e2u(2,jj)))*umask(2,jj,1)
         END DO

         DO jk=1,jpkm1     ! move back zonal transport to zonal current
            DO jj=1,jpj
               ua(1:2,jj,jk) = (zua(1:2,jj,jk)/(rhoy*e2u(1:2,jj)))
               ua(1:2,jj,jk) = ua(1:2,jj,jk) / fse3u(1:2,jj,jk)
            END DO
         END DO

         DO jk=1,jpkm1
            DO jj=1,jpj
               ua(2,jj,jk) = (ua(2,jj,jk) - z2dt * znugdt * laplacu(2,jj))*umask(2,jj,jk)
            END DO
         END DO

         spgu(2,:)=0.

         DO jk=1,jpkm1
            DO jj=1,jpj
               spgu(2,jj)=spgu(2,jj)+fse3u(2,jj,jk)*ua(2,jj,jk)
            END DO
         END DO

         DO jj=1,jpj
            IF (umask(2,jj,1).NE.0.) THEN
               spgu(2,jj)=spgu(2,jj)/hu(2,jj)
            ENDIF
         END DO

         DO jk=1,jpkm1   ! 1/4 1/2 1/4 filter
            DO jj=1,jpj
               ua(2,jj,jk) = 0.25*(ua(1,jj,jk)+2.*ua(2,jj,jk)+ua(3,jj,jk))
               ua(2,jj,jk) = ua(2,jj,jk) * umask(2,jj,jk)
            END DO
         END DO

         spgu1(2,:)=0.

         DO jk=1,jpkm1
            DO jj=1,jpj
               spgu1(2,jj)=spgu1(2,jj)+fse3u(2,jj,jk)*ua(2,jj,jk)
            END DO
         END DO

         DO jj=1,jpj
            IF (umask(2,jj,1).NE.0.) THEN
               spgu1(2,jj)=spgu1(2,jj)/hu(2,jj)
            ENDIF
         END DO

         DO jk=1,jpkm1
            DO jj=1,jpj
               ua(2,jj,jk) = (ua(2,jj,jk)+spgu(2,jj)-spgu1(2,jj))*umask(2,jj,jk)
            END DO
         END DO

         DO jk=1,jpkm1
            DO jj=1,jpj
               va(2,jj,jk) = (zva(2,jj,jk)/(zrhox*e1v(2,jj)))*vmask(2,jj,jk)
               va(2,jj,jk) = va(2,jj,jk) / fse3v(2,jj,jk)
            END DO
         END DO

      ENDIF

      IF((nbondi == 1).OR.(nbondi == 2)) THEN

         DO jj=1,jpj
            laplacu(nlci-2,jj) = timeref * (zua2d(nlci-2,jj)/(rhoy*e2u(nlci-2,jj)))
         END DO

         DO jk=1,jpkm1
            DO jj=1,jpj
               ua(nlci-2:nlci-1,jj,jk) = (zua(nlci-2:nlci-1,jj,jk)/(rhoy*e2u(nlci-2:nlci-1,jj)))

               ua(nlci-2:nlci-1,jj,jk) = ua(nlci-2:nlci-1,jj,jk) / fse3u(nlci-2:nlci-1,jj,jk)

            END DO
         END DO

         DO jk=1,jpkm1
            DO jj=1,jpj
               ua(nlci-2,jj,jk) = (ua(nlci-2,jj,jk)- z2dt * znugdt * laplacu(nlci-2,jj))*umask(nlci-2,jj,jk)
            END DO
         END DO


         spgu(nlci-2,:)=0.

         do jk=1,jpkm1
            do jj=1,jpj
               spgu(nlci-2,jj)=spgu(nlci-2,jj)+fse3u(nlci-2,jj,jk)*ua(nlci-2,jj,jk)
            enddo
         enddo

         DO jj=1,jpj
            IF (umask(nlci-2,jj,1).NE.0.) THEN
               spgu(nlci-2,jj)=spgu(nlci-2,jj)/hu(nlci-2,jj)
            ENDIF
         END DO

         DO jk=1,jpkm1
            DO jj=1,jpj
               ua(nlci-2,jj,jk) = 0.25*(ua(nlci-3,jj,jk)+2.*ua(nlci-2,jj,jk)+ua(nlci-1,jj,jk))

               ua(nlci-2,jj,jk) = ua(nlci-2,jj,jk) * umask(nlci-2,jj,jk)

            END DO
         END DO

         spgu1(nlci-2,:)=0.

         DO jk=1,jpkm1
            DO jj=1,jpj
               spgu1(nlci-2,jj)=spgu1(nlci-2,jj)+fse3u(nlci-2,jj,jk)*ua(nlci-2,jj,jk)*umask(nlci-2,jj,jk)
            END DO
         END DO

         DO jj=1,jpj
            IF (umask(nlci-2,jj,1).NE.0.) THEN
               spgu1(nlci-2,jj)=spgu1(nlci-2,jj)/hu(nlci-2,jj)
            ENDIF
         END DO

         DO jk=1,jpkm1
            DO jj=1,jpj
               ua(nlci-2,jj,jk) = (ua(nlci-2,jj,jk)+spgu(nlci-2,jj)-spgu1(nlci-2,jj))*umask(nlci-2,jj,jk)
            END DO
         END DO

         DO jk=1,jpkm1
            DO jj=1,jpj-1
               va(nlci-1,jj,jk) = (zva(nlci-1,jj,jk)/(zrhox*e1v(nlci-1,jj)))*vmask(nlci-1,jj,jk)
               va(nlci-1,jj,jk) = va(nlci-1,jj,jk) / fse3v(nlci-1,jj,jk)
            END DO
         END DO

      ENDIF

      IF((nbondj == -1).OR.(nbondj == 2)) THEN

         DO ji=1,jpi
            laplacv(ji,2) = timeref * (zva2d(ji,2)/(zrhox*e1v(ji,2)))
         END DO

         DO jk=1,jpkm1
            DO ji=1,jpi
               va(ji,1:2,jk) = (zva(ji,1:2,jk)/(zrhox*e1v(ji,1:2)))
               va(ji,1:2,jk) = va(ji,1:2,jk) / fse3v(ji,1:2,jk)
            END DO
         END DO

         DO jk=1,jpkm1
            DO ji=1,jpi
               va(ji,2,jk) = (va(ji,2,jk) - z2dt * znugdt * laplacv(ji,2))*vmask(ji,2,jk)
            END DO
         END DO

         spgv(:,2)=0.

         DO jk=1,jpkm1
            DO ji=1,jpi
               spgv(ji,2)=spgv(ji,2)+fse3v(ji,2,jk)*va(ji,2,jk)
            END DO
         END DO

         DO ji=1,jpi
            IF (vmask(ji,2,1).NE.0.) THEN
               spgv(ji,2)=spgv(ji,2)/hv(ji,2)
            ENDIF
         END DO

         DO jk=1,jpkm1
            DO ji=1,jpi
               va(ji,2,jk)=0.25*(va(ji,1,jk)+2.*va(ji,2,jk)+va(ji,3,jk))
               va(ji,2,jk)=va(ji,2,jk)*vmask(ji,2,jk)
            END DO
         END DO

         spgv1(:,2)=0.

         DO jk=1,jpkm1
            DO ji=1,jpi
               spgv1(ji,2)=spgv1(ji,2)+fse3v(ji,2,jk)*va(ji,2,jk)*vmask(ji,2,jk)
            END DO
         END DO

         DO ji=1,jpi
            IF (vmask(ji,2,1).NE.0.) THEN
               spgv1(ji,2)=spgv1(ji,2)/hv(ji,2)
            ENDIF
         END DO

         DO jk=1,jpkm1
            DO ji=1,jpi
               va(ji,2,jk) = (va(ji,2,jk)+spgv(ji,2)-spgv1(ji,2))*vmask(ji,2,jk)
            END DO
         END DO

         DO jk=1,jpkm1
            DO ji=1,jpi
               ua(ji,2,jk) = (zua(ji,2,jk)/(rhoy*e2u(ji,2)))*umask(ji,2,jk) 
               ua(ji,2,jk) = ua(ji,2,jk) / fse3u(ji,2,jk)
            END DO
         END DO

      ENDIF

      IF((nbondj == 1).OR.(nbondj == 2)) THEN

         DO ji=1,jpi
            laplacv(ji,nlcj-2) = timeref * (zva2d(ji,nlcj-2)/(zrhox*e1v(ji,nlcj-2)))
         END DO

         DO jk=1,jpkm1
            DO ji=1,jpi
               va(ji,nlcj-2:nlcj-1,jk) = (zva(ji,nlcj-2:nlcj-1,jk)/(zrhox*e1v(ji,nlcj-2:nlcj-1)))
               va(ji,nlcj-2:nlcj-1,jk) = va(ji,nlcj-2:nlcj-1,jk) / fse3v(ji,nlcj-2:nlcj-1,jk)
            END DO
         END DO

         DO jk=1,jpkm1
            DO ji=1,jpi
               va(ji,nlcj-2,jk) = (va(ji,nlcj-2,jk)-z2dt * znugdt * laplacv(ji,nlcj-2))*vmask(ji,nlcj-2,jk)
            END DO
         END DO


         spgv(:,nlcj-2)=0.

         DO jk=1,jpkm1
            DO ji=1,jpi
               spgv(ji,nlcj-2)=spgv(ji,nlcj-2)+fse3v(ji,nlcj-2,jk)*va(ji,nlcj-2,jk)
            END DO
         END DO

         DO ji=1,jpi
            IF (vmask(ji,nlcj-2,1).NE.0.) THEN
               spgv(ji,nlcj-2)=spgv(ji,nlcj-2)/hv(ji,nlcj-2)
            ENDIF
         END DO

         DO jk=1,jpkm1
            DO ji=1,jpi
               va(ji,nlcj-2,jk)=0.25*(va(ji,nlcj-3,jk)+2.*va(ji,nlcj-2,jk)+va(ji,nlcj-1,jk))
               va(ji,nlcj-2,jk) = va(ji,nlcj-2,jk) * vmask(ji,nlcj-2,jk)
            END DO
         END DO

         spgv1(:,nlcj-2)=0.

         DO jk=1,jpkm1
            DO ji=1,jpi
               spgv1(ji,nlcj-2)=spgv1(ji,nlcj-2)+fse3v(ji,nlcj-2,jk)*va(ji,nlcj-2,jk)
            END DO
         END DO

         DO ji=1,jpi
            IF (vmask(ji,nlcj-2,1).NE.0.) THEN
               spgv1(ji,nlcj-2)=spgv1(ji,nlcj-2)/hv(ji,nlcj-2)
            ENDIF
         END DO

         DO jk=1,jpkm1
            DO ji=1,jpi
               va(ji,nlcj-2,jk) = (va(ji,nlcj-2,jk)+spgv(ji,nlcj-2)-spgv1(ji,nlcj-2))*vmask(ji,nlcj-2,jk)
            END DO
         END DO

         DO jk=1,jpkm1
            DO ji=1,jpi
               ua(ji,nlcj-1,jk) = (zua(ji,nlcj-1,jk)/(rhoy*e2u(ji,nlcj-1)))*umask(ji,nlcj-1,jk)
               ua(ji,nlcj-1,jk) = ua(ji,nlcj-1,jk) / fse3u(ji,nlcj-1,jk)
            END DO
         END DO

      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, spgv1, spgu1, zua2d, zva2d )
      CALL wrk_dealloc( jpi, jpj, jpk, zua, zva )
      !
   END SUBROUTINE Agrif_dyn


   SUBROUTINE Agrif_ssh( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_ssh  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) ::   kt
      !!
      !!----------------------------------------------------------------------  

      IF( Agrif_Root() )   RETURN


      IF((nbondi == -1).OR.(nbondi == 2)) THEN
         ssha(2,:)=ssha(3,:)
         sshn(2,:)=sshn(3,:)
      ENDIF

      IF((nbondi == 1).OR.(nbondi == 2)) THEN
         ssha(nlci-1,:)=ssha(nlci-2,:)
         sshn(nlci-1,:)=sshn(nlci-2,:)        
      ENDIF

      IF((nbondj == -1).OR.(nbondj == 2)) THEN
         ssha(:,2)=sshn(:,3)
         sshn(:,2)=sshb(:,3)
      ENDIF

      IF((nbondj == 1).OR.(nbondj == 2)) THEN
         ssha(:,nlcj-1)=ssha(:,nlcj-2)
         ssha(:,nlcj-1)=sshn(:,nlcj-2)                
      ENDIF

   END SUBROUTINE Agrif_ssh


   SUBROUTINE Agrif_tke
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_tke  ***
      !!----------------------------------------------------------------------  
      REAL(wp), POINTER, DIMENSION(:,:,:) :: z3d
      !!----------------------------------------------------------------------  
      IF( Agrif_Root() )   RETURN

      CALL wrk_alloc( jpi, jpj, jpk, z3d )
      
      Agrif_SpecialValue    = 0.e0
      Agrif_UseSpecialValue = .TRUE.
      z3d(:,:,:) = 0.
      
      CALL Agrif_Bc_variable(z3d,avt_id,calledweight=1.,procname=interpavt)
      
      avt_k(mi0(       1):mi1(     2),:,:) = z3d(mi0(       1):mi1(     2),:,:)   ! west
      avt_k(mi0(jpiglo-1):mi1(jpiglo),:,:) = z3d(mi0(jpiglo-1):mi1(jpiglo),:,:)   ! east
      avt_k(:,mj0(       1):mj1(     2),:) = z3d(:,mj0(       1):mj1(     2),:)   ! south
      avt_k(:,mj0(jpjglo-1):mj1(jpjglo),:) = z3d(:,mj0(jpjglo-1):mj1(jpjglo),:)   ! north
       
      CALL Agrif_Bc_variable(z3d,avm_id,calledweight=1.,procname=interpavm)
      
      avm_k(mi0(       1):mi1(     2),:,:) = z3d(mi0(       1):mi1(     2),:,:)   ! west
      avm_k(mi0(jpiglo-1):mi1(jpiglo),:,:) = z3d(mi0(jpiglo-1):mi1(jpiglo),:,:)   ! east
      avm_k(:,mj0(       1):mj1(     2),:) = z3d(:,mj0(       1):mj1(     2),:)   ! south
      avm_k(:,mj0(jpjglo-1):mj1(jpjglo),:) = z3d(:,mj0(jpjglo-1):mj1(jpjglo),:)   ! north
       
      CALL Agrif_Bc_variable(z3d,avmu_id,calledweight=1.,procname=interpavmu)
      
      avmu_k(mi0(       1):mi1(     2),:,:) = z3d(mi0(       1):mi1(     2),:,:)   ! west
      avmu_k(mi0(jpiglo-1):mi1(jpiglo),:,:) = z3d(mi0(jpiglo-1):mi1(jpiglo),:,:)   ! east
      avmu_k(:,mj0(       1):mj1(     2),:) = z3d(:,mj0(       1):mj1(     2),:)   ! south
      avmu_k(:,mj0(jpjglo-1):mj1(jpjglo),:) = z3d(:,mj0(jpjglo-1):mj1(jpjglo),:)   ! north

      CALL Agrif_Bc_variable(z3d,avmv_id,calledweight=1.,procname=interpavmv)
      
      avmv_k(mi0(       1):mi1(     2),:,:) = z3d(mi0(       1):mi1(     2),:,:)   ! west
      avmv_k(mi0(jpiglo-1):mi1(jpiglo),:,:) = z3d(mi0(jpiglo-1):mi1(jpiglo),:,:)   ! east
      avmv_k(:,mj0(       1):mj1(     2),:) = z3d(:,mj0(       1):mj1(     2),:)   ! south
      avmv_k(:,mj0(jpjglo-1):mj1(jpjglo),:) = z3d(:,mj0(jpjglo-1):mj1(jpjglo),:)   ! north
              
      Agrif_UseSpecialValue = .FALSE.
      CALL wrk_dealloc( jpi, jpj, jpk, z3d )

   END SUBROUTINE Agrif_tke


   SUBROUTINE Agrif_e3t
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE Agrif_e3t  ***
      !!----------------------------------------------------------------------  
      !!
      INTEGER :: ji,jj,jk
      INTEGER :: icnt
      REAL(wp) :: zrhox, zrhoy
      REAL(wp), POINTER, DIMENSION(:,:,:) :: ze3t
      !!----------------------------------------------------------------------  
      IF( Agrif_Root() )   RETURN
      
      CALL wrk_alloc( jpi, jpj, jpk, ze3t )
      zrhox = Agrif_Rhox()
      zrhoy = Agrif_Rhoy()
      ze3t(:,:,:) = 0.
      icnt = 0 
      
      CALL Agrif_Bc_variable(ze3t,e3t_id,calledweight=1.,procname=interpe3t)
      
      ! Warning: do not take into account the fist/last column/line that are masked in the child grid
      
      DO jk=1,jpkm1    !    west
         DO jj=mj0(2),mj1(jpjglo-1)
            DO ji=mi0(2),mi1(2 + 3*zrhox)
               IF( ABS(ze3t(ji,jj,jk) - fse3t(ji,jj,jk))*tmask(ji,jj,jk) > 1.e-2 ) THEN
                  WRITE(numout,*) 'ERROR bathymetry merge at the western border ji,jj,jk ', ji,jj,jk
                  WRITE(numout,*) '      ze3t(ji,jj,jk), fse3t(ji,jj,jk) ', ze3t(ji,jj,jk), fse3t(ji,jj,jk)
                  icnt = icnt + 1
               END IF
            END DO
         END DO
      END DO
      
      DO jk=1,jpkm1    !    east
         DO jj=mj0(2),mj1(jpjglo-1)
            DO ji=mi0(jpiglo - 1 - 3*zrhox),mi1(jpiglo-1)
               IF( ABS(ze3t(ji,jj,jk) - fse3t(ji,jj,jk))*tmask(ji,jj,jk) > 1.e-2 ) THEN
                  WRITE(numout,*) 'ERROR bathymetry merge at the eastern border ji,jj,jk ', ji,jj,jk
                  WRITE(numout,*) '      ze3t(ji,jj,jk), fse3t(ji,jj,jk) ', ze3t(ji,jj,jk), fse3t(ji,jj,jk)
                  icnt = icnt + 1
               END IF
            END DO
         END DO
      END DO
      
      DO jk=1,jpkm1    ! south
         DO jj=mj0(2),mj1(2 + 3*zrhoy)
            DO ji=mi0(2),mi1(jpiglo-1)
               IF( ABS(ze3t(ji,jj,jk) - fse3t(ji,jj,jk))*tmask(ji,jj,jk) > 1.e-2 ) THEN 
                  WRITE(numout,*) 'ERROR bathymetry merge at the southern border ji,jj,jk', ji,jj,jk
                  WRITE(numout,*) '      ze3t(ji,jj,jk), fse3t(ji,jj,jk) ', ze3t(ji,jj,jk), fse3t(ji,jj,jk)
                  icnt = icnt + 1
               END IF
            END DO
         END DO
      END DO
      DO jk=1,jpkm1   ! north
         DO jj=mj0(jpjglo - 1 - 3*zrhoy),mj1(jpjglo-1)
            DO ji=mi0(2),mi1(jpiglo-1)
               IF( ABS(ze3t(ji,jj,jk) - fse3t(ji,jj,jk))*tmask(ji,jj,jk) > 1.e-2 ) THEN
                  WRITE(numout,*) 'ERROR bathymetry merge at the northen border ji,jj,jk', ji,jj,jk
                  WRITE(numout,*) '      ze3t(ji,jj,jk), fse3t(ji,jj,jk) ', ze3t(ji,jj,jk), fse3t(ji,jj,jk)
                  icnt = icnt + 1
               END IF
            END DO
         END DO
      END DO

      CALL wrk_dealloc( jpi, jpj, jpk, ze3t )
      
      IF(icnt /= 0) THEN 
         CALL ctl_stop('ERROR in bathymetry merge between parent and child grids...')
      ELSE
         IF(lwp) WRITE(numout,*) 'interp e3t ok...'
      END IF
      
   END SUBROUTINE Agrif_e3t


   SUBROUTINE interpu(tabres,i1,i2,j1,j2,k1,k2)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpu  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      !!
      INTEGER :: ji,jj,jk
      !!----------------------------------------------------------------------  

      DO jk=k1,k2
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj,jk) = e2u(ji,jj) * un(ji,jj,jk) * fse3u(ji,jj,jk)
            END DO
         END DO
      END DO

   END SUBROUTINE interpu


   SUBROUTINE interpu2d(tabres,i1,i2,j1,j2)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpu2d  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) :: i1,i2,j1,j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      !!
      INTEGER :: ji,jj
      !!----------------------------------------------------------------------  

      DO jj=j1,j2
         DO ji=i1,i2
            tabres(ji,jj) = e2u(ji,jj) * ((gcx(ji+1,jj) - gcx(ji,jj))/e1u(ji,jj)) &
               * umask(ji,jj,1)
         END DO
      END DO

   END SUBROUTINE interpu2d


   SUBROUTINE interpv(tabres,i1,i2,j1,j2,k1,k2)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpv  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      !!
      INTEGER :: ji, jj, jk
      !!----------------------------------------------------------------------  

      DO jk=k1,k2
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj,jk) = e1v(ji,jj) * vn(ji,jj,jk) * fse3v(ji,jj,jk)
            END DO
         END DO
      END DO

   END SUBROUTINE interpv

   
   SUBROUTINE interpv2d(tabres,i1,i2,j1,j2)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpu2d  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) :: i1,i2,j1,j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      !!
      INTEGER :: ji,jj
      !!----------------------------------------------------------------------  

      DO jj=j1,j2
         DO ji=i1,i2
            tabres(ji,jj) = e1v(ji,jj) * ((gcx(ji,jj+1) - gcx(ji,jj))/e2v(ji,jj)) &
               * vmask(ji,jj,1)
         END DO
      END DO

   END SUBROUTINE interpv2d


   SUBROUTINE interpe3t(tabres,i1,i2,j1,j2,k1,k2)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpv  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      !!
      INTEGER :: ji, jj, jk
      !!----------------------------------------------------------------------  
      
      DO jk=k1,k2
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj,jk) = tmask(ji,jj,jk) * fse3t(ji,jj,jk)
            END DO
         END DO
      END DO
      
   END SUBROUTINE interpe3t


   SUBROUTINE interpavt(tabres,i1,i2,j1,j2,k1,k2)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interavt  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      
      tabres(i1:i2,j1:j2,k1:k2) = avt_k(i1:i2,j1:j2,k1:k2)
      
   END SUBROUTINE interpavt


   SUBROUTINE interpavm(tabres,i1,i2,j1,j2,k1,k2)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interavm  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      
      tabres(i1:i2,j1:j2,k1:k2) = avm_k(i1:i2,j1:j2,k1:k2)
      
   END SUBROUTINE interpavm


   SUBROUTINE interpavmu(tabres,i1,i2,j1,j2,k1,k2)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interavmu  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      
      tabres(i1:i2,j1:j2,k1:k2) = avmu_k(i1:i2,j1:j2,k1:k2)
      
   END SUBROUTINE interpavmu


   SUBROUTINE interpavmv(tabres,i1,i2,j1,j2,k1,k2)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interavmv  ***
      !!----------------------------------------------------------------------  
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      
      tabres(i1:i2,j1:j2,k1:k2) = avmv_k(i1:i2,j1:j2,k1:k2)
      
   END SUBROUTINE interpavmv


#else
   !!----------------------------------------------------------------------
   !!   Empty module                                          no AGRIF zoom
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE Agrif_OPA_Interp_empty
      WRITE(*,*)  'agrif_opa_interp : You should not have seen this print! error?'
   END SUBROUTINE Agrif_OPA_Interp_empty
#endif

   !!======================================================================
END MODULE agrif_opa_interp
