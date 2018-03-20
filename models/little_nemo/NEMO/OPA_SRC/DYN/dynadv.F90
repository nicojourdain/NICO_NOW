MODULE dynadv
   !!==============================================================================
   !!                       ***  MODULE  dynadv  ***
   !! Ocean active tracers:  advection scheme control
   !!==============================================================================
   !! History :  1.0  !  2006-11  (G. Madec)  Original code
   !!            3.3  !  2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_adv      : compute the momentum advection trend 
   !!   dyn_adv_init : control the different options of advection scheme
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE dynadv_cen2     ! centred flux form advection      (dyn_adv_cen2 routine)
   USE dynadv_ubs      ! UBS flux form advection          (dyn_adv_ubs  routine)
   USE dynkeg          ! kinetic energy gradient          (dyn_keg      routine)
   USE dynzad          ! vertical advection               (dyn_zad      routine)
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC dyn_adv       ! routine called by step module
   PUBLIC dyn_adv_init  ! routine called by opa module
 
   LOGICAL, PUBLIC ::   ln_dynadv_vec  = .TRUE.    !: vector form flag
   LOGICAL, PUBLIC ::   ln_dynadv_cen2 = .FALSE.   !: flux form - 2nd order centered scheme flag
   LOGICAL, PUBLIC ::   ln_dynadv_ubs  = .FALSE.   !: flux form - 3rd order UBS scheme flag
   
   INTEGER ::   nadv   ! choice of the formulation and scheme for the advection

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: dynadv.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_adv( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_adv  ***
      !!                
      !! ** Purpose :   compute the ocean momentum advection trend.
      !!
      !! ** Method  : - Update (ua,va) with the advection term following nadv
      !!      NB: in flux form advection (ln_dynadv_cen2 or ln_dynadv_ubs=T) 
      !!      a metric term is add to the coriolis term while in vector form 
      !!      it is the relative vorticity which is added to coriolis term
      !!      (see dynvor module).
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dyn_adv')
      !
      SELECT CASE ( nadv )                  ! compute advection trend and add it to general trend
      CASE ( 0 )     
                      CALL dyn_keg     ( kt )    ! vector form : horizontal gradient of kinetic energy
                      CALL dyn_zad     ( kt )    ! vector form : vertical advection
      CASE ( 1 ) 
                      CALL dyn_adv_cen2( kt )    ! 2nd order centered scheme
      CASE ( 2 )   
                      CALL dyn_adv_ubs ( kt )    ! 3rd order UBS      scheme
      !
      CASE (-1 )                                 ! esopa: test all possibility with control print
                      CALL dyn_keg     ( kt )
                      CALL dyn_zad     ( kt )
                      CALL dyn_adv_cen2( kt )
                      CALL dyn_adv_ubs ( kt )
      END SELECT
      !
      IF( nn_timing == 1 )  CALL timing_stop('dyn_adv')
      !
   END SUBROUTINE dyn_adv

   
   SUBROUTINE dyn_adv_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_adv_init  ***
      !!                
      !! ** Purpose :   Control the consistency between namelist options for 
      !!              momentum advection formulation & scheme and set nadv
      !!----------------------------------------------------------------------
      INTEGER ::   ioptio
      !!
      NAMELIST/namdyn_adv/ ln_dynadv_vec, ln_dynadv_cen2 , ln_dynadv_ubs
      !!----------------------------------------------------------------------

      REWIND ( numnam )               ! Read Namelist namdyn_adv : momentum advection scheme
      READ   ( numnam, namdyn_adv )

      IF(lwp) THEN                    ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'dyn_adv_init : choice/control of the momentum advection scheme'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*) '       Namelist namdyn_adv : chose a advection formulation & scheme for momentum'
         WRITE(numout,*) '          Vector/flux form (T/F)             ln_dynadv_vec  = ', ln_dynadv_vec
         WRITE(numout,*) '          2nd order centred advection scheme ln_dynadv_cen2 = ', ln_dynadv_cen2
         WRITE(numout,*) '          3rd order UBS advection scheme     ln_dynadv_ubs  = ', ln_dynadv_ubs
      ENDIF

      ioptio = 0                      ! Parameter control
      IF( ln_dynadv_vec  )   ioptio = ioptio + 1
      IF( ln_dynadv_cen2 )   ioptio = ioptio + 1
      IF( ln_dynadv_ubs  )   ioptio = ioptio + 1
      IF( lk_esopa       )   ioptio =          1

      IF( ioptio /= 1 )   CALL ctl_stop( 'Choose ONE advection scheme in namelist namdyn_adv' )

      !                               ! Set nadv
      IF( ln_dynadv_vec  )   nadv =  0 
      IF( ln_dynadv_cen2 )   nadv =  1
      IF( ln_dynadv_ubs  )   nadv =  2
      IF( lk_esopa       )   nadv = -1

      IF(lwp) THEN                    ! Print the choice
         WRITE(numout,*)
         IF( nadv ==  0 )   WRITE(numout,*) '         vector form : keg + zad + vor is used'
         IF( nadv ==  1 )   WRITE(numout,*) '         flux form   : 2nd order scheme is used'
         IF( nadv ==  2 )   WRITE(numout,*) '         flux form   : UBS       scheme is used'
         IF( nadv == -1 )   WRITE(numout,*) '         esopa test: use all advection formulation'
      ENDIF
      !
   END SUBROUTINE dyn_adv_init

  !!======================================================================
END MODULE dynadv
