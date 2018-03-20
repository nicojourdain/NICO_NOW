MODULE agrif_oce
   !!======================================================================
   !!                       ***  MODULE agrif_oce  ***
   !! AGRIF :   define in memory AGRIF variables
   !!----------------------------------------------------------------------
   !! History :  2.0  ! 2007-12  (R. Benshila)  Original code
   !!----------------------------------------------------------------------
#if defined key_agrif
   !!----------------------------------------------------------------------
   !!   'key_agrif'                                              AGRIF zoom
   !!----------------------------------------------------------------------
   USE par_oce      ! ocean parameters
   USE dom_oce      ! domain parameters
   
   IMPLICIT NONE
   PRIVATE 

   PUBLIC agrif_oce_alloc ! routine called by nemo_init in nemogcm.F90

   !                                              !!* Namelist namagrif: AGRIF parameters
   LOGICAL , PUBLIC ::   ln_spc_dyn    = .FALSE.   !:
   INTEGER , PUBLIC ::   nn_cln_update = 3         !: update frequency 
   REAL(wp), PUBLIC ::   rn_sponge_tra = 2800.     !: sponge coeff. for tracers
   REAL(wp), PUBLIC ::   rn_sponge_dyn = 2800.     !: sponge coeff. for dynamics

   !                                              !!! OLD namelist names
   INTEGER , PUBLIC ::   nbclineupdate             !: update frequency 
   REAL(wp), PUBLIC ::   visc_tra                  !: sponge coeff. for tracers
   REAL(wp), PUBLIC ::   visc_dyn                  !: sponge coeff. for dynamics

   LOGICAL , PUBLIC :: spongedoneT = .FALSE.   !: tracer   sponge layer indicator
   LOGICAL , PUBLIC :: spongedoneU = .FALSE.   !: dynamics sponge layer indicator

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,  DIMENSION(:,:) ::   spe1ur , spe2vr , spbtr2   !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,  DIMENSION(:,:) ::   spe1ur2, spe2vr2, spbtr3   !: ???
   
   INTEGER :: tsn_id,tsb_id,tsa_id
   INTEGER :: un_id, vn_id, ua_id, va_id
   INTEGER :: e3t_id
   INTEGER :: e1u_id, e2v_id, sshn_id, gcb_id
   INTEGER :: trn_id, trb_id, tra_id
   INTEGER :: glamt_id, gphit_id
   INTEGER :: avt_id, avm_id, avmu_id, avmv_id

   !!----------------------------------------------------------------------
   !! NEMO/NST 3.3.1 , NEMO Consortium (2011)
   !! $Id: agrif_oce.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS 

   INTEGER FUNCTION agrif_oce_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION agrif_oce_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( spe1ur (jpi,jpj) , spe2vr (jpi,jpj) , spbtr2(jpi,jpj) ,      &
         &      spe1ur2(jpi,jpj) , spe2vr2(jpi,jpj) , spbtr3(jpi,jpj) , STAT = agrif_oce_alloc ) 
   END FUNCTION agrif_oce_alloc

#endif
   !!======================================================================
END MODULE agrif_oce
