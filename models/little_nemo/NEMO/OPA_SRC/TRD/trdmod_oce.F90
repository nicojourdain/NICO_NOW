MODULE trdmod_oce
   !!======================================================================
   !!                   ***  MODULE trdmod_oce  ***
   !! Ocean trends :   set tracer and momentum trend variables
   !!======================================================================
   !! History :  1.0  !  2004-08  (C. Talandier) Original code
   !!----------------------------------------------------------------------
   USE trdicp_oce              ! ocean momentum/tracers bassin properties trends variables
   USE trdmld_oce              ! ocean active mixed layer tracers trends variables
   USE trdvor_oce              ! ocean vorticity trends variables

   IMPLICIT NONE
   PUBLIC

   !                                                     !!* Namelist namtrd:  diagnostics on dynamics/tracer trends *
   INTEGER , PUBLIC  ::   nn_trd  = 10                    !: time step frequency dynamics and tracers trends
   INTEGER , PUBLIC  ::   nn_ctls =  0                    !: control surface type for trends vertical integration
   REAL(wp), PUBLIC  ::   rn_ucf   = 1.                   !: unit conversion factor (for netCDF trends outputs)
                                                          !: =1. (=86400.) for degC/s (degC/day) and psu/s (psu/day)
   CHARACTER(len=32) ::   cn_trdrst_in  = "restart_mld"   !: suffix of ocean restart name (input)
   CHARACTER(len=32) ::   cn_trdrst_out = "restart_mld"   !: suffix of ocean restart name (output)
   LOGICAL , PUBLIC  ::   ln_trdmld_instant = .FALSE.     !: flag to diagnose inst./mean ML T/S trends
   LOGICAL , PUBLIC  ::   ln_trdmld_restart = .FALSE.     !: flag to restart mixed-layer diagnostics

# if defined key_trdtra   ||   defined key_trdmld
   LOGICAL , PUBLIC ::   l_trdtra = .TRUE.                !: tracers  trend flag
# else
   LOGICAL , PUBLIC ::   l_trdtra = .FALSE.               !: tracers  trend flag
# endif
# if defined key_trddyn   ||   defined key_trdvor
   LOGICAL , PUBLIC ::   l_trddyn = .TRUE.                !: momentum trend flag
# else
   LOGICAL , PUBLIC ::   l_trddyn = .FALSE.               !: momentum trend flag
# endif
# if ( defined key_trdtrc && defined key_iomput )  ||  defined key_trdmld_trc
   LOGICAL , PUBLIC ::   l_trdtrc = .TRUE.                !: tracers  trend flag
# else
   LOGICAL , PUBLIC ::   l_trdtrc = .FALSE.               !: tracers  trend flag
# endif
   !                                                     !!!* Active tracers trends indexes
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_xad =  1     !: x- horizontal advection
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_yad =  2     !: y- horizontal advection
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_zad =  3     !: z- vertical   advection
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_ldf =  4     !: lateral       diffusion
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_zdf =  5     !: vertical diffusion (Kz)
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_bbc =  6     !: Bottom Boundary Condition (geoth. flux) 
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_bbl =  7     !: Bottom Boundary Layer (diffusive/convective)
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_npc =  8     !: static instability mixing
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_dmp =  9     !: damping
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_qsr = 10     !: penetrative solar radiation
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_nsr = 11     !: non solar radiation
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_atf = 12     !: Asselin correction
#if defined key_top
   !                                                     !!!* Passive tracers trends indexes
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_sms  = 13    !: sources m. sinks
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_radn = 14    !: corr. trn<0 in trcrad
   INTEGER, PUBLIC, PARAMETER ::   jptra_trd_radb = 15    !: corr. trb<0 in trcrad (like atf)
#endif
   
   !                                                     !!!* Momentum trends indexes
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_hpg =  1     !: hydrostatic pressure gradient 
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_keg =  2     !: kinetic energy gradient
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_rvo =  3     !: relative vorticity
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_pvo =  4     !: planetary vorticity
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_ldf =  5     !: lateral diffusion
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_had =  6     !: horizontal advection
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_zad =  7     !: vertical advection
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_zdf =  8     !: vertical diffusion
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_spg =  9     !: surface pressure gradient
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_dat = 10     !: damping term
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_swf = 11     !: surface wind forcing
   INTEGER, PUBLIC, PARAMETER ::   jpdyn_trd_bfr = 12     !: bottom friction 

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trdmod_oce.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE trdmod_oce
