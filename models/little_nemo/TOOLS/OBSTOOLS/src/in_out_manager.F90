MODULE in_out_manager   
   !!======================================================================
   !!                       ***  MODULE  in_out_manager  ***
   !! Ocean physics:  vertical mixing coefficient compute from the tke 
   !!                 turbulent closure parameterization
   !!=====================================================================
   !! History :   1.0  !  2002-06  (G. Madec)   original code
   !!             2.0  !  2006-07  (S. Masson)  iom, add ctl_stop, ctl_warn
   !!             3.0  !  2008-06  (G. Madec)   add ctmp4 to ctmp10
   !!             3.2  !  2009-08  (S. MAsson)  add new ctl_opn
   !!             3.3  !  2010-10  (A. Coward)  add NetCDF4 usage
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   USE par_oce       ! ocean parameter
!   USE lib_print     ! formated print library
!   USE nc4interface  ! NetCDF4 interface

   IMPLICIT NONE
   PUBLIC

   !!----------------------------------------------------------------------
   !!                   namrun namelist parameters
   !!----------------------------------------------------------------------
   CHARACTER(lc) ::   cn_exp        = "exp0"      !: experiment name used for output filename
   CHARACTER(lc) ::   cn_ocerst_in  = "restart"   !: suffix of ocean restart name (input)
   CHARACTER(lc) ::   cn_ocerst_out = "restart"   !: suffix of ocean restart name (output)
   LOGICAL       ::   ln_rstart     = .FALSE.     !: start from (F) rest or (T) a restart file
   INTEGER       ::   nn_no         = 0           !: job number
   INTEGER       ::   nn_rstctl     = 0           !: control of the time step (0, 1 or 2)
   INTEGER       ::   nn_rstssh     = 0           !: hand made initilization of ssh or not (1/0)
   INTEGER       ::   nn_it000      = 1           !: index of the first time step
   INTEGER       ::   nn_itend      = 10          !: index of the last time step
   INTEGER       ::   nn_date0      = 961115      !: initial calendar date aammjj
   INTEGER       ::   nn_leapy      = 0           !: Leap year calendar flag (0/1 or 30)
   INTEGER       ::   nn_istate     = 0           !: initial state output flag (0/1)
   INTEGER       ::   nn_write      =   10        !: model standard output frequency
   INTEGER       ::   nn_stock      =   10        !: restart file frequency
   LOGICAL       ::   ln_dimgnnn    = .FALSE.     !: type of dimgout. (F): 1 file for all proc
                                                       !:                  (T): 1 file per proc
   LOGICAL       ::   ln_mskland    = .FALSE.     !: mask land points in NetCDF outputs (costly: + ~15%)
   LOGICAL       ::   ln_clobber    = .FALSE.     !: clobber (overwrite) an existing file
   INTEGER       ::   nn_chunksz    = 0           !: chunksize (bytes) for NetCDF file (works only with iom_nf90 routines)
#if defined key_netcdf4
   !!----------------------------------------------------------------------
   !!                   namnc4 namelist parameters                         (key_netcdf4)
   !!----------------------------------------------------------------------
   ! The following four values determine the partitioning of the output fields
   ! into netcdf4 chunks. They are unrelated to the nn_chunk_sz setting which is
   ! for runtime optimisation. The individual netcdf4 chunks can be optionally 
   ! gzipped (recommended) leading to significant reductions in I/O volumes 
   !                                   !!!**  variables only used with iom_nf90 routines and key_netcdf4 **
   INTEGER ::   nn_nchunks_i = 1        !: number of chunks required in the i-dimension 
   INTEGER ::   nn_nchunks_j = 1        !: number of chunks required in the j-dimension 
   INTEGER ::   nn_nchunks_k = 1        !: number of chunks required in the k-dimension 
   INTEGER ::   nn_nchunks_t = 1        !: number of chunks required in the t-dimension 
   LOGICAL ::   ln_nc4zip    = .TRUE.   !: netcdf4 usage: (T) chunk and compress output using the HDF5 sublayers of netcdf4
   !                                    !                 (F) ignore chunking request and use the netcdf4 library 
   !                                    !                     to produce netcdf3-compatible files 
#endif
!$AGRIF_DO_NOT_TREAT
!   TYPE(snc4_ctl)     :: snc4set        !: netcdf4 chunking control structure (always needed for decision making)
!$AGRIF_END_DO_NOT_TREAT


   !! conversion of DOCTOR norm namelist name into model name
   !! (this should disappear in a near futur)

   CHARACTER(lc) ::   cexper                      !: experiment name used for output filename
   INTEGER       ::   no                          !: job number
   INTEGER       ::   nrstdt                      !: control of the time step (0, 1 or 2)
   INTEGER       ::   nit000                      !: index of the first time step
   INTEGER       ::   nitend                      !: index of the last time step
   INTEGER       ::   ndate0                      !: initial calendar date aammjj
   INTEGER       ::   nleapy                      !: Leap year calendar flag (0/1 or 30)
   INTEGER       ::   ninist                      !: initial state output flag (0/1)
   INTEGER       ::   nwrite                      !: model standard output frequency
   INTEGER       ::   nstock                      !: restart file frequency

   !!----------------------------------------------------------------------
   !! was in restart but moved here because of the OFF line... better solution should be found...
   !!----------------------------------------------------------------------
   INTEGER ::   nitrst   !: time step at which restart file should be written

   !!----------------------------------------------------------------------
   !!                    output monitoring
   !!----------------------------------------------------------------------
   LOGICAL ::   ln_ctl     = .FALSE.   !: run control for debugging
   INTEGER ::   nn_print     =    0    !: level of print (0 no print)
   INTEGER ::   nn_ictls     =    0    !: Start i indice for the SUM control
   INTEGER ::   nn_ictle     =    0    !: End   i indice for the SUM control
   INTEGER ::   nn_jctls     =    0    !: Start j indice for the SUM control
   INTEGER ::   nn_jctle     =    0    !: End   j indice for the SUM control
   INTEGER ::   nn_isplt     =    1    !: number of processors following i
   INTEGER ::   nn_jsplt     =    1    !: number of processors following j
   INTEGER ::   nn_bench     =    0    !: benchmark parameter (0/1)
   INTEGER ::   nn_bit_cmp   =    0    !: bit reproducibility  (0/1)

   !                                          
   INTEGER ::   nprint, nictls, nictle, njctls, njctle, isplt, jsplt, nbench    !: OLD namelist names

   INTEGER ::   ijsplt     =    1      !: nb of local domain = nb of processors

   !!----------------------------------------------------------------------
   !!                        logical units
   !!----------------------------------------------------------------------
   INTEGER ::   numstp     =   -1      !: logical unit for time step
   INTEGER ::   numout     =    6      !: logical unit for output print
   INTEGER ::   numnam     =   -1      !: logical unit for namelist
   INTEGER ::   numnam_ice =   -1      !: logical unit for ice namelist
   INTEGER ::   numevo_ice =   -1      !: logical unit for ice variables (temp. evolution)
   INTEGER ::   numsol     =   -1      !: logical unit for solver statistics

   !!----------------------------------------------------------------------
   !!                          Run control  
   !!----------------------------------------------------------------------
   INTEGER       ::   nstop = 0             !: error flag (=number of reason for a premature stop run)
   INTEGER       ::   nwarn = 0             !: warning flag (=number of warning found during the run)
   CHARACTER(lc) ::   ctmp1, ctmp2, ctmp3   !: temporary characters 1 to 3
   CHARACTER(lc) ::   ctmp4, ctmp5, ctmp6   !: temporary characters 4 to 6
   CHARACTER(lc) ::   ctmp7, ctmp8, ctmp9   !: temporary characters 7 to 9
   CHARACTER(lc) ::   ctmp10                !: temporary character 10
   CHARACTER(lc) ::   cform_err = "(/,' ===>>> : E R R O R',     /,'         ===========',/)"       !:
   CHARACTER(lc) ::   cform_war = "(/,' ===>>> : W A R N I N G', /,'         ===============',/)"   !:
   LOGICAL       ::   lwp      = .FALSE.    !: boolean : true on the 1st processor only
   LOGICAL       ::   lsp_area = .TRUE.     !: to make a control print over a specific area

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: in_out_manager.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!=====================================================================
END MODULE in_out_manager
