+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   N. Jourdain, CCRC-UNSW, June 2014
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Here is explained how to build an entire coupled configuration for NEMO-OASIS-WRF (NOW) 
starting from WRF's input files (i.e. those from WPS/real.exe) with or without a nest.

WARNING: So far, all this pre-processing works only in one of these conditions:
		- either there is no nest
		- or there is one nest (no more please) in both WRF and NEMO

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

NB: the presence of a 2-way nest in WRF modifies the effective 
    LANDMASK of the parent grid in wrfout* files but not in the
    geogrid/wrfinput files !!!

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     Last updates:
          - nj- Feb. 2017 : put on GitHub
          - nj- Sep. 2014 : fix major error in e1,e2 calculation in build_bathy_xxxx_from_ETOPO.f90
          - nj- Sep. 2014 : works with or without lake physics (from wrfv3.6).
          - nj- Oct. 2014 : handle effective landmask in case of 2-way nesting.
          - nj- Oct. 2014 : Automatization of all the process (introduction of namelist_precpl)

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   step    1    : Compilation of all fortran routines
   steps  2-5   : Build netcdf files for NEMO's parent grid
   steps  6-9   : Build netcdf files for NEMO's child grid
   steps 10-12  : Build netcdf files for OASIS + correct CPLMASK variable in wrfinput

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#########################################################################################################
#########################################################################################################
## 1-- Compile all the fortran scripts. 
##     Edit the bash script compile_ALL.sh, set up compiler and netcdf libraries, 
##     then execute the following to create all the executables needed here :

./compile_ALL.sh



#########################################################################################################
#########################################################################################################
## 2-- Build bathymetry and coordinates for NEMO parent grid.
##     First, edit the file namelist_precpl, and make sure that the file names point to existing files.
##     - If you use 1-way nesting or no nesting in WRF, you can directly execute the command below.
##     - If you use 2-way nesting in WRF (max_dom>1 and feedback=1), then you will have to first run WRF
##       with a nest for a number time steps, and save the LANDMASK variable fron one of the wrfout_d01_*.
##       Then keep only one time step and only the variable LANDMASK (use ncks) and rename it (e.g. as 
##       landeff_d01_trop075_nest025.nc).
##       NB: you need to use the same "smooth" namelist option to generate landeff_d01_trop075_nest025.nc
##           and to run NOW.
##     Then execute the commande below to create  coordinate and bathy_meter netcdf files :

./build_bathy_parent            ## should create coordinates_xxxx.nc and bathy_meter_xxxx.nc

#########################################################################################################
## 3-- Now you need to run nemo.exe (no agrif, no coupling) to calculate the exact mesh and mask.
##     Run it with nn_msh = 1 in the namelist. No need to have input files here, just coordinates.nc and 
##     bathy_meter.nc. It will crash just after having created mesh_mask.nc. Rename the file with 
##     configuration name, for instance, if you had coordinates_xxxx.nc, you should have mesh_mask_xxxx.nc.
##     Copy the mesh_mask file in the current directory (i.e. here).

cd ../run


#########################################################################################################
## 4-- Build the open boundary conditions (OBCs) for the parent grid (first calculate interpolation 
##     coefficients then interpolate global ocean data on every OBC :

./build_NEMO_OBC_intrp_coeff  ## should create a file coeff_OBC_yyyy_xxxx_from_ORCA025_L75.nc for each boundary
./build_NEMO_yearly_OBC       ## it usually takes several hours to process OBCs...
                              ## should create one OBC file per year per boundary

#########################################################################################################
## 5-- Calculate 3D interpolation coefficients between the global and the regional parent grid, 
##     then interpolate initial state, runoff, chlorophyll, and tidal parameters on parent grid :

./build_parent_3D_intrp_coeff_from_ORCA025    ## should create coeff_3D_xxxx_from_ORCA025.nc 
                                              ## (may take some time to run...)
./build_NEMO_parent_istate                    ## should create dta_temp_xxxx_y1989m01.nc 
                                              ## and dta_sal_xxxx_y1989m01.nc
./build_NEMO_parent_runoff_from_ORCA025       ## should create runoff_xxxx.nc
./build_NEMO_parent_chlorophyll_from_ORCA025  ## should create chlorophyll_xxxx.nc
./build_NEMO_parent_tides_from_ORCA025        ## should create alltides_xxxx.nc



#########################################################################################################
#########################################################################################################
## 6-- It is not possible to have the exact same grid size for WRF's nest and NEMO's nest.
##     The way it is done here, is to have a larger nest in NEMO than in WRF, with parent landmask in 
##     a halo in NEMO. So you first need to create temporary geo_em_d02.nc and wrfinput_d02 files by 
##     extending the domain size when launching WPS and real.exe (just run WPS and real.exe for 1-day 
##     by adjusting end_date in namelist.wps and end_year,end_month,end_day in namelist.input).
##             -> do not change anything for d01 in the namelists
##             -> for d02, reduce i_parent_start and j_parent_start by 4 points
##             -> for d02, increase e_sn and e_we by (2*4*parent_grid_ratio) points 
##                (e.g. by 24 points for a grid ratio of 3)
##             -> rename the newly created geo_em.d02.nc as geo_em.d02_EXTENDED_FOR_AGRIF.nc
##             -> rename the newly created wrfinput_d02 as wrfinput_d02_EXTENDED_FOR_AGRIF
##     NB: if you use the lake physics option in your simulations, be sure to run WPS/real.exe with this 
##         option for the steps above.

#########################################################################################################
## 7-- Build bathymetry and coordinates for NEMO child grid.

./build_bathy_child          ## should create 1_coordinates_xxxx.nc and 1_bathy_meter_xxxx.nc

#########################################################################################################
## 8-- Now you need to run nemo.exe (compiled with key_agrif, no coupling) to calculate the exact mesh 
##     and mask. Run it with nn_msh = 1 in the 1_namelist. You'll need to have input files for parent grid,
##     and 1_coordinates.nc and 1_bathy_meter.nc. it will crash just after having created 1_mesh_mask.nc. 
##
##     To run NEMO with AGRIF, you need to create the ASCII file AGRIF_FixedGrids.in.
##     It should contain the followings :
##        1
##        IPS IPE JPS JPE RRR RRR RRR
##        0
##     with theses values from WRF 
##     (see netcdf metadata in the geo_em.d02.nc that is used to run NOW, not the extended one)
##        IPS = i_parent_start - 5 ( +1 if parent grid is Eat-West periodic )
##        IPE = i_parent_end   + 2 ( +1 if parent grid is Eat-West periodic )
##        JPS = j_parent_start - 5
##        JPE = j_parent_end   + 2
##        RRR = parent_grid_ratio
##
##     WARNING! It is highly recommended to check that 1_mesh_mask_MC25new.nc has these dimensions:
##            mx_NEMO = meast_west_WRF   + 2*(3*RRR+2)
##            my_NEMO = msouth_north_WRF + 2*(3*RRR+2)
##
##     At the end, rename the new mesh_mask file with configuration name (for instance, 
##     if you had 1_coordinates_xxxx.nc, you rename it as 1_mesh_mask_xxxx.nc).
##     Copy the zoom mesh_mask file in the current directory (i.e. here).

#########################################################################################################
## 9-- Calculate 3D interpolation coefficients between the global and the regional child grid,
##     then interpolate initial state, runoff, chlorophyll, and tidal parameters on child grid :

./build_child_3D_intrp_coeff_from_ORCA025     ## should create coeff_3D_xxxx_from_ORCA025.nc 
                                              ## (may take some time to run...)
./build_NEMO_child_istate                     ## should create 1_dta_temp_xxxx_y1989m01.nc
                                              ## and 1_dta_sal_xxxx_y1989m01.nc
./build_NEMO_child_runoff_from_ORCA025        ## should create 1_runoff_xxxx.nc
./build_NEMO_child_chlorophyll_from_ORCA025   ## should create 1_chlorophyll_xxxx.nc
./build_NEMO_child_tides_from_ORCA025         ## should create 1_alltides_xxxx.nc



#########################################################################################################
#########################################################################################################
## 10-- Let's move to the coupling interface. The first thing to do is to create a coupling mask for WRF.
##      The variable CPLMASK in wrfinput tells WRF where you want to couple and where you want to read SST
##      in the wrflowinp file. This variable has to be added to wrfinput files prior to  WRF's 3.6 version.
##      From 3.6 the CPLMASK variable is there, but is just zero everywhere, so you need to put correct 
##      values. Here is the way to process. If you use one domain, just execute the first command.

./create_WRF_cplmask_d01   ## to do in any case                       -> should create cplmask_d01_xxxx.nc
./create_WRF_cplmask_d02   ## to do only if you have a nested domain  -> should create cplmask_d02_xxxx.nc

## then, you need to put the CPLMASK variable in all the wrfinput files. For example, for domain d01, do :
ncks -O -x -v CPLMASK wrfinput_d01 wrfinput_d01  ## not needed prior to WRF's 3.6 version
ncks -A  cplmask_d01_xxxx.nc  wrfinput_d01
## and process similarly with cplmask_d02_xxxx.nc and wrfinput_d02.
## NB: If you are on raijin, you can use the bash script correct_wrfinput_CPL.sh to do this 
##     (see in this directory).

#########################################################################################################
## 11-- Now let's create mozaic files to specify which grid point of NEMO goes with which grid point of WRF
##      (these files are used by OASIS) :

./build_mozaic_OASIS_Ad01_Od01   ## to do in any case            
                                 ##   -> creates mozaic_Ad01_to_Od01_xxxx.nc + mozaic_Od01_to_Ad01_xxxx.nc 
./build_mozaic_OASIS_Ad02_Od02   ## to do only with nested domain  
                                 ##   -> creates mozaic_Ad02_to_Od02_xxxx.nc + mozaic_Od02_to_Ad02_xxxx.nc
./build_mozaic_OASIS_Ad01_Od02   ## to do only with nested domain  
                                 ##   -> creates mozaic_Ad01_to_Od02_xxxx.nc

#########################################################################################################
## 12-- Then, let's create the initial state for OASIS (kind of restart) :

./create_OASIS_flxatmos_Ad01_Od01   ## to do in any case              -> creates flxat_Ad01_to_Od01_xxxx.nc
./create_OASIS_flxatmos_Ad01_Od02   ## to do only with nested domain  -> creates flxat_Ad01_to_Od02_xxxx.nc
./create_OASIS_flxatmos_Ad02_Od02   ## to do only with nested domain  -> creates flxat_Ad02_to_Od02_xxxx.nc

./create_OASIS_sstocean_Ad01_Od01   ## to do in any case              -> creates sstoc_Od01_to_Ad01_xxxx.nc
./create_OASIS_sstocean_Ad02_Od02   ## to do only with nested domain  -> creates sstoc_Od02_to_Ad02_xxxx.nc



#########################################################################################################
#########################################################################################################
## END OF PRE-PROCESSING
#########################################################################################################
#########################################################################################################
