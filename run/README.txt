create_mesh_mask_cordex24_Template_noAGRIF is used to generate the original mesh_mask file.

now_cordex24_Template_noAGRIF is used to run long NOW simulations (with automatic resubmission).

postnow_cordex24_Template_noAGRIF is used to postprocess WRF's outputs.

It is recommended to keep these templates and copy them in your directory. For example, say that your configuration (i.e. your grid/mesh/mask) is called MC25 and you case (i.e. your simulation) is called GNJ001. you can do as follow:

export CONFIG='MC25'
export CASE='GNJ001'

cp -rp create_mesh_mask_cordex24_Template_noAGRIF create_mesh_mask_${CONFIG}_${CASE}
cd create_mesh_mask_${CONFIG}_${CASE}
## Adapt the header in run_nemo.sh to your platform batch jobs manager.

NB: you may have to replace rebuild_restart.f90 with rebuild_restart_2.f90 if rebuild the single restart file from multi-processor restart files does not work.
