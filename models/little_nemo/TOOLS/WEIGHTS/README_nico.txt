cd TOOLS
./maketools -m X64_RAIJIN -n WEIGHTS
cd WEIGHTS/

vi namelist_cordex24_bilin
./scripgrid.exe   ## namelist_cordex24_bilin
./scrip.exe       ## namelist_cordex24_bilin
./scripshape.exe  ## namelist_cordex24_bilin

vi namelist_cordex24_bicub
./scrip.exe       ## namelist_cordex24_bicub
./scripshape.exe  ## namelist_cordex24_bicub

