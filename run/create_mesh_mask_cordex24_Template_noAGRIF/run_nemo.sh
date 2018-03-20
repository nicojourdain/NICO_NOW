#!/bin/bash
#PBS -P e14
#PBS -q express
#PBS -l wd
#PBS -l ncpus=4,other=iobound,mem=2Gb,walltime=01:00:00

## NB: change nb of proc in mpirun if change ncpus

echo "-np 2 ./nemo.exe" > app.conf
echo "-np 2 ./xios_server.exe" >> app.conf
time mpirun --app app.conf

