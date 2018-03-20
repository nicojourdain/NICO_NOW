# Automatic Makefile

FCM_BLD_CFG = /short/e14/ncj561/now/models/little_nemo/TOOLS/COMPILE/bld_tools.cfg
export FCM_VERBOSE ?= 1

export FCM_ROOTDIR := /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/BLD
export FCM_CACHEDIR := $(FCM_ROOTDIR)/.cache/.bld
export FCM_CFGDIR := $(FCM_ROOTDIR)/cfg
export FCM_SRCDIR := $(FCM_ROOTDIR)/src
export FCM_BINDIR := $(FCM_ROOTDIR)/bin
export FCM_ETCDIR := $(FCM_ROOTDIR)/etc
export FCM_LIBDIR := $(FCM_ROOTDIR)/lib
export FCM_TMPDIR := $(FCM_ROOTDIR)/tmp
export FCM_DONEDIR := $(FCM_ROOTDIR)/done
export FCM_FLAGSDIR := $(FCM_ROOTDIR)/flags
export FCM_INCDIR := $(FCM_ROOTDIR)/inc
export FCM_PPSRCDIR := $(FCM_ROOTDIR)/ppsrc
export FCM_OBJDIR := $(FCM_ROOTDIR)/obj
export FCM_ROOTPATH := $(FCM_ROOTDIR)
export FCM_CACHEPATH := $(FCM_CACHEDIR)
export FCM_CFGPATH := $(FCM_CFGDIR)
export FCM_SRCPATH := $(FCM_SRCDIR)
export FCM_BINPATH := $(FCM_BINDIR)
export FCM_ETCPATH := $(FCM_ETCDIR)
export FCM_LIBPATH := $(FCM_LIBDIR)
export FCM_TMPPATH := $(FCM_TMPDIR)
export FCM_DONEPATH := $(FCM_DONEDIR)
export FCM_FLAGSPATH := $(FCM_FLAGSDIR)
export FCM_INCPATH := $(FCM_INCDIR)
export FCM_PPSRCPATH := $(FCM_PPSRCDIR)
export FCM_OBJPATH := $(FCM_OBJDIR)

export PERL5LIB := /short/e14/ncj561/now/models/little_nemo/EXTERNAL/fcm/bin/../lib

export AR = ar
export ARFLAGS = rs
export CC = cc
export CC_COMPILE = -c
export CC_DEFINE = -D
export CC_INCLUDE = -I
export CC_OUTPUT = -o
export CFLAGS = 
export CPPKEYS = 
export DIFF3FLAGS = -E -m
export FC = mpif90 -c -cpp
export FC_COMPILE = -c
export FC_DEFINE = -D
export FC_INCLUDE = -I
export FC_MODSEARCH = 
export FC_OUTPUT = -o
export FFLAGS = -i4 -r8 -O3 -xAVX -fp-model precise -I/short/e14/ncj561/now/models/xios_nico/inc -I/short/e14/ncj561/util/include
export FPPKEYS = 
export LD = mpif90
export LDFLAGS =  -L/short/e14/ncj561/now/models/xios_nico/lib -lxios -L/short/e14/ncj561/util/lib -lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lhdf5 -lz -lcurl -lstdc++
export LD_LIBLINK = -l
export LD_LIBSEARCH = -L
export LD_OUTPUT = -o
export SHELL = /bin/sh

vpath % $(FCM_BINPATH)
vpath %.done $(FCM_DONEPATH)
vpath %.idone $(FCM_DONEPATH)
vpath %.etc $(FCM_ETCPATH)
vpath %.flags $(FCM_FLAGSPATH)
vpath % $(FCM_INCPATH)
vpath %.a $(FCM_LIBPATH)
vpath %.o $(FCM_OBJPATH)

FCM_BLD_TARGETS = agrif_create_bathy.exe agrif_create_coordinates.exe agrif_create_data.exe agrif_create_restart.exe agrif_create_restart_trc.exe

.PHONY : all

all : $(FCM_BLD_TARGETS)

$(FCM_DONEDIR)/FCM_CP.dummy:
	touch $@

CFLAGS__nemo.flags: CFLAGS.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS__nemo.flags: CPPKEYS.flags
	touch $(FCM_FLAGSDIR)/$@

FFLAGS__nemo.flags: FFLAGS.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS__nemo.flags: FPPKEYS.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo.flags: LDFLAGS.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo.flags: LD.flags
	touch $(FCM_FLAGSDIR)/$@

OBJECTS__nemo = agrif_connect_topo.o agrif_extrapolation.o agrif_gridsearch.o agrif_interpolation.o agrif_modutil.o agrif_partial_steps.o agrif_readwrite.o agrif_types.o bicubic_interp.o bilinear_interp.o io_netcdf.o

libnemo.a: $(OBJECTS__nemo)
	fcm_internal archive $@ $^

CC.flags:
	touch $(FCM_FLAGSDIR)/$@

CFLAGS.flags: CC.flags
	touch $(FCM_FLAGSDIR)/$@

CPPKEYS.flags:
	touch $(FCM_FLAGSDIR)/$@

FC.flags:
	touch $(FCM_FLAGSDIR)/$@

FFLAGS.flags: FC.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS.flags:
	touch $(FCM_FLAGSDIR)/$@

LD.flags:
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS.flags:
	touch $(FCM_FLAGSDIR)/$@

export OBJECTS = $(OBJECTS__nemo)

libfcm_default.a: $(OBJECTS)
	fcm_internal archive $@ $^

FFLAGS__nemo__agrif_connect_topo.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_connect_topo.done: agrif_connect_topo.o agrif_types.done
	touch $(FCM_DONEDIR)/$@

agrif_connect_topo.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_connect_topo.f90 FFLAGS__nemo__agrif_connect_topo.flags agrif_types.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_create_bathy.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__agrif_create_bathy.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__agrif_create_bathy.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_create_bathy.exe: create_bathy.o LD__nemo__agrif_create_bathy.flags LDFLAGS__nemo__agrif_create_bathy.flags agrif_connect_topo.done agrif_partial_steps.done agrif_readwrite.done bilinear_interp.done
	fcm_internal load:F nemo $< $@

create_bathy.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_create_bathy.f90 FFLAGS__nemo__agrif_create_bathy.flags agrif_connect_topo.o agrif_partial_steps.o agrif_readwrite.o bilinear_interp.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_create_coordinates.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__agrif_create_coordinates.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__agrif_create_coordinates.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_create_coordinates.exe: create_coordinate.o LD__nemo__agrif_create_coordinates.flags LDFLAGS__nemo__agrif_create_coordinates.flags agrif_interpolation.done agrif_readwrite.done
	fcm_internal load:F nemo $< $@

create_coordinate.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_create_coordinates.f90 FFLAGS__nemo__agrif_create_coordinates.flags agrif_interpolation.o agrif_readwrite.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_create_data.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__agrif_create_data.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__agrif_create_data.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_create_data.exe: create_data.o LD__nemo__agrif_create_data.flags LDFLAGS__nemo__agrif_create_data.flags agrif_interpolation.done agrif_readwrite.done bilinear_interp.done io_netcdf.done
	fcm_internal load:F nemo $< $@

create_data.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_create_data.f90 FFLAGS__nemo__agrif_create_data.flags agrif_interpolation.o agrif_readwrite.o bilinear_interp.o io_netcdf.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_create_restart.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__agrif_create_restart.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__agrif_create_restart.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_create_restart.exe: create_rstrt.o LD__nemo__agrif_create_restart.flags LDFLAGS__nemo__agrif_create_restart.flags agrif_extrapolation.done agrif_interpolation.done agrif_partial_steps.done agrif_readwrite.done bicubic_interp.done bilinear_interp.done io_netcdf.done
	fcm_internal load:F nemo $< $@

create_rstrt.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_create_restart.f90 FFLAGS__nemo__agrif_create_restart.flags agrif_extrapolation.o agrif_interpolation.o agrif_partial_steps.o agrif_readwrite.o bicubic_interp.o bilinear_interp.o io_netcdf.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_create_restart_trc.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__agrif_create_restart_trc.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__agrif_create_restart_trc.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_create_restart_trc.exe: create_rstrt_trc.o LD__nemo__agrif_create_restart_trc.flags LDFLAGS__nemo__agrif_create_restart_trc.flags agrif_extrapolation.done agrif_interpolation.done agrif_partial_steps.done agrif_readwrite.done bicubic_interp.done bilinear_interp.done io_netcdf.done
	fcm_internal load:F nemo $< $@

create_rstrt_trc.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_create_restart_trc.f90 FFLAGS__nemo__agrif_create_restart_trc.flags agrif_extrapolation.o agrif_interpolation.o agrif_partial_steps.o agrif_readwrite.o bicubic_interp.o bilinear_interp.o io_netcdf.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_extrapolation.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_extrapolation.done: agrif_extrapolation.o agrif_gridsearch.done agrif_readwrite.done agrif_types.done io_netcdf.done
	touch $(FCM_DONEDIR)/$@

agrif_extrapolation.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_extrapolation.f90 FFLAGS__nemo__agrif_extrapolation.flags agrif_gridsearch.o agrif_readwrite.o agrif_types.o io_netcdf.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_gridsearch.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_gridsearch.done: agrif_gridsearch.o agrif_modutil.done
	touch $(FCM_DONEDIR)/$@

agrif_gridsearch.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_gridsearch.f90 FFLAGS__nemo__agrif_gridsearch.flags agrif_modutil.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_interpolation.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_interpolation.done: agrif_interpolation.o agrif_extrapolation.done agrif_readwrite.done agrif_types.done bicubic_interp.done bilinear_interp.done io_netcdf.done
	touch $(FCM_DONEDIR)/$@

agrif_interpolation.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_interpolation.f90 FFLAGS__nemo__agrif_interpolation.flags agrif_extrapolation.o agrif_readwrite.o agrif_types.o bicubic_interp.o bilinear_interp.o io_netcdf.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_modutil.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_modutil.done: agrif_modutil.o
	touch $(FCM_DONEDIR)/$@

agrif_modutil.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_modutil.f90 FFLAGS__nemo__agrif_modutil.flags
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_partial_steps.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_partial_steps.done: agrif_partial_steps.o agrif_types.done
	touch $(FCM_DONEDIR)/$@

agrif_partial_steps.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_partial_steps.f90 FFLAGS__nemo__agrif_partial_steps.flags agrif_types.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_readwrite.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_readwrite.done: agrif_readwrite.o agrif_types.done io_netcdf.done
	touch $(FCM_DONEDIR)/$@

agrif_readwrite.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_readwrite.f90 FFLAGS__nemo__agrif_readwrite.flags agrif_types.o io_netcdf.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__agrif_types.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

agrif_types.done: agrif_types.o
	touch $(FCM_DONEDIR)/$@

agrif_types.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/agrif_types.f90 FFLAGS__nemo__agrif_types.flags
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__bicubic_interp.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

bicubic_interp.done: bicubic_interp.o agrif_modutil.done
	touch $(FCM_DONEDIR)/$@

bicubic_interp.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/bicubic_interp.f90 FFLAGS__nemo__bicubic_interp.flags agrif_modutil.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__bilinear_interp.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

bilinear_interp.done: bilinear_interp.o agrif_modutil.done
	touch $(FCM_DONEDIR)/$@

bilinear_interp.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/bilinear_interp.f90 FFLAGS__nemo__bilinear_interp.flags agrif_modutil.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__io_netcdf.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

io_netcdf.done: io_netcdf.o agrif_types.done
	touch $(FCM_DONEDIR)/$@

io_netcdf.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/NESTING/src/io_netcdf.f90 FFLAGS__nemo__io_netcdf.flags agrif_types.o
	fcm_internal compile:F nemo $< $@

# EOF
