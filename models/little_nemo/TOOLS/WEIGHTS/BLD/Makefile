# Automatic Makefile

FCM_BLD_CFG = /short/e14/ncj561/now/models/little_nemo/TOOLS/COMPILE/bld_tools.cfg
export FCM_VERBOSE ?= 1

export FCM_ROOTDIR := /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/BLD
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

FCM_BLD_TARGETS = scrip.exe scripgrid.exe scripinterp.exe scripshape.exe

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

OBJECTS__nemo = constants.o grids.o iounits.o kinds_mod.o netcdf_mod.o remap_mod.o remap_bicubic.o remap_bilinear.o remap_conservative.o remap_distance_weight.o remap_read.o remap_vars.o remap_write.o scripgrid_mod.o scripinterp_mod.o timers.o

libnemo.a: $(OBJECTS__nemo)
	fcm_internal archive $@ $^

nemo.etc: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/CHANGES_BY_NOCS /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/copyright $(FCM_DONEDIR)/FCM_CP.dummy
	cp $^ $(FCM_ETCDIR)
	touch $(FCM_ETCDIR)/$@

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

FFLAGS__nemo__constants.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

constants.done: constants.o kinds_mod.done
	touch $(FCM_DONEDIR)/$@

constants.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/constants.f90 FFLAGS__nemo__constants.flags kinds_mod.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__grids.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

grids.done: grids.o constants.done iounits.done kinds_mod.done netcdf_mod.done
	touch $(FCM_DONEDIR)/$@

grids.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/grids.f90 FFLAGS__nemo__grids.flags constants.o iounits.o kinds_mod.o netcdf_mod.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__iounits.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

iounits.done: iounits.o kinds_mod.done
	touch $(FCM_DONEDIR)/$@

iounits.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/iounits.f90 FFLAGS__nemo__iounits.flags kinds_mod.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__kinds_mod.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

kinds_mod.done: kinds_mod.o
	touch $(FCM_DONEDIR)/$@

kinds_mod.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/kinds_mod.f90 FFLAGS__nemo__kinds_mod.flags
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__netcdf_mod.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

netcdf_mod.done: netcdf_mod.o constants.done kinds_mod.done
	touch $(FCM_DONEDIR)/$@

netcdf_mod.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/netcdf_mod.f90 FFLAGS__nemo__netcdf_mod.flags constants.o kinds_mod.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__remap.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

remap_mod.done: remap_mod.o constants.done kinds_mod.done
	touch $(FCM_DONEDIR)/$@

remap_mod.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/remap.f90 FFLAGS__nemo__remap.flags constants.o kinds_mod.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__remap_bicubic.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

remap_bicubic.done: remap_bicubic.o constants.done grids.done kinds_mod.done remap_vars.done
	touch $(FCM_DONEDIR)/$@

remap_bicubic.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/remap_bicubic.f90 FFLAGS__nemo__remap_bicubic.flags constants.o grids.o kinds_mod.o remap_vars.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__remap_bilinear.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

remap_bilinear.done: remap_bilinear.o constants.done grids.done kinds_mod.done remap_vars.done
	touch $(FCM_DONEDIR)/$@

remap_bilinear.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/remap_bilinear.f90 FFLAGS__nemo__remap_bilinear.flags constants.o grids.o kinds_mod.o remap_vars.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__remap_conserv.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

remap_conservative.done: remap_conservative.o constants.done grids.done kinds_mod.done remap_vars.done timers.done
	touch $(FCM_DONEDIR)/$@

remap_conservative.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/remap_conserv.f90 FFLAGS__nemo__remap_conserv.flags constants.o grids.o kinds_mod.o remap_vars.o timers.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__remap_distwgt.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

remap_distance_weight.done: remap_distance_weight.o constants.done grids.done kinds_mod.done remap_vars.done
	touch $(FCM_DONEDIR)/$@

remap_distance_weight.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/remap_distwgt.f90 FFLAGS__nemo__remap_distwgt.flags constants.o grids.o kinds_mod.o remap_vars.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__remap_read.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

remap_read.done: remap_read.o constants.done grids.done kinds_mod.done netcdf_mod.done remap_vars.done
	touch $(FCM_DONEDIR)/$@

remap_read.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/remap_read.f90 FFLAGS__nemo__remap_read.flags constants.o grids.o kinds_mod.o netcdf_mod.o remap_vars.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__remap_vars.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

remap_vars.done: remap_vars.o constants.done grids.done kinds_mod.done
	touch $(FCM_DONEDIR)/$@

remap_vars.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/remap_vars.f90 FFLAGS__nemo__remap_vars.flags constants.o grids.o kinds_mod.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__remap_write.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

remap_write.done: remap_write.o constants.done grids.done kinds_mod.done netcdf_mod.done remap_vars.done
	touch $(FCM_DONEDIR)/$@

remap_write.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/remap_write.f90 FFLAGS__nemo__remap_write.flags constants.o grids.o kinds_mod.o netcdf_mod.o remap_vars.o
	fcm_internal compile:F nemo $< $@

FFLAGS__nemo__scrip.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS__nemo__scrip.flags: FPPKEYS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__scrip.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__scrip.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

scrip.exe: scrip.o LD__nemo__scrip.flags LDFLAGS__nemo__scrip.flags constants.done grids.done iounits.done kinds_mod.done remap_bicubic.done remap_bilinear.done remap_conservative.done remap_distance_weight.done remap_vars.done remap_write.done timers.done
	fcm_internal load:F nemo $< $@

scrip.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/scrip.F90 FFLAGS__nemo__scrip.flags FPPKEYS__nemo__scrip.flags constants.o grids.o iounits.o kinds_mod.o remap_bicubic.o remap_bilinear.o remap_conservative.o remap_distance_weight.o remap_vars.o remap_write.o timers.o
	fcm_internal compile:F nemo $< $@ 1

FFLAGS__nemo__scripgrid.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS__nemo__scripgrid.flags: FPPKEYS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__scripgrid.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__scripgrid.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

scripgrid.exe: scripgrid.o LD__nemo__scripgrid.flags LDFLAGS__nemo__scripgrid.flags scripgrid_mod.done
	fcm_internal load:F nemo $< $@

scripgrid.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/scripgrid.F90 FFLAGS__nemo__scripgrid.flags FPPKEYS__nemo__scripgrid.flags scripgrid_mod.o
	fcm_internal compile:F nemo $< $@ 1

FFLAGS__nemo__scripgrid_mod.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS__nemo__scripgrid_mod.flags: FPPKEYS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

scripgrid_mod.done: scripgrid_mod.o constants.done iounits.done kinds_mod.done netcdf_mod.done
	touch $(FCM_DONEDIR)/$@

scripgrid_mod.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/scripgrid_mod.F90 FFLAGS__nemo__scripgrid_mod.flags FPPKEYS__nemo__scripgrid_mod.flags constants.o iounits.o kinds_mod.o netcdf_mod.o
	fcm_internal compile:F nemo $< $@ 1

FFLAGS__nemo__scripinterp.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS__nemo__scripinterp.flags: FPPKEYS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__scripinterp.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__scripinterp.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

scripinterp.exe: scripinterp.o LD__nemo__scripinterp.flags LDFLAGS__nemo__scripinterp.flags scripinterp_mod.done
	fcm_internal load:F nemo $< $@

scripinterp.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/scripinterp.F90 FFLAGS__nemo__scripinterp.flags FPPKEYS__nemo__scripinterp.flags scripinterp_mod.o
	fcm_internal compile:F nemo $< $@ 1

FFLAGS__nemo__scripinterp_mod.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS__nemo__scripinterp_mod.flags: FPPKEYS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

scripinterp_mod.done: scripinterp_mod.o constants.done grids.done iounits.done kinds_mod.done netcdf_mod.done remap_mod.done remap_read.done remap_vars.done
	touch $(FCM_DONEDIR)/$@

scripinterp_mod.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/scripinterp_mod.F90 FFLAGS__nemo__scripinterp_mod.flags FPPKEYS__nemo__scripinterp_mod.flags constants.o grids.o iounits.o kinds_mod.o netcdf_mod.o remap_mod.o remap_read.o remap_vars.o
	fcm_internal compile:F nemo $< $@ 1

FFLAGS__nemo__scripshape.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

FPPKEYS__nemo__scripshape.flags: FPPKEYS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LDFLAGS__nemo__scripshape.flags: LDFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

LD__nemo__scripshape.flags: LD__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

scripshape.exe: scripshape.o LD__nemo__scripshape.flags LDFLAGS__nemo__scripshape.flags
	fcm_internal load:F nemo $< $@

scripshape.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/scripshape.F90 FFLAGS__nemo__scripshape.flags FPPKEYS__nemo__scripshape.flags
	fcm_internal compile:F nemo $< $@ 1

FFLAGS__nemo__timers.flags: FFLAGS__nemo.flags
	touch $(FCM_FLAGSDIR)/$@

timers.done: timers.o kinds_mod.done
	touch $(FCM_DONEDIR)/$@

timers.o: /short/e14/ncj561/now/models/little_nemo/TOOLS/WEIGHTS/src/timers.f90 FFLAGS__nemo__timers.flags kinds_mod.o
	fcm_internal compile:F nemo $< $@

# EOF
