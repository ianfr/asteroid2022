#!/bin/bash

# remove automatic module files, and object files
rm *.mod
rm *.o

ifort -stand f08 -O2 -c -warn all testfunction_module.f90 && \
  ifort -stand f08 -O2 -c -warn all type_module.f90 && \
  ifort -stand f08 -O2 -c -warn all readparams_module.f90 && \
  ifort -stand f08 -O2 -c -warn all utilities_module.f90 && \
  ifort -stand f08 -O2 -c -warn all asteroid_module.f90 && \
  ifort -stand f08 -O2 -c -warn all gravity_module.f90 && \
  ifort -stand f08 -O2 -warn all \
    testfunction_module.o \
    type_module.o \
    readparams_module.o \
    utilities_module.o \
    asteroid_module.o \
    gravity_module.o \
    main_prog.f90 -o main_prog