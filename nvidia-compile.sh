#!/bin/bash

# remove automatic module files, and object files
rm *.mod
rm *.o

nvfortran -c -acc -fast -Minfo testfunction_module.f90 && \
  nvfortran -c -acc -fast -Minfo type_module.f90 && \
  nvfortran -c -acc -fast -Minfo readparams_module.f90 && \
  nvfortran -c -acc -fast -Minfo utilities_module.f90 && \
  nvfortran -c -acc -fast -Minfo asteroid_module.f90 && \
  nvfortran -c -acc -fast -Minfo gravity_module.f90 && \
  nvfortran -c -acc -fast -Minfo collision_module.f90 && \
  nvfortran -acc -fast -Minfo \
    testfunction_module.o \
    type_module.o \
    readparams_module.o \
    utilities_module.o \
    asteroid_module.o \
    gravity_module.o \
    collision_module.o \
    main_prog.f90 -o main_prog-nvidia