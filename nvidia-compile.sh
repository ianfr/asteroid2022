#!/bin/bash

# remove automatic module files, and object files
rm *.mod
rm *.o

nvfortran -c -acc -fast -Minfo -gpu=cc86 -Minline=my_norm2 testfunction_module.f90 && \
  nvfortran -c -acc -fast -Minfo -gpu=cc86 -Minline=my_norm2 type_module.f90 && \
  nvfortran -c -acc -fast -Minfo -gpu=cc86 -Minline=my_norm2 readparams_module.f90 && \
  nvfortran -c -acc -fast -Minfo -gpu=cc86 -Minline=my_norm2 utilities_module.f90 && \
  nvfortran -c -acc -fast -Minfo -gpu=cc86 -Minline=my_norm2 asteroid_module.f90 && \
  nvfortran -c -acc -fast -Minfo -gpu=cc86 -Minline=my_norm2 gravity_module.f90 && \
  nvfortran -c -acc -fast -Minfo -gpu=cc86 -Minline=my_norm2 collision_module.f90 && \
  nvfortran -acc -fast -Minfo -gpu=cc86 -Minline=my_norm2 \
    testfunction_module.o \
    type_module.o \
    readparams_module.o \
    utilities_module.o \
    asteroid_module.o \
    gravity_module.o \
    collision_module.o \
    main_prog.f90 -o main_prog-nvidia