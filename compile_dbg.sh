#!/bin/bash

# remove automatic module files, and object files
rm *.mod
rm *.o

gfortran -g -c -std=f2008 -Wextra -Wall -pedantic testfunction_module.f90 && \
  gfortran -g -c -std=f2008 -Wextra -Wall -pedantic type_module.f90 && \
  gfortran -g -c -std=f2008 -Wextra -Wall -pedantic readparams_module.f90 && \
  gfortran -g -c -std=f2008 -Wextra -Wall -pedantic utilities_module.f90 && \
  gfortran -g -c -std=f2008 -Wextra -Wall -pedantic asteroid_module.f90 && \
  gfortran -g -c -std=f2008 -Wextra -Wall -pedantic gravity_module.f90 && \
  gfortran -g -c -std=f2008 -Wextra -Wall -pedantic collision_module.f90 && \
  gfortran -g -std=f2008 -Wextra -Wall -pedantic \
    testfunction_module.o \
    type_module.o \
    readparams_module.o \
    utilities_module.o \
    asteroid_module.o \
    gravity_module.o \
    collision_module.o \
    main_prog.f90 -o main_prog-dbg