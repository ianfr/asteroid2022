#!/bin/bash

# function files
gfortran -c -std=f2008 -Wextra -Wall -pedantic testfunction_module.f90
gfortran -c -std=f2008 -Wextra -Wall -pedantic type_module.f90
gfortran -c -std=f2008 -Wextra -Wall -pedantic readparams_module.f90
gfortran -c -std=f2008 -Wextra -Wall -pedantic utilities_module.f90

# main file
gfortran -std=f2008 -Wextra -Wall -pedantic \
  testfunction_module.o \
  type_module.o \
  readparams_module.o \
  utilities_module.o \
  main_prog.f90 -o main_prog