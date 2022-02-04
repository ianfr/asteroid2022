#!/bin/bash

# function files
ifort -stand f08 -c -warn all testfunction_module.f90
ifort -stand f08 -c -warn all type_module.f90
ifort -stand f08 -c -warn all readparams_module.f90
ifort -stand f08 -c -warn all utilities_module.f90

# main file
ifort -stand f08 -warn all \
  testfunction_module.o \
  type_module.o \
  readparams_module.o \
  utilities_module.o \
  main_prog.f90 -o main_prog