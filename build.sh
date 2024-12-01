#!/bin/bash

fc=gfortran

flags="-cpp -Wall -Wextra -Wno-tabs -fbounds-check"

install_dir="/usr/include/fortran_stdlib/GNU-14.2.1/"
lib_dir="/usr/lib/fortran_stdlib/"


$fc -o main main.f90 $flags -I $install_dir -L $lib_dir -lfortran_stdlib
