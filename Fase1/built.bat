@echo off

del myapp.exe

REM this file is used to build the project
gfortran -c structure/cola_module.f90 -o library/cola.o
gfortran -c structure/linkedlist.f90 -o library/linkedlist.o
gfortran -c main.f90 -o library/main.o

REM linking the object files 
rem gfortran -o executable/myapp  executable/main.o executable/linkedlist.o

gfortran -o myapp.exe  library/main.o library/linkedlist.o library/cola.o



REM run the executable
rem cd executable
rem .\myapp.exe
