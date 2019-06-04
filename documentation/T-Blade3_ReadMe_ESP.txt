-----------------------------------------------
T-Blade3: Turbomachinery 3D Blade Generator 
Version 1.13.1 - 4 March 2019
-----------------------------------------------
by Mayank Sharma
Dept. of Aerospace Engineering
University of Cincinnati
sharmamm@mail.uc.edu
-----------------------------------------------

1.  Introduction

    T-Blade3 version 1.13.1 has been integrated with Engineering Sketch Pad (ESP) which is a 
    geometry creation and manipulation system targeted at aerospace design and analysis. ESP
    is used to drive T-Blade3 via user defined primitives (UDPs) which generate a single
    solid. Various spanwise parameters provided by T-Blade3 can be further manipulated using
    ESP input files which have a .csm extension. The resulting solids can be written in 
    .step files. Three UDPs have been provided with this release of T-Blade3:

    1. udpTblade.c         - used to generate a 3D blade
    2. udpHubWedge.c       - used to generate a blade and hub union (linux only)
    3. udpBladeVolume.c    - used to generate a fluid volume around a 3D blade (linux only)

    The latest ESP source code can be obtained here: https://acdl.mit.edu/ESP/ 
    
    This document explains how to compile ESP 1.13 using T-Blade3 version 1.13.1 so that exact 
    thickness controls can be used.
    
2.1 T-Blade3 Source Distribution Layout
    
    $TBLADEROOT will be used here as the name of the directory that contains:
    
    3dbgb_ReadMe_New.txt    -   instruction on how to run T-Blade3
    T-Blade3_ReadMe_ESP.txt -   this file
    github.address          -   file containing T-Blade3 GitHub repository URL
    Makefile                -   makefile for compiling T-Blade3
    MakefileESP             -   makefile to be used when compiling ESP to run T-Blade version
                                1.13.1
    README.md               -   readme file
    license.txt             -   T-Blade3 license information
    inputs                  -   directory containing various example input files 
    *.f90                   -   T-Blade3 source files
    *.c                     -   ESP UDPs 

2.2 ESP Source Distribution Layout

    $ESPROOT will be used here as the name of the directory that contains:

    README.txt              -   main documentation for ESP
    bin                     -   a directory that will contain executables
    config                  -   files that allow for aautomatic configuration
    data                    -   test and example scripts
    doc                     -   documentation
    ESP                     -   web client code for the Engineering Sketch Pad
    externApps              -   the ESP connections to 3rd party Apps (outside of CAPS)
    include                 -   location for all ESP header files
    lib                     -   a directory that will contain libraries, shared objects
                                and DLLs
    SLUGS                   -   the browser code for Slugs (web Slugs client)
    src                     -   source files (contains EGADS, CAPS, wvServer & OpenCSM)
    training                -   training slides and examples
    udc                     -   a collection of User Defined Components
    wvClient                -   simple examples of Web viewing used by EGADS

3.  Instructions

    1. Refer to $ESPROOT/README.txt for instructions to compile ESP for the first time.
    2. Copy $TBLADEROOT/Makefile, $TBLADEROOT/MakefileESP, $TBLADEROOT/*.f90 and 
       $TBLADEROOT/*.c and paste them to $ESPROOT/src/OpenCSM/Tblade3
    3. In the terminal, run:
            
       $ESPROOT/src/OpenCSM/Tblade3% make -f MakefileESP cleanall
       $ESPROOT/src/OpenCSM/Tblade3% make -f MakefileESP 

    4. This will generate the executable .serveCSM in $ESPROOT/bin
    5. The sample input files in $TBLADEROOT/inputs/OpenCSM can now be run and modified
