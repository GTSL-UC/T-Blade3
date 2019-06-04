-----------------------------------------------
T-Blade3: Turbomachinery 3D Blade Generator 
Version 1.2 - 4 June 2019
-----------------------------------------------
by Mayank Sharma
Dept. of Aerospace Engineering
University of Cincinnati
sharmamm@mail.uc.edu
-----------------------------------------------

1.  INTRODUCTION

    T-Blade3 version 1.2 has been integrated with Engineering Sketch Pad (ESP) which is a 
    geometry creation and manipulation system targeted at aerospace design and analysis. ESP
    is used to drive T-Blade3 via user defined primitives (UDPs) which generate a single
    solid. Various spanwise parameters provided by T-Blade3 can be further manipulated using
    ESP input files which have a .csm extension. The resulting solids can be written in 
    .step files. Three UDPs have been provided with this release of T-Blade3:

    1. udpTblade.c         - used to generate a 3D blade
    2. udpHubWedge.c       - used to generate a blade and hub union (linux only)
    3. udpBladeVolume.c    - used to generate a fluid volume around a 3D blade (linux only)

    The latest ESP source code can be obtained here: https://acdl.mit.edu/ESP/ 
    
    This document explains how to compile ESP 1.13 using T-Blade3 version 1.2 so that NACA 
    thickness controls can be used. 

    ESP 1.14 is the latest release with ESP but is not being used for geometry generation 
    with T-Blade3 at this moment. Users are thus directed to the package ESP1.13.tgz at 
    https://acdl.mit.edu/ESP/archive
    
2.1 T-BLADE3 SOURCE DISTRIBUTION LAYOUT
    
    $TBLADEROOT will be used here as the name of the directory that contains:
   
    README                  -   file containing instructions on buliding T-Blade3
    Copyright.txt           -   file containing information pertaining to the use
                                and distribution of T-Blade3
    GitHub.address          -   file containing T-Blade3 GitHub repository URL
    Makefile                -   master makefile for compiling T-Blade3/test suite/ESP UDPs
    license.txt             -   T-Blade3 license information
    source                  -   source code for T-Blade3 and ESP UDPs (contains sub makefiles
                                for T-Blade3 and ESP)
    tests                   -   subdirectory which contains unit tests for T-Blade3 (also
                                contains sub makefile for the test suite)
    documentation           -   subdirectory which contains additional documentation for 
                                T-Blade3
    inputs                  -   directory containing various example input files 

2.2 ESP SOURCE DISTRIBUTION LAYOUT

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

3.  INSTRUCTIONS

    1. Refer to $ESPROOT/README.txt for instructions to compile ESP for the first time.
    2. Change to the following directory:
       
       $ cd ESPROOT/src/OpenCSM/
       $ rm -rf Tblade3
       $ git clone https://github.com/GTSL-UC/T-Blade3.git
       $ cd T-Blade3
       $ git fetch origin
       $ git checkout develop

    3. In $TBLADEROOT, execute make:
            
       $ make clean_esp
       $ make esp

    4. This will generate the executable .serveCSM in $ESPROOT/bin
    5. The sample input files in $TBLADEROOT/inputs/OpenCSM can now be run and modified
