Mayank Sharma
Aerospace Engineering Dept.
University of Cincinnati
sharmamm@mail.uc.edu
--------------------------------------------------



Refer to the main documentation (T-Blade3_v1.12_ReadMe.pdf) on the T-Blade3 website (http://gtsl.ase.uc.edu/t-blade3/) to understand how to run T-Blade3 v1.12.

This document details the changes made to T-Blade3 in v1.12.8. 

--------------------------------------------------
RUNNING T-BLADE3
--------------------------------------------------

Once T-Blade3 has been compiled, the code be run from the terminal/command line as follows:

(LINUX)
>> tblade3 3dbgbinput.bladerow.dat argument

(WINDOWS)
>> tblade.exe 3dbgbinput.bladerow.dat argument

The additional command line arguments are as follows:
1) 'xyzstreamlines' - used to create 3D streamline files
2) 'xygrid'         - used to output O-grids for 2D blade sections
3) 'dev'            - used to print additional data files for debugging
4) 'v0'             - used to run with older auxiliary input files


--------------------------------------------------
CHANGES MADE IN T-BLADE3 v1.12.8
--------------------------------------------------

1) Lean and sweep switch added to the main input file, 3dbgbinput.bladerow.dat
    a) If switched to '0': tangential lean and axial sweep are used
    b) If switched to '1': true lean and true sweep are used
2) Clustering control switch added to the main input file, 3dbgbinput.bladerow.dat
    a) If switched to '1': uses sine function based clustering
    b) If switched to '2': uses exponential stretching function based clustering
    c) If switched to '3': uses hyperbolic stretching function based clustering
    d) If switched to '4': uses ellipse based clustering at LE and TE with hyperbolic midchord clustering
    
    Examples:
    "1  2.0": sine function based clustering with clustering_parameter=2.0
              the clustering parameter here is an exponent, a larger value indicates more clustering
    "2  2.0": exponential stretching function based clustering with clusteing_parameter=2.0
              the clustering parameter here denotes the strength of the stretching, a larger value indicates more clustering
    "3  2.0": hyperbolic stretching function based clustering with clustering_parameter=2.0
              the clustering parameter here denotes the strength of the stretching, a larger value indicates more clustering
    "4  31" : elliptical-hyperbolic clustering with clustering_parameter=31
              the clustering parameter here denotes the number of points placed near the LE and TE, a larger value indicates more clustering
              
3) With the exact thickness distribution, the tm/c values specified in the new auxiliary input file, spancotrolinputs.bladerow.dat is treated as the full thickness
   at the specified chord location
4) Log file is now created with every T-Blade3 run
5) ESP override subroutines for spanwise control points specified in the control tables for sweep, lean, in_beta*, out_beta*, chord_multiplier and tm/c in the
   main input file 3dbgbinput.bladerow.dat have been added to updTblade.c, udpHubWedge.c and udpBladeVolume.c











































