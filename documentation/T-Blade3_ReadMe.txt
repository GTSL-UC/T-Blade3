Mayank Sharma
Aerospace Engineering Dept.
University of Cincinnati
sharmamm@mail.uc.edu
--------------------------------------------------



Refer to the main documentation (T-Blade3_v1.13.1_ReadMe.pdf) on the T-Blade3 website (http://gtsl.ase.uc.edu/t-blade3/) to understand how to run T-Blade3 v1.13.1.

This document details the changes made to T-Blade3 in v1.2

--------------------------------------------------
RUNNING T-BLADE3
--------------------------------------------------

Once T-Blade3 has been compiled, the code can be run from the terminal/command line as follows:

(LINUX)
>> tblade3 3dbgbinput.bladerow.dat argument

(WINDOWS)
>> tblade.exe 3dbgbinput.bladerow.dat argument

The additional command line arguments are as follows:
1) 'xyzstreamlines' - used to create 3D streamline files
2) 'xygrid'         - used to output O-grids for 2D blade sections
3) 'dev'            - used to print additional data files for debugging


--------------------------------------------------
CHANGES MADE IN T-BLADE3 v1.13.1
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
              
3) Log files are now created with every T-Blade3 run

4) A new thickness distribution based on the modified four-digit NACA thickness has been added
    a) The thickness distribution switch in the main input file needs to be switched to "5" to use this thickness distribution
    b) A new auxiliary input file, "spancontrolinputs_NACA_bladerow.dat" has also been added
    c) Needs specification of the leading edge radius, the maximum thickness (tm/c), the chordwise location of tm/c and the trailing edge thickness
    d) A spline switch has been added to the new auxiliary input file
        i)  If switched to '1': uses a cubic B-spline for spanwise variation
        ii) If switched to '2': uses a cubic spline for spanwise variation

5) ESP override subroutines for spanwise control points specified in the control tables for sweep, lean, in_beta*, out_beta*, chord_multiplier and tm/c in the
   main input file 3dbgbinput.bladerow.dat have been added to updTblade.c, udpHubWedge.c and udpBladeVolume.c

6) ESP override subroutines for spanwise control points specified in the control tables for u, thk, letht and tetht involved in the exact thickness distribution
   have been added to udpTblade.c, udpHubWedge.c and udpBladeVolume.c

7) ESP override subroutines for spanwise control tables for LE_radius, u_max, t_max and t_TE involved in the modified four-digit NACA thickness distribution have 
   been added to udpTblade.c, udpHubWedge.c and udpBladeVolume.c
