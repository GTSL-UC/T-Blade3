Mayank Sharma
Aerospace Engineering Dept.
University of Cincinnati
sharmamm@mail.uc.edu
--------------------------------------------------



Refer to the main documentation (T-Blade3_v1.13.1_ReadMe.pdf) on the T-Blade3 website (http://gtsl.ase.uc.edu/t-blade3/) to understand how to run T-Blade3 v1.13.1. To
generate updated documentation for T-Blade3 v1.2, use the file T-Blade3_v1.2_ReadMe.tex in this directory.

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
2) 'dev'            - used to print additional data files for debugging


--------------------------------------------------
CHANGES MADE IN T-BLADE3 v1.2
--------------------------------------------------

1) A new thickness distribution based on the modified four-digit NACA thickness has been added
    a) The thickness distribution switch in the main input file needs to be switched to "5" to use this thickness distribution
    b) Needs specification of the leading edge radius, the maximum thickness (tm/c), the chordwise location of tm/c, the trailing edge thickness and optionally
       the first derivative of thickness at the trailing edge
    c) This thickness distribution uses a circular TE instead of a blunt/sharp TE
    d) A spline switch has been added to the new auxiliary input file
        i)  If switched to '1': uses a cubic B-spline for spanwise variation
        ii) If switched to '2': uses a cubic spline for spanwise variation

2) The auxiliary input file spancontrolinputs_NACA_bladerow.dat created with T-Blade3 v1.13.1 has been changed back to spancontrolinputs.bladerow.dat

3) The directory structure has been changed from v1.13.1

4) Compilation is now controlled by a master Makefile in $TBLADEROOT (the top-level T-Blade3 directory). Refer to $TBLADEROOT/README or T-Blade3_v1.2_ReadMe.tex
   for more information

5) (m',theta) grid generation (O-grids and background grids) present in earlier versions has been separated from the main code with the input files in 
   $TBLADEROOT/inputs being changed accordingly

6) Error trapping has been included in many major subroutines. This now leads to the creation of an error log file with each T-Blade3 run. If there are no fatal
   errors, errors or warnings issued, the error log file is empty

7) A test suite containing unit tests (presently) has been added to $TBLADEROOT/tests. Testing requires a serial installation of pFUnit
