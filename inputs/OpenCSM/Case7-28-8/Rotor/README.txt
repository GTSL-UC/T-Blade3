CASE SPECIFIC INFORMATION

Rotor and hubwedge for the 7-28-8 case. This rotor blade is in row 2 which contains 
16 blades. The geometry includes one axial rotor blade and its hubwedge. The rotor 
and hubwedge are merged but not filleted. There is a cylinder cut out of the bottom 
of the hubwedge to fit together the whole row of the axial compressor. 

This case is used for regression testing. For regression testing instructions, 
see the last section of this README.

PREREQUISITES

1) gcc/gfortran 6.0 or above
   - clang can be used as an alternative to gcc/g++
2) git
3) make

SOURCE CODE AND INSTALLATION

1) The T-Blade3 source code and documentation can be obtained from 
   https://github.com/GTSL-UC/T-Blade3.git
2) The ESP source code/binaries and documentation can be obtained from 
   https://acdl.mit.edu/ESP
3) Refer to the documents T-Blade3_v1.2_ReadMe.pdf and updated_compilation.pdf 
   on the T-Blade3 website, gtsl.ase.uc.edu/t-blade3 for instruction on compiling
   T-Blade3 from source
4) Refer to the ESP README file available with the source code and the document
   T-Blade3_ReadMe_ESP.txt available with the T-Blade3 source code to set up
   ESP with T-Blade3
   or
   Refer to the document T-Blade3_ESP_Integration.zip and the file within it,
   ESP_T-Blade3_build.txt, for instructions on integrating T-Blade3 with ESP

The user is strongly advised to read the documentation attached to both codes
to understand their operation.

RUNNING CASE

1) To run case with T-Blade3:
   /path/to/executable/.tblade3 /path/to/file/3dbgbinput.2.dat
2) To run with ESP:
   /path/to/executable/.serveCSM /path/to/file/tblade_rotor_hubwedge.csm

CHANGING PARAMETERS 
To change additional parameters in the blade geometry, locate the 
parameter with "tblade" in the second column and uncomment it. Similarly, if the 
geometry contains a hubwedge design, locate the parameter with "hubwedge" in the
second column and uncomment the parameter. 

NOTE: To run ESP to create the combined blade and hubwedge model, use the 
      file tblade_rotor_hubwedge.csm
	  or
	  To run ESP to create just the blade model, use the file
	  tblade_rotor_hubwedge_blade.csm 



REGRESSION TESTING INSTRUCTIONS

$ESPROOT refers to the ESP top-level directory.
$TBLADE3 refers to the T-Blade3 top-level directory.

The regression test for udpTblade.c is located in
$TBLADE3/tests/regtest_blade_UDP.pf. Running and
compiling the test suite using pFUnit is described in
the document $TBLADE3/documentation/updated_compilation.pdf.

The test runs ESP (with udpTblade.c) to generate output files
which are compared to the reference files located in the case
directory:
$TBLADE3/inputs/OpenCSM/new_inputs/Case7-28-8/Rotor/regtest_files

The test assumes that the executable serveCSM is located in
$ESPROOT/bin/ and that the system knows the path of this
executable. Adding the path to the global PATH has also been
described in the document ESP_T-Blade3_build.dat. Both of
these conditions must be met to run the test successfully.

To include this regression test when compiling and running the
test suite, add the following line to the file
$TBLADE3/tests/testSuites.inc:
"ADD_TEST_SUITE(regtest_blade_UDP_suite)"

