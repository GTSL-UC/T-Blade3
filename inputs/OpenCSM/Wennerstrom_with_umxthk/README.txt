CASE SPECIFIC INFORMATION

Geometry includes one blade. Uses 3dbgbinput.1.dat instead of .3.dat. This blade is 
in row 1 which contains 3 blades. 

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
3) Refer to the documents T-Blade3_v1.12_ReadMe.pdf and updated_compilation.pdf 
   on the T-Blade3 website, gtsl.ase.uc.edu/t-blade3 for instructions on compiling 
   T-Blade3 from source 
4) Refer to the ESP README file available with the source code and the document
   T-Blade3_readMe_ESP.txt available with the T-Blade3 source cose to set up
   ESP with T-Blade3
   or
   Refer to the document T-Blade3_ESP_Integration.zip and the file within it,
   ESP_T-Blade3_build.txt, for instructions on integrating T-Blade3 with ESP

The user is strongly advised to read the documentation attached to both codes 
to understand their operation.

RUNNING CASE
1) To run case with T-Blade3:
   /path/to/executable/.tblade /path/to/file/3dbgbinput.1.dat
2) To run with ESP:
   /path/to/executable/.serveCSM /path/to/file/tblade_Wennerstrom.csm

CHANGING PARAMETERS 
To change additional parameters in the blade geometry, locate the 
parameter with "tblade" in the second column and uncomment it. 

