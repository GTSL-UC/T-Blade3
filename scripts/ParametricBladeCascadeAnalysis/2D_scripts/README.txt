PARAMETRIC BLADE CASCASE ANALYIS (PBCA)

A python based suite of codes used for the 2D analysis of parametrically defined blades. The model driver is OpenMDAO in Python3, 2D CFD is conducted using MISES of MIT's Mark Drela, and the parametric blade generator is T-Blade3 of the University of Cincinnati's Mark Turner. 

The current capabilies of this code are the full optimizations of complete 3D blade shapes using 2D CFD, design of experiement developement and tests, and plotting and post processing abilities.



******* TUTORIAL *******
The root.py python file contains the class ParametricBladeCascadeAnalysis. To begin the setup of the PBCA tool, initalize your project file in the 2D_scipts folder that also contains the root.py file. The class ParametricBladeCascadeAnalysis must first be imported into your python script using the following line:

**example code**
from root import ParametricBladeCascadeAnalysis as PBCA
****

Once PBCA has been imported, an object may be initailized by calling PBCA with the appropiate inputs. An example of a correctly inialized object with the available input options is displayed below. 


simulation1 = PBCA(spans, blades, RE, MISES_path, Tblade3_path off_deg, step_up_flag)

** spans = [1.00, 0.00, 0.75, 0.25, 0.50]
The spans input is a list of values to specify which spanwise locations to optimize using T-Blade3 formatted span values. The available options for spans to update are: 0.00, 0.25, 0.50, 0.75, and 1.00. Spans will be optimized in the order they are entered as the input. 

** blades = [1_blade, 2_blade]
The blades input informs the optimizer of which blade rows to optimize. Blade rows must be formatted as follows: [BladeNum]_blade (ex. for blade 3, 3_blade). Corresponding T-Blade3 and MISES inputs must exist in the PBCA input directory. Blade names must also match within input files.

** RE = [4435945.47, 4390566.25]
Reynolds number input for each blade row. A Reynolds number input is reqired for each row that is being run. 

** MISES_path = 
File path that points to the MISES program "ises", "iset", "iprint" and "iplot".

** Tblade3_path = 
File path that points to the T-Blade3 program "tblade3" and "techop"

** off_deg = 7
Single integer value larger than 0. Indicates the number of degrees of added incidence for the off design point during simulation, constant for all blade rows. Defaults to 7 degrees. 

** step_up_flag = 0
Single integer flag of either 0 or 1. Defaults to 0, off position. When switched to 1, on position, a Mach number step up method is implemented where blade row optimization begins at a lower Mach number to assist with CFD convergance. Once an optimal blade is found at the lower Mach, optimization at an incrementally larger Mach number is initalized with the lower Mach optimal geometry. This method can assist with the sucessful optimization of blades that are challenging for the 2D CFD to solve. 




Following the initalization of PBCA, optimization may be run by simply calling the optimizer function within PBCA on the object simulation1. 

**example code**
simulation1.optimizer()
****


********** Example scripts to run various optimizations **********
(1) Single blade optimization without step up implementation, 7 degrees off design point

from root import ParametricBladeCascadeAnalysis as PBCA
ises_path = '/home/hamw/Document_0c/Programs/Mises2.70/bin'
tblade_path = '/home/hamw/Document_0c/Programs/T-Blade3/bin'
simulation1 = PBCA([1.00, 0.00, 0.75, 0.25, 0.50],['1_blade'],[3454477.23287],ises_path,tblade_path,7,0)
simulation1.optimizer()


(2) Three blade optimization without step up implementation, 5 degrees off design point

from root import ParametricBladeCascadeAnalysis as PBCA
ises_path = '/home/hamw/Document_0c/Programs/Mises2.70/bin'
tblade_path = '/home/hamw/Document_0c/Programs/T-Blade3/bin'
simulation1 = PBCA([1.00, 0.00, 0.75, 0.25, 0.50],['1_blade','2_blade','3_blade'],[3454477.23, 4435945.47, 4390566.25],ises_path,tblade_path,5,0)
simulation1.optimizer()


(3) Two blade optimization at casing then at hub with step up implementation, 7 degrees off design point

from root import ParametricBladeCascadeAnalysis as PBCA
ises_path = '/home/hamw/Document_0c/Programs/Mises2.70/bin'
tblade_path = '/home/hamw/Document_0c/Programs/T-Blade3/bin'
simulation1 = PBCA([1.00, 0.00],['2_blade','3_blade'],[3454477.23, 4390566.25],ises_path,tblade_path,7,1)
simulation1.optimizer()

