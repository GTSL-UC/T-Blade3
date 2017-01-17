----------------------------------------------
3 Dimensional Blade Geometry Builder (3DBGB)
----------------------------------------------
by Kiran Siddappaji
Aerospace Engineering Dept.
University of Cincinnati
s2kn@mail.uc.edu
--------------------------------------------------

--------------------------------------------------
INSTRUCTIONS TO USE 3DBGB
--------------------------------------------------
This code creates specified number of 3D blade section data files in xyz coordinates.
Executable:3dbgb

Instructions to use the code and details about the input file:-

1. The input file is called '3dbgbinput.#.dat' where # represents the bladerow number.

2. The output files are 
  (a) sec$.dat, sec$.csv :$ number of 3D blade section files containing xyz coordinates.
  (b) blade.$.#.casename: 2D blade section files containing m', theta coordinates.
  (c) blade3d.dat : File containing unscaled xyz coordinates of all the blade sections.
  (d) meanline.sec$.dat, meanline.sec$.csv : meanline coordinates of the airfoil in 3D.

3. Command to run the code is :>3dbgb 3dbgbinput.#.dat
                               # represents the bladerow number.

4. Details about the input file :~
  (i)    case names: e3c = EEE HPC full defintion          (21 sections)
				
  (i.a) Blade row number.

  (i.b) Number of blades in the current blade row.  

  (ii)   Blade scaling factor: The output coordinates is in mm. This value is used to scale the non-dimensional blade
                           to the desired scale.

  (iii)  Number of streamlines: Currently 21. It can be as many as desired by the user.
  
  (iii.a)Switches for angles, camber definition, thickness and LE/TE definition.

  (iv)   Non-dimensional Actual Chord switch (0=no, 1=yes, 2 = spline): Enabling this switch to 1 instructs the code to use the actual chord 
                                                         values from the  'Sectionwise properties' table rather than calculating the meridional
                                                         chord values ( all non dimensional) and converting them to the
                                                         actual values.
														 
														 2 =  spline : The chord can be controlled using a spline definition radially.
 
  (v)   Sectionwise properties: The table is self explanatory about the type of values. Angles used are Beta_Z.
  
  (v.a)  LE/TE curve definition at hub and tip or more if needed.

  (vi)   Airfoil type: This table defines the type of airfoil sections to be used such as 
                    sect1: default type
                    naca4: NACA 4 digit
                    s809m: S809 with modified tip (less sharp)
                    crcle: circular type
					user defined airfoil type
					
		> For designing Turbine airfoils a curvature controlled camber definition, spline LE and thickness multiplier option is available as switches.
		> Keep the airfoil type as 'sect1'.
		> Another input is needed called as 'controlinputs.dat' which contains the control points for the curvature, thickness of the turbine airfoil,
		   LE and TE thicknesses, LE deflection, LE elongation, curvature at LE (x,y).
		> Example folder called 'UC_turbine' has the sample input files for a turbine design.
					

  (vii)
        Switch for Variable Radial Stacking: Activating this switch enables the user to stack the airfoils radially at different locations. 
         stack_u, stack_v : stacking values along the chord and perpendicular to the chord. 
                            The stacking can be varied radially by activating the switch here and using different values.
         umxthk : location of max thickness along the chord for an airfoil.
		 lethk,tethk : thickness of the LE and TE. Do not use these while designing turbine airfoils. Instead, use the lethk and tethk parameters in 'controlinputs.dat'

  (viii) Stacking axis as a fraction of chord: The user can define the position of the stacking axis as a fraction of chord.
                                             000000 : Stacking at Leading Edge
                                             100000 : Stacking at Trailing Edge
                                             025000 : Stacking at 25% of the chord 
                                             200000 : Stacking at the centre of area of the airfoil

  (ix)   Control points for various quantities from the first table:- The number of control points can be as many as desired.
                                                                      First column is blade span ranging from 0.0 to 1.0.

                                                                      Second column is the value in control points which generates the
                                                                               value function( cubic spline).

  (x)    Hub and Tip offset:- Offset values as a fraction to define the hub and tip offset percentage.

  (xi)   Streamline Data: x,r coordinates of the specified number of streamlines. The streamline data are separated by '0        0'.
  
  








































