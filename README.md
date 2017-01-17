# T-Blade3
T-Blade3 is a general parametric 3D blade geometry builder. The tool can create a variety of 3D blade geometries based on few basic parameters and limited interaction with a CAD system. The geometric and aerodynamic parameters are used to create 2D airfoils and these airfoils are stacked on the desired stacking axis. The tool generates a specified number of 2D blade sections in a 3D Cartesian coordinate system. The geometry modeler can also be used for generating 3D blades with special features like bent tip, split tip and other concepts, which can be explored with minimum changes to the blade geometry. The use of control points for the definition of splines makes it easy to modify the blade shapes quickly and smoothly to obtain the desired blade model. The second derivative of the mean-line (related to the curvature) is controlled using B-splines to create the airfoils. This is analytically integrated twice to obtain the mean-line. A smooth thickness distribution is then added to the airfoil with two options either the Wennerstrom distribution or a quartic B-spline thickness distribution. B-splines have also been implemented to achieve customized airfoil leading and trailing edges.

T-Blade3 Copyright (C) 2017 University of Cincinnati, developed by Dr. Mark Turner, Karthik  Balasubramanian, Syed Moez Hussain, Ahmed Farid Nemnem, Kiran Siddappaji and Marshall C. Galbraith.

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.  For the complete terms of the GNU General Public License, please see this URL:
http://www.gnu.org/licenses/gpl-2.0.html 

Inputs: LE and TE curve(x,r), inlet and exit angles, chord, tm/c, incidence, deviation,construction line cordinates:(x,r),
        control points for sweep, lean, blade scaling factor. 
        
Outputs: 3D blade sections (x,y,z), 2D airfoils (mprime,theta), 2D grids with periodic boundaries.

Below are the authors:

Kiran Siddappaji         
Mark G. Turner (turnermr@ucmail.uc.edu)           
Karthik Balasubramanian  
Syed Moez Hussain Mahmood
Ahmed Farid Nemnem             
Marshall C. Galbraith  


Please visit the website http://gtsl.ase.uc.edu/t-blade3/ for more information.
