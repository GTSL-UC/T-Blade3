# Script to plot meanline slope u (nondimensional chord)
# Written by Sabrina Shrestha (University of Cincinnati)
# Modified by Mayank Sharma (GTSL, University of Cincinnati)
from shutil import which
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from cycler import cycler
import os
import sys



# Determine whether a LaTeX executable exists or not
if which('latex'):
    uselatex = True

# Enable LaTeX text formatting
if uselatex:
    plt.rc('font', **{'family':'sans-serif','sans-serif':['Helvetica']})
    plt.rc('text', usetex=True)



# Display message to screen
print ('')
print ('**************************************************************************')
print ('*        T-Blade3 blade section meanline slope plotting script           *')
print ('*                                                                        *')
print ('* Default command: $python plot_meanline_data.py                         *')
print ('*                  This will plot meanline slopes of all spanwise        *')
print ('*                  sections in a single plot and store them in a PDF     *')
print ('*                  (meanline_slope_vs_u.pdf)                             *')
print ('*                                                                        *')
print ('* Secondary command: $python plot_meanline_data.py c1 c2 c3 ... cn       *')
print ('*                    The arguments ci should be integers                 *')
print ('*                    ci represent the indices of spanwise sections       *')
print ('*                                                                        *')
print ('* Example command: $python plot_meanline_data.py 5 16 21                 *')
print ('*                  Will plot the meanline data for the 5th, 16th         *')
print ('*                  and 21st sections                                     *')
print ('**************************************************************************')
print ('')



# Determine the name of the main T-Blade3 input filea
current_dir = os.getcwd()
files = os.listdir(current_dir)
for file in files:
    if "3dbgbinput" in file and "dat" in file:
        input_file = file.strip()
        break

# Read main T-Blade3 input file and store the input in a list
f = open(input_file, 'r')
lines = f.readlines()
f.close()

# Determine the casename
casename = lines[1].strip()



# If meanline data for all sections needs to be plotted
if len(sys.argv) == 1:
    
    # Determine the number of spanwise section
    nspan = int(lines[9])
    sections = np.linspace(1,nspan,nspan)
    sections = sections.astype(int)

# If meanline data for only some sections needs to be plotted
elif len(sys.argv) > 1:

    # Read and store indices of all sections to be plotted
    sections = np.zeros(len(sys.argv) - 1)
    for i in range(1,len(sys.argv)):
        sections[i - 1] = int(sys.argv[i])
    sections = sections.astype(int)



# Read the stagger angle file and convert to degrees
stagger_file = 'stagger_angles.dat'
stag = np.loadtxt(stagger_file,dtype=float,unpack=True)
stagger = stag*(180/np.pi)



# Empty list to which legend labels will be appended
legend_str = []



# Plot meanline slope vs. u for all spanwise sections
# Stored in the same PDF
with PdfPages('meanline_slope_vs_u.pdf') as pdf:
    
    # Read the file 
    for i_sec in sections:
        
        filename = 'curvature_data.' + str(i_sec) + '.' + casename
        u, camber, slope, curv  = np.loadtxt(filename, unpack=True)


        # Compute angle in [-pi/2,pi/2] 
        angle = np.zeros(len(slope))
        for i in range(0, len(slope)):
            angle[i] = np.arctan(slope[i])
            
        # Plot the blade angles
        plt.plot(u, angle*(180/np.pi))
        if uselatex:
            plt.xlabel(r'$u$', fontsize=14)
            plt.ylabel(r'${tan} ^{-1} \left(\frac{dv}{du} \right)$', fontsize=14)
            plt.title(r'Blade angles in ($u,v$) plane', fontsize=14)
        else:
            plt.xlabel("u", fontsize=14)
            plt.ylabel("meanline slope (deg)", fontsize=14)
            plt.title("Blade angles in (u, v) plane", fontsize=14)

        # Generate legend labels for each section to be plotted
        legend_str.append('%d: %.3f'%(i_sec, stagger[i_sec - 1]))

    # Plot legend
    plt.legend(legend_str,
                title='Key and Stagger angles (deg)',loc='upper left',prop={'size':7},ncol=3)

    # Save PDF
    pdf.savefig()

plt.close()
