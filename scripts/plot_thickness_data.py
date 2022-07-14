# Thickness vs u (nondimensional chord)
# Written by: Sabrina Shrestha (University of Cincinnati)
# Modified by: Mayank Sharma (GTSL, University of Cincinnati)
from shutil import which
import numpy as np
import matplotlib.pyplot as plt
from cycler import cycler
from matplotlib.backends.backend_pdf import PdfPages
import os
import sys



# Determine if a LaTeX executable exists or not
if which('latex'):
    uselatex = True

# Enable LaTex text formatting
if uselatex:
    plt.rc('font', **{'family':'sans-serif','sans-serif':['Helvetica']})
    plt.rc('text', usetex = True)



# Display message to screen
print ('')
print ('**************************************************************************')
print ('*        T-Blade3 blade section thickness data plotting script           *')
print ('*                                                                        *')
print ('* Default command: $python plot_thickness_data.py                        *')
print ('*                  This will plot thickness distributions of all         *')
print ('*                  spanwise sections in a single plot and store them     *')
print ('*                  in a PDF (thickness_data_plots.pdf)                   *')
print ('*                                                                        *')
print ('* Secondary command: $python plot_thickness_data.py c1 c2 c3 ... cn      *')
print ('*                    The arguments ci can be integers or the word "max"  *')
print ('*                    ci can represent the indices of spanwise sections   *')
print ('*                    If argument "max" is used, this will plot the       *')
print ('*                    max thickness and max thickness locations           *')
print ('*                                                                        *')
print ('* Example command: $python plot_thickness_data.py max                    *')
print ('*                  Plot all thickness distributions and plot max         *')
print ('*                  thickness data in the same plot                       *')
print ('*                                                                        *')
print ('* Example command: $python plot_thickness_data.py 5                      *')
print ('*                  Only plot the thickness distribution for the 5th      *')
print ('*                  spanwise section                                      *')
print ('*                                                                        *')
print ('* Example command: $python plot_thickness_data.py 5 max                  *')
print ('*                  Plot the thickness distribution and max thickness     *')
print ('*                  data for only the 5th section                         *')
print ('*                                                                        *')
print ('* Example command: $python plot_thickness_data.py 5 16 max 21            *')
print ('*                  Plot the thickness distributions and max thickness    *')
print ('*                  data for only the 5th, 16th and 21st sections         *')
print ('**************************************************************************')
print ('')



# Determine the name of the main T-Blade3 input file
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

# Determine the thickness_distribution
thick_distr = int(lines[15])
if thick_distr != 0 and thick_distr != 5:
    sys.exit('Thickness plotting is not available for the current thickness distribution')



# If thickness data for all sections needs to be plotted
if len(sys.argv) == 1:

    # Determine number of spanwise sections
    nsections = int(lines[9])
    sections = np.linspace(1,nsections,nsections)
    sections = sections.astype(int)

# If thickness data for only some sections need to be plotted
if len(sys.argv) > 1:

    # If there is only one command line argument, it can be:
    # 1) 'max' - plot maximum thickness data
    # 2) index of a single section for which thickness data 
    #    has to be plotted
    if len(sys.argv) == 2:
        if sys.argv[1] == 'max':
            plotstatus = 'max'

            # Determine number of spanwise sections
            nsections = int(lines[9])
            sections = np.linspace(1,nsections,nsections)
            sections = sections.astype(int)

        else:

            # Read and store index of single section
            sections = np.zeros(1)
            sections[0] = int(sys.argv[1])
            sections = sections.astype(int)

    elif len(sys.argv) > 2:

        # Append all section indices to a list
        sections_list = []
        for i in range(1,len(sys.argv)):
            if sys.argv[i] == 'max':
                plotstatus = 'max'
            else:
                sections_list.append(sys.argv[i])

        # Convert section indices list to a numpy array
        sections = np.zeros(len(sections_list))
        for i in range(0, np.size(sections)):
            sections[i] = int(sections_list[i])
        sections = sections.astype(int)



# Plot thickness for prescribed sections
# Stored in the same PDF
with PdfPages('thickness_data_plots.pdf') as pdf:
    for i_sec in sections:

        # Read the thickness data files
        filename = 'thickness_data.' + str(i_sec) + '.' + casename
        
        # Thickness data file for Wennerstrom thickness distribution
        if thick_distr == 0:
            u, thk = np.loadtxt(filename, unpack=True)

            # Store max thickness and max thickness location if needed
            if 'plotstatus' in vars() and plotstatus == 'max':
                u_max = u[np.argmax(thk)]
                t_max = thk[np.argmax(thk)]

        # Thickness data file for modified four digit NACA thickness distribution
        elif thick_distr == 5:
            u, thk, thk_d, thk_dd = np.loadtxt(filename, unpack=True)
            
            # Store max thickness and max thickness location if needed
            if 'plotstatus' in vars() and plotstatus == 'max':
                u_max = u[np.argmax(thk)]
                t_max = thk[np.argmax(thk)]

        # Plot thickness data
        plt.plot(u, thk, label='%d'%i_sec)
        if 'plotstatus' in vars() and plotstatus == 'max':
            plt.plot(u_max, t_max, 'kx')
        plt.xlabel(r'$u$', fontsize=16)
        plt.ylabel(r'$v_{t}(u)$', fontsize=16)
        plt.title(r'Thickness distribution in ($u,v$)', fontsize=16)

    # Save PDF
    art = []
    lgd = plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    art.append(lgd)
    pdf.savefig(bbox_extra_artists=art, bbox_inches='tight')

plt.close()
