# Script to plot (m', \theta) sections
# Written by Sabrina Shrestha (University of Cincinnati)
# Modified by Mayank Sharma (GTSL, University of Cincinnati)
from shutil import which
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import os
import sys
import warnings



# Suppress all warning messages
warnings.filterwarnings("ignore")

# Check if a LaTeX executable exists or not
if which('latex'):
    uselatex = True

# Enable LaTex text formatting
if uselatex:
    plt.rc('font', **{'family':'sans-serif','sans-serif':['Helvetica']})
    plt.rc('text', usetex=True)



# Display message to screen
print ('')
print ('**************************************************************************')
print ('*           T-Blade3 2D blade section plotting script                    *')
print ('*                                                                        *')
print ('* Default command: $python plot_2D_sections.py                           *')
print ('*                  This will plot all spanwise sections and save         *')
print ('*                  all plots in a single PDF (2D_sections.pdf)           *')
print ('*                                                                        *')
print ('* Secondary command: $python plot_2D_sections.py c1 c2 c3 ... cn         *')
print ('*                    The arguments ci should be integers                 *')
print ('*                    ci represent the indices of spanwise sections       *')
print ('*                                                                        *')
print ('* Example command: $python plot_2D_sections.py 5 16 21                   *')
print ('*                  Will plot only the 5th, 16th and 21st sections        *')
print ('**************************************************************************')
print ('')



# Determine the name of the main T-Blade3 input files
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

# Determine the bladerow number
bladerow = int(lines[3])



# If all sections need to be plotted
if len(sys.argv) == 1:

    # Determine number of spanwise sections
    nsections = int(lines[9])
    sections = np.linspace(1,nsections,nsections)
    sections = sections.astype(int)

# If only some sections need to be plotted
elif len(sys.argv) > 1:
    
    # Read and store indices of all sections to be plotted
    sections = np.zeros(len(sys.argv) - 1)
    for i in range(1, len(sys.argv)):
        sections[i - 1] = int(sys.argv[i])
    sections = sections.astype(int)



# Plot (m', \theta) sections
# Stored in the same PDF
with PdfPages('2D_sections.pdf') as pdf:
    for i_sec in sections:

        # Read section files
        filename = 'blade.' + str(i_sec) + '.' + str(bladerow) + '.' + casename
        f = open(filename, "r")
        lines = f.readlines()
        lines1 = lines[2:]     # Skip the first two lines
        f.close()

        # numpy arrays for storing blade section coordinates read from the file
        m_prime = np.zeros(len(lines1))
        theta = np.zeros(len(lines1))

        # Store the blade coordinates read from the file in the arrays defined above
        i = 0
        for line in lines1:
            m_prime[i] = float(line.split()[0])
            theta[i] = float(line.split()[1])
            i = i + 1
            
        # Plot the 2D blade section
        plt.figure()
        plt.plot(m_prime, theta, 'k')

        if uselatex:
            plt.xlabel(r"$m'$", fontsize=16)
            plt.ylabel(r'$\theta$', fontsize=16)
            plt.title(r"($m', \theta$) plot for section %d"%int(i_sec), fontsize=16)
        else:
            plt.xlabel("m'", fontsize=16)
            plt.ylabel("theta", fontsize=16)
            plt.title("(m', theta) plot for section %d"%int(i_sec), fontsize=16)

        plt.axis('equal')

        # Save PDF
        pdf.savefig()

plt.close()
