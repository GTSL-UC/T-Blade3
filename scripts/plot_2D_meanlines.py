# Script to plot (u, v) meanline second derivative
# control points along with meanline, meanline
# derivative and second derivative
# Written by Mayank Sharma (GTSL, University of Cincinnati)
from shutil import which
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import os
import sys



# Determine whether a LaTeX executable exists or not
# Enable LaTeX text formatting if it does
if which('latex'):
    uselatex = True
    plt.rc('font', **{'family':'sans-serif','sans-serif':['Helvetica']})
    plt.rc('text', usetex=True)



# Display message to screen
print('')
print ('**************************************************************************')
print ('*           T-Blade3 blade section meanline plotting script              *')
print ('*                                                                        *')
print ('* Default command: $python plot_2D_meanlines.py                          *')
print ('*                  This will plot non-dimensional meanline data for      *')
print ('*                  spanwise section and save all plots in a single       *')
print ('*                  PDF (2D_meanlines.pdf)                                *')
print ('*                                                                        *')
print ('* Secondary command: $python plot_2D_meanlines.py c1 c2 c3 ... cn        *')
print ('*                    The arguments ci should be integers                 *')
print ('*                    ci represent the indices of spanwise sections       *')
print ('*                                                                        *')
print ('* Example command: $python plot_2D_meanlines.py 5 16 21                  *')
print ('*                  Will plot meanline data for only the 5th, 16th and    *')
print ('*                  21st sections                                         *')
print ('**************************************************************************')
print('')



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

# Determine number of spanwise sections
nsections = int(lines[9])

# Determine if T-Blade has been run with dev or not
filename = 'curvature_data.1.' + casename
file_exists = os.path.exists(filename)
if not file_exists:
    error_message = "ERROR: Sectionwise meanline data files do not exist. Please run "\
                    "T-Blade3 with the 'dev' command line option."
    sys.exit(error_message)



# If all sections need to be plotted
if len(sys.argv) == 1:

    # Determine number of spanwise sections
    sections = np.linspace(1, nsections, nsections)
    sections = sections.astype(int)

# If only some sections need to be plotted
elif len(sys.argv) > 1:

    # Read and store indices of all sections to be plotted
    sections = np.zeros(len(sys.argv) - 1)
    for i in range(1, len(sys.argv)):
        sections[i - 1] = int(sys.argv[i])
    sections = sections.astype(int)



# Plot meanline data
# Stored in the same PDF
with PdfPages('2D_meanlines.pdf') as pdf:

    # Read spanwise control points of mean-line
    # second derivative
    filename = 'curvature_span_variation.' + casename + '.dat'
    cps = np.loadtxt(filename, unpack=True)
    ncp = (int) ((cps.shape[0] - 1)/2)
    ucp = cps[0:ncp, :]
    vcp = cps[ncp:2*ncp, :]
    k = cps[2*ncp, :]

    for i_sec in sections:

        # Read meanline data files
        filename = 'curvature_data.' + str(i_sec) + '.' + casename
        u, v, vd, vdd = np.loadtxt(filename, unpack=True)

        # Plot mean-line data
        plt.figure()

        if uselatex:
            plt.plot(u, v, 'r', linewidth=0.5, label=r"$v_{m}(u)$")
            plt.plot(u, vd, 'b', linewidth=0.5, label=r"$v'_{m}(u)$")
            plt.plot(u, vdd, 'k', linewidth=0.5, label=r"$v''_{m}(u)$")
            plt.plot(ucp[:, i_sec - 1], k[i_sec - 1] * vcp[:, i_sec - 1], 'k--o', linewidth=0.5,
                     markersize=2, alpha=0.8, label=r"$k \hat{v}''_{m}(u)$")
            plt.xlabel(r"$u$", fontsize=16)
            plt.ylabel(r"meanline data", fontsize=16)
            plt.title(r"Meanline plots for section %d"%int(i_sec), fontsize=16)
        else:
            plt.plot(u, v, 'r', linewidth=0.5, label="meanline")
            plt.plot(u, vd, 'b', linewidth=0.5, label="derivative")
            plt.plot(u, vdd, 'k', linewidth=0.5, label="2nd derivative")
            plt.plot(ucp[:, i_sec - 1], k[i_sec - 1] * vcp[:, i_sec - 1], 'k--o', linewidth=0.5,
                     markersize=2, alpha=0.8, label="scaled CPs")
            plt.xlabel("u", fontsize=16)
            plt.ylabel("meanline data", fontsize=16)
            plt.title("Meanline plots for section %d"%int(i_sec), fontsize=16)

        plt.xlim([0, 1])

        # Save PDF
        art = []
        lgd = plt.legend(loc='center left', bbox_to_anchor=(1, 0.5), edgecolor='black')
        art.append(lgd)
        pdf.savefig(bbox_extra_artists=art, bbox_inches='tight')

        # Close plt.figure
        plt.close()
