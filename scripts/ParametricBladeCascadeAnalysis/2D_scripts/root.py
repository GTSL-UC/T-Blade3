import os
import subprocess
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import openmdao.api as om
from openmdao.utils.file_wrap import InputFileGenerator
writeParser = InputFileGenerator()


class ParametricBladeCascadeAnalysis:
    "Parametric Blade Analysis Machine using T-Blade3, Mises, OpenMDAO, and Python3"
    "Optimization, Sweep, and DOE capabilities"
    "Developed and Designed by Matthew Ha"
    
    
    def __init__(self,spans,blades,RE,MISES_path,Tblade3_path,offd_deg=7,step_up=0):
            
            self.spans = spans
            self.blades = blades
            self.RE = RE
            self.offd_deg = offd_deg
            self.step_up = step_up
            
            os.chdir('..')
            self.path = os.getcwd() + '/'
            self.path_ises = self.path + 'inputs/axisym_ises_input/'
            self.path_bin = self.path + '2D_scripts/'
            self.path_tblade = self.path + 'inputs/tblade3_input/'
            self.path_blades = self.path + 'CFD/'
            self.path_results = self.path + 'results/'
            self.path_ISES = MISES_path
            self.path_tblade3 = Tblade3_path
            
            ## TBLADE3 CLEANUP
            os.chdir(self.path_tblade)
        
            ## COUNTING NUMBER OF ROWS TO OPTIMIZE
            _rows = sorted(os.listdir())
            _row = []
            for i in _rows:
                i = i.split('.')
                _row.append(i[1])
            self.row = sorted(list(set(_row)))
        
            ## SPAN VALUES FOR OPTIMIZATION ASSIGNMENT
            span_loc_T = [0.00,0.25,0.50,0.75,1.00]
            span_loc_ises_T = [1,2,3,4,5]
            span_loc_tblade_T = [1,6,11,16,21]
            span_loc=[]
            span_loc_ises=[]
            span_loc_tblade=[]
        
            for i in range(len(spans)):
                span = spans[i]
                if span == float(span_loc_T[0]):
                    span_loc.append(span)
                    span_loc_ises.append(span_loc_ises_T[0])
                    span_loc_tblade.append(span_loc_tblade_T[0])
                elif span == float(span_loc_T[1]):
                    span_loc.append(span)
                    span_loc_ises.append(span_loc_ises_T[1])
                    span_loc_tblade.append(span_loc_tblade_T[1])
                elif span == float(span_loc_T[2]):
                    span_loc.append(span)
                    span_loc_ises.append(span_loc_ises_T[2])
                    span_loc_tblade.append(span_loc_tblade_T[2])
                elif span == float(span_loc_T[3]):
                    span_loc.append(span)
                    span_loc_ises.append(span_loc_ises_T[3])
                    span_loc_tblade.append(span_loc_tblade_T[3])
                elif span == float(span_loc_T[4]):
                    span_loc.append(span)
                    span_loc_ises.append(span_loc_ises_T[4])
                    span_loc_tblade.append(span_loc_tblade_T[4])
            self.span = [(span_loc[i], span_loc_ises[i], span_loc_tblade[i]) for i in range(0, len(span_loc))]

            ## REYNOLDS NUMBER CHECK
            if len(RE) != len(self.blades):
                print('Reynolds Numbers are improperly assigned.\nCheck that 1 Reynolds Number is assigned to each row')
                sys.exit()
            
            ## MAKEDIRECTORY FOR CFD
            os.chdir(self.path_blades)
            subprocess.run('rm -r *',shell=True)
            for i in blades:
                filename = str(i)
                os.mkdir(filename)
                
            ## MAKEDIRECTORY FOR TBLADE3 POST-OPTIMIZATION RESULTS
            os.chdir(self.path_results)
            subprocess.run('rm -r *',shell=True)
            for i in blades:
                filename = str(i)
                os.mkdir(filename)
                
            ## MAKEDIRECTORY FOR SPANWISE CFD WORKING FILE
            os.chdir(self.path_blades)
            rows = sorted(os.listdir())
            for row in rows:
                os.chdir(self.path_blades + row)
                for name in span_loc:
                    os.mkdir(str(name))
        
            ## GATHER CASE NAME
            case_name=[]
            os.chdir(self.path_tblade)
            list_dir = os.listdir()
            for file in list_dir:
                with open(file) as tblade:
                    for i, line in enumerate(tblade):
                        if i == 1:
                            case_name.append(line.split())
            case_name = list(set(case_name[0])) 
            if len(case_name) > 1:
                print('Case Name is defined inconsistently within T-Blade3 input files.')
                sys.exit(1)
            
            ## CHECK FOR MACH STEP-UP FLAG
            ct=0
            if step_up == 0:
                for x in blades:
                    row = x[0].split('_')
                    for y,z,v in self.span:

                        
                        ## SETUP OPTIMIZATION INITALIZATION W/O MACH STEP-UP
                        working_file = self.path_blades + str(x) +'/'+ str(y)
                        os.chdir(working_file)
                        
                        ## IMPORT MISES AND BASH SCRIPTS
                        ises_inv = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_inv'
                        ises_Re = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_Re'
                        subprocess.run('cp ' + self.path_bin + '*.sh .', shell=True)
                        subprocess.run('cp ' + self.path_ises +'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +' ./'+ ises_inv, shell=True)
        
                        ## MAKE VISCOUS ISES FILE USING USER INPUT REYNOLDS NUMBER
                        keyword = {"0.0000e6":str(RE[ct])}
                        with open(ises_inv,'r') as inv, open(ises_Re,'w+') as Re:
                            for line in inv:
                                for word, replacement in keyword.items():
                                    Re.write(line.replace(word,replacement))
        
                                ## OFF DESIGN POINT INITALIZATION
                                tblade_fname = '3dbgbinput.' +str(row[0])+ '.dat'
                                with open(self.path_tblade+ '/' +tblade_fname,'r') as tbld3:
                                    for i, line in enumerate(tbld3):
                                        if i == 27 + int(v):
                                            line = line.split()
                                            beta_in = np.radians(float(line[1]))
                                
                                S1_dp = str(np.tan(beta_in))
                                S1 = float(S1_dp)

                                with open(ises_inv,'r') as inv:
                                    for line in inv:
                                        if 'S1' in line:
                                            line = line.split()
                                            S1_dp_ises = line[2]
                                            
                                if beta_in > 0:
                                    beta_in_offd = beta_in + np.radians(offd_deg)
                                elif beta_in < 0:
                                    beta_in_offd = beta_in - np.radians(offd_deg)
                
                                S1_offd = np.tan(beta_in_offd)
                                S1_offd = str(S1_offd)
                                dp_inv = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_inv'
                                dp_Re = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_Re'


                                with open(ises_inv, 'r') as file :
                                    filedata = file.read()
                                filedata = filedata.replace(S1_dp_ises, S1_dp)
                                with open(dp_inv, 'w') as file:
                                    file.write(filedata)
                                with open(ises_Re, 'r') as file :
                                    filedata = file.read()
                                filedata = filedata.replace(S1_dp_ises, S1_dp)
                                with open(dp_Re, 'w') as file:
                                    file.write(filedata)

                                S1_offd = np.tan(beta_in_offd)
                                S1_offd = str(S1_offd)
                                offd_inv = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_offd_inv'
                                offd_Re = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_offd_Re'
                
                                with open(ises_inv, 'r') as file :
                                    filedata = file.read()
                                filedata = filedata.replace(S1_dp, S1_offd)
                                with open(offd_inv, 'w') as file:
                                    file.write(filedata)
                                with open(ises_Re, 'r') as file :
                                    filedata = file.read()
                                filedata = filedata.replace(S1_dp, S1_offd)
                                with open(offd_Re, 'w') as file:
                                    file.write(filedata)
        
                        ## SETUP LINUX BASH.SH FILES FILE
                        tblade_name = str(v) + '.' + str(row[0]) + '.' + str(case_name[0])
                        blade_name = str(z) + '.' + str(row[0]) + '.' + str(case_name[0])
                        sh_dp = 'run_dp.sh'
                        sh_offd = 'run_offd.sh'
                        
                        with open(sh_dp,'r') as file:
                            filedata = file.read()
                        filedata = filedata.replace('XXXX',tblade_name)
                        filedata = filedata.replace('YYYY',blade_name)
                        filedata = filedata.replace('ZZZ',str(row[0]))
                        filedata = filedata.replace('ISES_PATH', self.path_ISES)
                        filedata = filedata.replace('TBLADE_PATH', self.path_tblade3)
                        with open(sh_dp, 'w') as file:
                            file.write(filedata)
                                
                        with open(sh_offd,'r') as file:
                            filedata = file.read()
                        filedata = filedata.replace('XXXX',tblade_name)
                        filedata = filedata.replace('YYYY',blade_name)
                        filedata = filedata.replace('ISES_PATH', self.path_ISES)
                        filedata = filedata.replace('TBLADE_PATH', self.path_tblade3)
                        with open(sh_offd, 'w') as file:
                            file.write(filedata)
                                
                        with open('run.sh','r') as file:
                            filedata = file.read()
                        filedata = filedata.replace('XXXX',tblade_name)
                        filedata = filedata.replace('YYYY',blade_name)
                        filedata = filedata.replace('ZZZ',str(row[0]))
                        filedata = filedata.replace('ISES_PATH', self.path_ISES)
                        filedata = filedata.replace('TBLADE_PATH', self.path_tblade3)
                        with open('run.sh','w') as file:
                            file.write(filedata)
                    ct+=1
                    
            elif step_up == 1:
                for x in blades:
                        row = x[0].split('_')
                        for y,z,v in self.span:
                            self.machstep =[]
                            with open(self.path_ises +'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0])) as ises_file:
                                for i, line in enumerate(ises_file):
                                    if i == 2:
                                        line = line.split()
                                        self.tg_mach = line[0]
                                        
                                        start=float(self.tg_mach)
    
                                        small_step = start - 0.05
                                        large_step = small_step - 0.1
                                        
                                        
                                        machstep1 = (np.linspace(small_step,start,num=6))
                                        machstep2 = (np.linspace(large_step,small_step-0.02,num=3))
    
                                        self.machstep.append(np.round(np.concatenate([machstep2,machstep1]),5))
                                    
                            os.chdir(self.path_blades +'/'+ str(x) +'/'+ str(y))
                            for p in self.machstep[0]:
                                os.chdir(self.path_blades + str(x) +'/'+ str(y))
                                os.mkdir(str(p))

                            ## SETUP OPTIMIZATION INITALIZATION W/O MACH STEP-UP
                                working_file = self.path_blades + str(x) +'/'+ str(y) + '/' + str(p)
                                os.chdir(working_file)
                                
                                ## IMPORT MISES AND BASH SCRIPTS
                                ises_inv = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_inv'
                                ises_Re = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_Re'
                                subprocess.run('cp ' + self.path_bin + '*.sh .', shell=True)
                                subprocess.run('cp ' + self.path_ises +'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +' ./'+ ises_inv, shell=True)
                                
                                with open(ises_inv, 'r') as file :
                                    filedata = file.read()
                                filedata = filedata.replace(self.tg_mach, str(p))
                                with open(ises_inv, 'w') as file:
                                    file.write(filedata)
                
                                ## MAKE VISCOUS ISES FILE USING USER INPUT REYNOLDS NUMBER
                                keyword = {"0.0000e6":str(RE[ct])}
                                with open(ises_inv,'r') as inv, open(ises_Re,'w+') as Re:
                                    for line in inv:
                                        for word, replacement in keyword.items():
                                            Re.write(line.replace(word,replacement))
                                            
                                ## OFF DESIGN POINT INITALIZATION
                                tblade_fname = '3dbgbinput.' +str(row[0])+ '.dat'
                                with open(self.path_tblade+ '/' +tblade_fname,'r') as tbld3:
                                    for i, line in enumerate(tbld3):
                                        if i == 27 + int(v):
                                            line = line.split()
                                            beta_in = np.radians(float(line[1]))
                                
                                S1_dp = str(np.tan(beta_in))
                                S1 = float(S1_dp)

                                with open(ises_inv,'r') as inv:
                                    for line in inv:
                                        if 'S1' in line:
                                            line = line.split()
                                            S1_dp_ises = line[2]
                                            
                                if beta_in > 0:
                                    beta_in_offd = beta_in + np.radians(offd_deg)
                                elif beta_in < 0:
                                    beta_in_offd = beta_in - np.radians(offd_deg)
                
                                S1_offd = np.tan(beta_in_offd)
                                S1_offd = str(S1_offd)
                                dp_inv = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_inv'
                                dp_Re = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_Re'


                                with open(ises_inv, 'r') as file :
                                    filedata = file.read()
                                filedata = filedata.replace(S1_dp_ises, S1_dp)
                                with open(dp_inv, 'w') as file:
                                    file.write(filedata)
                                with open(ises_Re, 'r') as file :
                                    filedata = file.read()
                                filedata = filedata.replace(S1_dp_ises, S1_dp)
                                with open(dp_Re, 'w') as file:
                                    file.write(filedata)

                                S1_offd = np.tan(beta_in_offd)
                                S1_offd = str(S1_offd)
                                offd_inv = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_offd_inv'
                                offd_Re = 'ises.'+ str(z) +'.'+ str(row[0]) +'.'+ str(case_name[0]) +'_offd_Re'
                
                                with open(ises_inv, 'r') as file :
                                    filedata = file.read()
                                filedata = filedata.replace(S1_dp, S1_offd)
                                with open(offd_inv, 'w') as file:
                                    file.write(filedata)
                                with open(ises_Re, 'r') as file :
                                    filedata = file.read()
                                filedata = filedata.replace(S1_dp, S1_offd)
                                with open(offd_Re, 'w') as file:
                                    file.write(filedata)

                                ## SETUP LINUX BASH.SH FILES FILE
                                tblade_name = str(v) + '.' + str(row[0]) + '.' + str(case_name[0])
                                blade_name = str(z) + '.' + str(row[0]) + '.' + str(case_name[0])
                                sh_dp = 'run_dp.sh'
                                sh_offd = 'run_offd.sh'
                                
                                with open(sh_dp,'r') as file:
                                    filedata = file.read()
                                filedata = filedata.replace('XXXX',tblade_name)
                                filedata = filedata.replace('YYYY',blade_name)
                                filedata = filedata.replace('ZZZ',str(row[0]))
                                filedata = filedata.replace('ISES_PATH', self.path_ISES)
                                filedata = filedata.replace('TBLADE_PATH', self.path_tblade3)
                                with open(sh_dp, 'w') as file:
                                    file.write(filedata)
                                        
                                with open(sh_offd,'r') as file:
                                    filedata = file.read()
                                filedata = filedata.replace('XXXX',tblade_name)
                                filedata = filedata.replace('YYYY',blade_name)
                                filedata = filedata.replace('ISES_PATH', self.path_ISES)
                                filedata = filedata.replace('TBLADE_PATH', self.path_tblade3)
                                with open(sh_offd, 'w') as file:
                                    file.write(filedata)
                                        
                                with open('run.sh','r') as file:
                                    filedata = file.read()
                                filedata = filedata.replace('XXXX',tblade_name)
                                filedata = filedata.replace('YYYY',blade_name)
                                filedata = filedata.replace('ZZZ',str(row[0]))
                                filedata = filedata.replace('ISES_PATH', self.path_ISES)
                                filedata = filedata.replace('TBLADE_PATH', self.path_tblade3)
                                with open('run.sh','w') as file:
                                    file.write(filedata)
                        ct+=1
          
    def optimizer(self):
        "Optiomization Tool: T-Blade3, Mises2.70, OpenMDAO"
        "Designed by Matthew Ha - GTSL | University of Cincinnati"
        
        def init_optimization(tblade3_fname,spancont_fname,z):
            self.data = np.ones(19)
            span1 = z
            with open(tblade3_fname) as tblade:
                for i, line in enumerate(tblade):
                    if i == 117 + span1:
                        line = line.split()
                        self.data[6] = line[1]
                        
                    if i == 126 + span1:
                        line = line.split()
                        self.data[7] = line[1]
                        
                    if i == 135 + span1:
                        line = line.split()
                        self.data[15] = line[1]
                        
            with open(spancont_fname) as tblade:
                for i, line in enumerate(tblade):
                    if i == 6 + span1:
                        line = line.split()
                        self.data[8] = line[6]
                        self.data[9] = line[7]
                        self.data[10] = line[8]
                        self.data[11] = line[9]
                        self.data[12] = line[10]
                        self.data[13] = line[11]
                        self.data[14] = line[12]
                        
                    if i == 15 + span1:
                        line = line.split()
                        self.data[16] = line[1]
                        self.data[17] = line[5]
                        self.data[18] = line[2]
                        
                        
        def run_optimization(data,span,tblade_fnamebase, tblade_fname, spancont_fnamebase, spancont_fname,v):
            
            with open(tblade_fname) as temp:
                data = temp.readlines()
            hold = int(v) + 27
            span_line = data[hold]
            span_line = span_line.split()
            S2 = span_line[2]
            
            with open('S2_constraint.out', 'w') as out:
                out.write(S2)
                
            class BladeDesignOptimizerMDAO(om.ExternalCodeComp):
        
                def setup(self):
            
                    # Link inputs
                    self.add_input('in_beta_star')
                    self.add_input('out_beta_star')
                    self.add_input('c1')
                    self.add_input('c2')
                    self.add_input('c3')
                    self.add_input('c4')
                    self.add_input('c5')
                    self.add_input('c6')
                    self.add_input('c7')
                    self.add_input('le_r')
                    self.add_input('c_m')
                    self.add_input('dx_dy_norm')
                    self.add_input('u_max')
                    
                    
                    # Link outputs
                    self.add_output('omegabar_dp')
                    self.add_output('omegabar_offd')
                    self.add_output('f')
                    self.add_output('S2')
                    self.add_output('S1')
                    
                    # Setup filenames
                    self.tbladeinputfilebase = tblade_fnamebase
                    self.tbladeinputfile = tblade_fname
            
                    self.spancontrolfilebase = spancont_fnamebase
                    self.spancontrolfile = spancont_fname
            
                    self.S1file = 'S1.dat'
                    self.S2dpfile = 'S2_out_dp.dat'
                    self.S2offdfile = 'S2_out_offd.dat'
                    self.omegadpfile = 'omega_dp.dat'
                    self.omegaoffdfile = 'omega_offd.dat'
                    self.casesdpfile = 'cases_dp.dat'
                    self.casesoffdfile = 'cases_offd.dat'
                    
                    # Define external files
                    self.options['external_input_files'] = [self.tbladeinputfile, self.spancontrolfile]
                    self.options['external_output_files'] = [self.S1file, self.S2dpfile, self.S2offdfile, self.omegadpfile, self.omegaoffdfile, self.casesdpfile, self.casesoffdfile]
                    
                    # Run bash scripts
                    self.options['command'] = ["bash", "run_dp.sh"]
                    
                    # Declare Partials
                    self.declare_partials(of='*', wrt='in_beta_star', method='fd', step=0.1, step_calc='abs')
                    self.declare_partials(of='*', wrt='out_beta_star', method='fd', step=0.01, step_calc='rel')
                    self.declare_partials(of='*', wrt='c1', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='c2', method='fd', step=0.01, step_calc='abs')
                    # self.declare_partials(of='*', wrt='c3', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='c4', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='c5', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='c6', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='c7', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='le_r', method='fd', step=0.1, step_calc='abs')
                    self.declare_partials(of='*', wrt='c_m', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='dx_dy_norm', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='u_max', method='fd', step=0.01, step_calc='abs')
                    
                def compute(self ,inputs , outputs):
                    
                    in_beta_star = inputs['in_beta_star']
                    out_beta_star = inputs['out_beta_star']
                    c1  = inputs['c1']
                    c2  = inputs['c2']
                    c3  = inputs['c3']
                    c4  = inputs['c4']
                    c5  = inputs['c5']
                    c6  = inputs['c6']
                    c7  = inputs['c7']
                    le_r = inputs['le_r']
                    c_m = inputs['c_m']
                    dx_dy_norm = inputs['dx_dy_norm']
                    u_max = inputs['u_max']
                    
                    # Span control file control 
                    writeParser.set_template_file(self.spancontrolfilebase)
                    writeParser.set_generated_file(self.spancontrolfile)
                    writeParser.reset_anchor()
                    
                    writeParser.mark_anchor("Span control points, Chord & thickness control points")
                    writeParser.transfer_var(le_r[0], 2 + span, 2)
                    writeParser.transfer_var(dx_dy_norm[0], 2 + span, 6)
                    writeParser.transfer_var(u_max[0], 2 + span, 3)
                    writeParser.generate()
                    
                    writeParser.reset_anchor()
                    writeParser.mark_anchor(" Span control points, Chord & curvature control points, number span, number of curv values")
                    writeParser.transfer_var(c1[0], 2 + span, 7)
                    writeParser.transfer_var(c2[0], 2 + span, 8)
                    # writeParser.transfer_var(c3[0], 2 + span, 9)
                    writeParser.transfer_var(c4[0], 2 + span, 10)
                    writeParser.transfer_var(c5[0], 2 + span, 11)
                    writeParser.transfer_var(c6[0], 2 + span, 12)
                    writeParser.transfer_var(c7[0], 2 + span, 13)
                    writeParser.generate()
                    
                    
                    # TBlade3 control file control 
                    writeParser.set_template_file(self.tbladeinputfilebase)
                    writeParser.set_generated_file(self.tbladeinputfile)
                    
                    writeParser.reset_anchor()        
                    writeParser.mark_anchor(" Control points for chord_multiplier (1+this parameter is multiplied by chord):")
                    writeParser.transfer_var(c_m[0], 2 + span, 2)
                    writeParser.generate()
                    
                    writeParser.reset_anchor()
                    writeParser.mark_anchor("in_beta* (deg):")
                    writeParser.transfer_var(in_beta_star[0], 2 + span, 2)
                    writeParser.generate()
                    
                    writeParser.reset_anchor()
                    writeParser.mark_anchor("out_beta* (deg):")
                    writeParser.transfer_var(out_beta_star[0], 2 + span, 2)
                    writeParser.generate()
                    
                    # Execute
                    super(BladeDesignOptimizerMDAO, self).compute(inputs, outputs)
                    
                    # Results parsing
                    S1=99
                    if os.path.getsize(self.S1file) > 0:
                        with open(self.S1file, 'r') as output_file:
                            S1=output_file.read()
                            substring = "NaN"
                            if substring in S1:
                                S1 = 99
                            else:
                                S1 = float(S1)
                            output_file.close()
                    S2_dp=99
                    if os.path.getsize(self.S2dpfile) > 0:
                        with open(self.S2dpfile, 'r') as output_file:
                            S2_dp=output_file.read()
                            substring = "NaN"
                            substring2 = "deg."
                            if substring in S2_dp:
                                S2_dp = 99
                            elif substring2 in S2_dp:
                                S2_dp= 99
                            else:
                                S2_dp = float(S2_dp)
                            output_file.close()
                            
                    S2_offd=99
                    if os.path.getsize(self.S2offdfile) > 0:
                        with open(self.S2offdfile, 'r') as output_file:
                            S2_offd=output_file.read()
                            substring = "NaN"
                            substring2 = "deg."
                            if substring in S2_offd:
                                S2_offd = 99
                            elif substring2 in S2_offd:
                                S2_offd = 99
                            else:
                                S2_offd = float(S2_offd)
                            output_file.close()
                    
                    omegabar_dp=99
                    if os.path.getsize(self.omegadpfile) > 0:
                        with open(self.omegadpfile, 'r') as output_file:
                            omegabar_dp=output_file.read()
                            substring = ["NaN","=>"]
                            if substring[0] in omegabar_dp:
                                omegabar_dp = 99
                            elif substring[1] in omegabar_dp:
                                omegabar_dp = 99
                            else:
                                omegabar_dp = float(omegabar_dp)
                            if omegabar_dp < 0:
                                omegabar_dp = 99
                                
                            output_file.close()
                            
                    omegabar_offd=99
                    if os.path.getsize(self.omegaoffdfile) > 0:
                        with open(self.omegaoffdfile, 'r') as output_file:
                            omegabar_offd=output_file.read()
                            substring = ["NaN","=>"]
                            if substring[0]in omegabar_offd:
                                omegabar_offd = 99
                            elif substring[1] in omegabar_offd:
                                omegabar_offd = 99
                            else:
                                omegabar_offd = float(omegabar_offd)
                            if omegabar_offd < 0:
                                omegabar_offd = 99
                                
                            output_file.close()
                    
                    cases_dp=99
                    if os.path.getsize(self.casesdpfile) > 0:
                        with open(self.casesdpfile, 'r') as output_file:
                            cases_dp = float(output_file.read())
                            output_file.close()
                            
                    cases_offd=99
                    if os.path.getsize(self.casesoffdfile) > 0:
                        with open(self.casesoffdfile, 'r') as output_file:
                            cases_offd = float(output_file.read())
                            output_file.close()
                    
                    
                    o_function = (omegabar_dp)*0.4 + (omegabar_offd)*0.6  
                    # o_function = omegabar_dp
                    f = open("case_results.txt","a")
                    f.write("%6.9f\t%6.9f\t%6.9f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\n" % (o_function, omegabar_dp, omegabar_offd, S1, S2_dp, S2_offd, in_beta_star[0], out_beta_star[0], c1[0], c2[0], c3[0], c4[0], c5[0], c6[0], c7[0], c_m[0], le_r[0], dx_dy_norm[0], u_max[0], cases_dp, cases_offd))
    
            
                    outputs['omegabar_dp'] = omegabar_dp
                    outputs['omegabar_offd']=omegabar_offd
                    outputs['f']=o_function
                    outputs['S2']=S2_dp
                    
                    
            f= open("case_results.txt","w")
            f.write("f\tobardp\tobaroffd\tS1\tS2_dp\tS2_offd\tibeta*\tobeta*\tc1\tc2\tc3\tc4\tc5\tc6\tc7\tc_m\tle_r\tdxdy_norm\tumax\tcases_dp\tcases_offd\n")
            f.close()
            
            prob = om.Problem()
            model = prob.model
            indeps = prob.model.add_subsystem('indeps', om.IndepVarComp())
            indeps.add_output('in_beta_star', self.data[6])
            indeps.add_output('out_beta_star', self.data[7]) 
            indeps.add_output('c1', self.data[8])   
            indeps.add_output('c2', self.data[9])  
            indeps.add_output('c3', 0.5)
            indeps.add_output('c4', self.data[11])   
            indeps.add_output('c5', self.data[12])   
            indeps.add_output('c6', self.data[13]) 
            indeps.add_output('c7', self.data[14])
            indeps.add_output('le_r', self.data[16])
            indeps.add_output('c_m', self.data[15])
            indeps.add_output('dx_dy_norm', self.data[17])
            indeps.add_output('u_max', self.data[18])
            
            model.add_subsystem('runblade', BladeDesignOptimizerMDAO())
            
            prob.driver = om.ScipyOptimizeDriver()
            prob.driver.options['optimizer'] = 'SLSQP'
            
            prob.model.connect('indeps.in_beta_star', 'runblade.in_beta_star')
            prob.model.connect('indeps.out_beta_star', 'runblade.out_beta_star')
            prob.model.connect('indeps.c1', 'runblade.c1')
            prob.model.connect('indeps.c2', 'runblade.c2')
            prob.model.connect('indeps.c3', 'runblade.c3')
            prob.model.connect('indeps.c4', 'runblade.c4')
            prob.model.connect('indeps.c5', 'runblade.c5')
            prob.model.connect('indeps.c6', 'runblade.c6')
            prob.model.connect('indeps.c7', 'runblade.c7')
            prob.model.connect('indeps.le_r', 'runblade.le_r')
            prob.model.connect('indeps.c_m', 'runblade.c_m')
            prob.model.connect('indeps.dx_dy_norm', 'runblade.dx_dy_norm')
            prob.model.connect('indeps.u_max', 'runblade.u_max')
            
            prob.model.add_design_var('indeps.in_beta_star', lower=-5, upper=5, ref=10)
            prob.model.add_design_var('indeps.out_beta_star', lower=-15, upper=10, ref=10)
            prob.model.add_design_var('indeps.c1', lower=-1, upper=1)
            prob.model.add_design_var('indeps.c2', lower=-1, upper=1)
            # prob.model.add_design_var('indeps.c3', lower=-1, upper=1)
            prob.model.add_design_var('indeps.c4', lower=-1, upper=1)
            prob.model.add_design_var('indeps.c5', lower=-1, upper=1)
            prob.model.add_design_var('indeps.c6', lower=-1, upper=1)
            prob.model.add_design_var('indeps.c7', lower=-1, upper=1)
            prob.model.add_design_var('indeps.le_r', lower=4.0, upper=8.0)
            prob.model.add_design_var('indeps.c_m', lower=-0.5, upper=0.5)
            prob.model.add_design_var('indeps.dx_dy_norm', lower=1, upper=3.5)
            prob.model.add_design_var('indeps.u_max', lower=0, upper=0.6)
            
            prob.model.add_constraint('runblade.S2',equals=float(S2))
            
            prob.model.add_objective('runblade.f')
        
            prob.driver.options['tol'] = 1e-3
            prob.driver.options['disp'] = True
            prob.driver.options['maxiter'] = 10
                
            prob.setup()
            # prob.run_model()
            prob.run_driver()
            
    
        def postprocess_optimization(tblade_fname,spancont_fname,j):
            
            df = pd.read_csv('case_results.txt', sep="\t", engine='python')
            pd.set_option("display.max.columns", None)
            S1 = df["S1"]
            S2 = df["S2_dp"]
            
    ##        print(S1,S2)
    ##        beta_in1 = np.arctan(S1)
    ##        beta_out1 = np.arctan(S2)
    ##        print(beta_in1,type(beta_in1))
    ##        beta_in = beta_in1[1]
    ##        beta_out = beta_out1[1]
            # *************************************************** Currently using beta_in from S1. Should be beta_in*
            
    ##        if beta_in > 0: 
    ##            incidence = df["ibeta*"] + beta_in
    ##            deviation = df["obeta*"] + beta_out
    ##        elif beta_in < 0:
    ##            incidence = df["ibeta*"] - beta_in
    ##            deviation = df["obeta*"] - beta_out
            
        # ****************************************************************************
        # SENSITIVE TO INLET ANGLE SIGN
    ##        incidence = df["ibeta*"] - beta_in
    ##        deviation = df["obeta*"] - beta_out
    ##        df['inc'] = incidence
    ##        df['dev'] = deviation
            
    
            df.drop(df[df["obardp"] < 0].index, inplace = True)
            df.drop(df[df["obaroffd"] < 0].index, inplace = True)
            df.drop(df[df["obardp"] > 1].index, inplace = True)
            df.drop(df[df["obaroffd"] > 1].index, inplace = True)
    
            with PdfPages('Optimization_Results.pdf') as pdf:
                
                _ = plt.figure()
                _ = plt.clf()
                _ = df.plot.scatter(x="obardp",y="obaroffd")
                plt.savefig('obardp_vs_obaroffd.jpg')
                pdf.savefig()
                
                _ = plt.figure()
                _ = plt.clf()
                _ = df.plot.scatter(x="obardp",y="obaroffd")
                _ = plt.xlim([0, 0.1])
                _ = plt.ylim([0, 0.1])
                plt.savefig('obardp_vs_obaroffd_zoom.jpg')
                _ = pdf.savefig()
                
                _ = plt.figure()
                _ = plt.clf()
                df.plot(y=["c1", "c2", "c3", "c4", "c5", "c6", "c7"], use_index=True)
                _ = plt.ylim([-1, 1])
                pdf.savefig()
                
                _ = plt.figure()
                _ = plt.clf()
                df.plot(y=["ibeta*", "obeta*"], use_index=True)
                pdf.savefig()
                
                # _ = plt.figure()
                # _ = plt.clf()
                # df.plot(y=["S2_dp"], use_index=True)
                # pdf.savefig()
                
    ##            _ = plt.figure()
    ##            _ = plt.clf()
    ##            df.plot(y=["inc"], use_index=True)
    ##            pdf.savefig()
    ##        
    ##            _ = plt.figure()
    ##            _ = plt.clf()
    ##            df.plot(y=["dev"], use_index=True)
    ##            pdf.savefig()
                
                # _ = plt.figure()
                # _ = plt.clf()
                # df.plot(y=["le_r"], use_index=True)
                # pdf.savefig()
                
    ##            _ = plt.figure()
    ##            _ = plt.clf()
    ##            df.plot.scatter(x="S2_dp",y="inc")
    ##            pdf.savefig()
    ##            
    ##            _ = plt.figure()
    ##            _ = plt.clf()
    ##            df.plot.scatter(x="S2_dp",y="dev")
    ##            pdf.savefig()
                
                plt.close('all')
                
        # ****************************************************************************
        # S2 Filter - SENSITIVE TO INLET ANGLE SIGN
                
            # S2_pos = S2_const + 0.6
            # S2_neg = S2_const - 0.6
            df.to_csv('case_results.out')
    ##        df.drop(df[df["S2_dp"] > S2_pos].index, inplace = True)
    ##        df.drop(df[df["S2_dp"] < S2_neg].index, inplace = True)
            df.drop(df[df["c_m"] > 0.50].index, inplace = True)
    
            # df = df[df['S2_dp'] > S2_neg]
            # df = df[df['S2_dp'] < S2_pos]
            data = (df[df.f==df.f.min()])

            data = df.iloc[-1]
            data.to_csv('target_blade.out', index = True)
            in_beta_str = data[6]
            out_beta_str = data[7] 
            c_m = data[15]
            
            writeParser.set_template_file(tblade_fname)
            writeParser.set_generated_file(tblade_fname)
            
            writeParser.reset_anchor()        
            writeParser.mark_anchor('Control points for in_beta* (deg):' )
            writeParser.transfer_var(in_beta_str, 2 + j, 2)
            writeParser.generate()
            
            writeParser.reset_anchor()        
            writeParser.mark_anchor('Control points for out_beta* (deg):' )
            writeParser.transfer_var(out_beta_str, 2 + j, 2)
            writeParser.generate()
            
            writeParser.reset_anchor()        
            writeParser.mark_anchor('Control points for chord_multiplier (1+this parameter is multiplied by chord):' )
            writeParser.transfer_var(c_m, 2 + j, 2)
            writeParser.generate()
        
            le_r = data[16]
            dx_dy_norm = data[17]
            u_max = data[18]
            c1 = data[8]
            c2 = data[9]
            c4 = data[11]
            c5 = data[12]
            c6 = data[13]
            c7 = data[14]
            
            writeParser.set_template_file(spancont_fname)
            writeParser.set_generated_file(spancont_fname)
            
            writeParser.reset_anchor()
            writeParser.mark_anchor("Span control points, Chord & thickness control points")
            writeParser.transfer_var(le_r, 2 + j, 2)
            writeParser.transfer_var(dx_dy_norm, 2 + j, 6)
            writeParser.transfer_var(u_max, 2 + j, 3)
            writeParser.generate()
            
            writeParser.reset_anchor()
            writeParser.mark_anchor(" Span control points, Chord & curvature control points, number span, number of curv values")
            writeParser.transfer_var(c1, 2 + j, 7)
            writeParser.transfer_var(c2, 2 + j, 8)
            writeParser.transfer_var(c4, 2 + j, 10)
            writeParser.transfer_var(c5, 2 + j, 11)
            writeParser.transfer_var(c6, 2 + j, 12)
            writeParser.transfer_var(c7, 2 + j, 13)
            writeParser.generate()
            
            subprocess.run('sh run.sh', shell=True)
            
            subprocess.run('mv *.pdf ' + self.path_results, shell=True)
            
        if self.step_up == 0:
            for x in self.blades:
                row = x[0].split('_')
                for y,z,v in self.span:
                    working_file = self.path_blades + str(x) +'/'+ str(y)
                    os.chdir(working_file)
                    tblade_fname = '3dbgbinput.' +str(row[0])+ '.dat'
                    spancont_fname = 'spancontrolinputs.' +str(row[0])+ '.dat'
                    tblade_fnamebase = '3dbgbinput.' +str(row[0])+ '.base'
                    spancont_fnamebase = 'spancontrolinputs.' +str(row[0])+ '.base'

                    if y == self.span[0][0]:
                        subprocess.run('cp ' +self.path_tblade+ '/' +tblade_fname+ ' .', shell=True)
                        subprocess.run('cp ' +self.path_tblade+ '/' +spancont_fname+ ' .', shell=True)
                        subprocess.run('cp ' +self.path_tblade+ '/' +tblade_fname+ ' ./' +tblade_fnamebase, shell=True)
                        subprocess.run('cp ' +self.path_tblade+ '/' +spancont_fname+ ' ./' +spancont_fnamebase, shell=True)
                        
                    else:
                        subprocess.run('cp ' +old_working+ '/' +tblade_fname+ ' .', shell=True)
                        subprocess.run('cp ' +old_working+ '/' +spancont_fname+ ' .', shell=True)
                        subprocess.run('cp ' +old_working+ '/' +tblade_fname+ ' ./' +tblade_fnamebase, shell=True)
                        subprocess.run('cp ' +old_working+ '/' +spancont_fname+ ' ./' +spancont_fnamebase, shell=True)
    
                    init_optimization(tblade_fname,spancont_fname,z)
                    run_optimization(self.data,z,tblade_fnamebase, tblade_fname, spancont_fnamebase, spancont_fname,v)
                    postprocess_optimization(tblade_fname,spancont_fname,z)
                    old_working = working_file
                    
        elif self.step_up == 1:
            for x in self.blades:
                row = x[0].split('_')
                for y,z,v in self.span:
                    
                    os.chdir(self.path_blades + str(x) +'/'+ str(y))
                    machs = os.listdir()
                    for h in machs:
                    
                        working_file = self.path_blades + str(x) +'/'+ str(y) +'/'+ str(h)
                        os.chdir(working_file)
                        tblade_fname = '3dbgbinput.' +str(row[0])+ '.dat'
                        spancont_fname = 'spancontrolinputs.' +str(row[0])+ '.dat'
                        tblade_fnamebase = '3dbgbinput.' +str(row[0])+ '.base'
                        spancont_fnamebase = 'spancontrolinputs.' +str(row[0])+ '.base'
    
                        if y == self.span[0][0] and h == machs[0]:
                            subprocess.run('cp ' +self.path_tblade+ '/' +tblade_fname+ ' .', shell=True)
                            subprocess.run('cp ' +self.path_tblade+ '/' +spancont_fname+ ' .', shell=True)
                            subprocess.run('cp ' +self.path_tblade+ '/' +tblade_fname+ ' ./' +tblade_fnamebase, shell=True)
                            subprocess.run('cp ' +self.path_tblade+ '/' +spancont_fname+ ' ./' +spancont_fnamebase, shell=True)
                            
                        else:
                            subprocess.run('cp ' +old_working+ '/' +tblade_fname+ ' .', shell=True)
                            subprocess.run('cp ' +old_working+ '/' +spancont_fname+ ' .', shell=True)
                            subprocess.run('cp ' +old_working+ '/' +tblade_fname+ ' ./' +tblade_fnamebase, shell=True)
                            subprocess.run('cp ' +old_working+ '/' +spancont_fname+ ' ./' +spancont_fnamebase, shell=True)
        
                        init_optimization(tblade_fname,spancont_fname,z)
                        run_optimization(self.data,z,tblade_fnamebase, tblade_fname, spancont_fnamebase, spancont_fname,v)
                        postprocess_optimization(tblade_fname,spancont_fname,z)
                        old_working = working_file

    
    def sweep(self):
        "Sweep Tool: T-Blade3, Mises2.70"
        "Designed by Matthew Ha - GTSL | University of Cincinnati"

        
    def doe(self):
        "DOE Machine: T-Blade3, Mises2.70, OpenMDAO"
        "Designed by Matthew Ha - GTSL | University of Cincinnati"
        
        def init_doe(tblade3_fname,spancont_fname,z):
            self.data = np.ones(19)
            span1 = z
            with open(tblade3_fname) as tblade:
                for i, line in enumerate(tblade):
                    if i == 117 + span1:
                        line = line.split()
                        self.data[6] = line[1]
                        
                    if i == 126 + span1:
                        line = line.split()
                        self.data[7] = line[1]
                        
                    if i == 135 + span1:
                        line = line.split()
                        self.data[15] = line[1]
                        
            with open(spancont_fname) as tblade:
                for i, line in enumerate(tblade):
                    if i == 6 + span1:
                        line = line.split()
                        self.data[8] = line[6]
                        self.data[9] = line[7]
                        self.data[10] = line[8]
                        self.data[11] = line[9]
                        self.data[12] = line[10]
                        self.data[13] = line[11]
                        self.data[14] = line[12]
                        
                    if i == 15 + span1:
                        line = line.split()
                        self.data[16] = line[1]
                        self.data[17] = line[5]
                        self.data[18] = line[2]                  
                        
        def run_doe(data,span,tblade_fnamebase, tblade_fname, spancont_fnamebase, spancont_fname,v):
            
            with open(tblade_fname) as temp:
                data = temp.readlines()
            hold = int(v) + 27
            span_line = data[hold]
            span_line = span_line.split()
            S2 = span_line[2]
            
            with open('S2_constraint.out', 'w') as out:
                out.write(S2)
                
            class BladeDesignOptimizerMDAO(om.ExternalCodeComp):
        
                def setup(self):
            
                    # Link inputs
                    self.add_input('in_beta_star')
                    self.add_input('out_beta_star')
                    self.add_input('c1')
                    self.add_input('c2')
                    self.add_input('c3')
                    self.add_input('c4')
                    self.add_input('c5')
                    self.add_input('c6')
                    self.add_input('c7')
                    self.add_input('le_r')
                    self.add_input('c_m')
                    self.add_input('dx_dy_norm')
                    self.add_input('u_max')
                    
                    
                    # Link outputs
                    self.add_output('omegabar_dp')
                    self.add_output('omegabar_offd')
                    self.add_output('f')
                    self.add_output('S2')
                    self.add_output('S1')
                    
                    # Setup filenames
                    self.tbladeinputfilebase = tblade_fnamebase
                    self.tbladeinputfile = tblade_fname
            
                    self.spancontrolfilebase = spancont_fnamebase
                    self.spancontrolfile = spancont_fname
            
                    self.S1file = 'S1.dat'
                    self.S2dpfile = 'S2_out_dp.dat'
                    self.S2offdfile = 'S2_out_offd.dat'
                    self.omegadpfile = 'omega_dp.dat'
                    self.omegaoffdfile = 'omega_offd.dat'
                    self.casesdpfile = 'cases_dp.dat'
                    self.casesoffdfile = 'cases_offd.dat'
                    
                    # Define external files
                    self.options['external_input_files'] = [self.tbladeinputfile, self.spancontrolfile]
                    self.options['external_output_files'] = [self.S1file, self.S2dpfile, self.S2offdfile, self.omegadpfile, self.omegaoffdfile, self.casesdpfile, self.casesoffdfile]
                    
                    # Run bash scripts
                    self.options['command'] = ["bash", "run_dp.sh"]
                    
                    # Declare Partials
                    self.declare_partials(of='*', wrt='in_beta_star', method='fd', step=0.1, step_calc='abs')
                    self.declare_partials(of='*', wrt='out_beta_star', method='fd', step=0.01, step_calc='rel')
                    self.declare_partials(of='*', wrt='c1', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='c2', method='fd', step=0.01, step_calc='abs')
                    # self.declare_partials(of='*', wrt='c3', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='c4', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='c5', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='c6', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='c7', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='le_r', method='fd', step=0.1, step_calc='abs')
                    self.declare_partials(of='*', wrt='c_m', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='dx_dy_norm', method='fd', step=0.01, step_calc='abs')
                    self.declare_partials(of='*', wrt='u_max', method='fd', step=0.01, step_calc='abs')
                    
                def compute(self ,inputs , outputs):
                    
                    in_beta_star = inputs['in_beta_star']
                    out_beta_star = inputs['out_beta_star']
                    c1  = inputs['c1']
                    c2  = inputs['c2']
                    c3  = inputs['c3']
                    c4  = inputs['c4']
                    c5  = inputs['c5']
                    c6  = inputs['c6']
                    c7  = inputs['c7']
                    le_r = inputs['le_r']
                    c_m = inputs['c_m']
                    dx_dy_norm = inputs['dx_dy_norm']
                    u_max = inputs['u_max']
                    
                    # Span control file control 
                    writeParser.set_template_file(self.spancontrolfilebase)
                    writeParser.set_generated_file(self.spancontrolfile)
                    writeParser.reset_anchor()
                    
                    writeParser.mark_anchor("Span control points, Chord & thickness control points")
                    writeParser.transfer_var(le_r[0], 2 + span, 2)
                    writeParser.transfer_var(dx_dy_norm[0], 2 + span, 6)
                    writeParser.transfer_var(u_max[0], 2 + span, 3)
                    writeParser.generate()
                    
                    writeParser.reset_anchor()
                    writeParser.mark_anchor(" Span control points, Chord & curvature control points, number span, number of curv values")
                    writeParser.transfer_var(c1[0], 2 + span, 7)
                    writeParser.transfer_var(c2[0], 2 + span, 8)
                    # writeParser.transfer_var(c3[0], 2 + span, 9)
                    writeParser.transfer_var(c4[0], 2 + span, 10)
                    writeParser.transfer_var(c5[0], 2 + span, 11)
                    writeParser.transfer_var(c6[0], 2 + span, 12)
                    writeParser.transfer_var(c7[0], 2 + span, 13)
                    writeParser.generate()
                    
                    
                    # TBlade3 control file control 
                    writeParser.set_template_file(self.tbladeinputfilebase)
                    writeParser.set_generated_file(self.tbladeinputfile)
                    
                    writeParser.reset_anchor()        
                    writeParser.mark_anchor(" Control points for chord_multiplier (1+this parameter is multiplied by chord):")
                    writeParser.transfer_var(c_m[0], 2 + span, 2)
                    writeParser.generate()
                    
                    writeParser.reset_anchor()
                    writeParser.mark_anchor("in_beta* (deg):")
                    writeParser.transfer_var(in_beta_star[0], 2 + span, 2)
                    writeParser.generate()
                    
                    writeParser.reset_anchor()
                    writeParser.mark_anchor("out_beta* (deg):")
                    writeParser.transfer_var(out_beta_star[0], 2 + span, 2)
                    writeParser.generate()
                    
                    # Execute
                    super(BladeDesignOptimizerMDAO, self).compute(inputs, outputs)
                    
                    # Results parsing
                    S1=99
                    if os.path.getsize(self.S1file) > 0:
                        with open(self.S1file, 'r') as output_file:
                            S1=output_file.read()
                            substring = "NaN"
                            if substring in S1:
                                S1 = 99
                            else:
                                S1 = float(S1)
                            output_file.close()
                    S2_dp=99
                    if os.path.getsize(self.S2dpfile) > 0:
                        with open(self.S2dpfile, 'r') as output_file:
                            S2_dp=output_file.read()
                            substring = "NaN"
                            substring2 = "deg."
                            if substring in S2_dp:
                                S2_dp = 99
                            elif substring2 in S2_dp:
                                S2_dp= 99
                            else:
                                S2_dp = float(S2_dp)
                            output_file.close()
                            
                    S2_offd=99
                    if os.path.getsize(self.S2offdfile) > 0:
                        with open(self.S2offdfile, 'r') as output_file:
                            S2_offd=output_file.read()
                            substring = "NaN"
                            substring2 = "deg."
                            if substring in S2_offd:
                                S2_offd = 99
                            elif substring2 in S2_offd:
                                S2_offd = 99
                            else:
                                S2_offd = float(S2_offd)
                            output_file.close()
                    
                    omegabar_dp=99
                    if os.path.getsize(self.omegadpfile) > 0:
                        with open(self.omegadpfile, 'r') as output_file:
                            omegabar_dp=output_file.read()
                            substring = "NaN"
                            if substring in omegabar_dp:
                                omegabar_dp = 99
                            else:
                                omegabar_dp = float(omegabar_dp)
                            output_file.close()
                            
                    omegabar_offd=99
                    if os.path.getsize(self.omegaoffdfile) > 0:
                        with open(self.omegaoffdfile, 'r') as output_file:
                            omegabar_offd=output_file.read()
                            substring = ["NaN","=>"]
                            if substring[0]in omegabar_offd:
                                omegabar_offd = 99
                            elif substring[1] in omegabar_offd:
                                omegabar_offd = 99
                            else:
                                omegabar_offd = float(omegabar_offd)
                            if omegabar_offd < 0:
                                omegabar_offd = 99
                                
                            output_file.close()
                    
                    cases_dp=99
                    if os.path.getsize(self.casesdpfile) > 0:
                        with open(self.casesdpfile, 'r') as output_file:
                            cases_dp = float(output_file.read())
                            output_file.close()
                            
                    cases_offd=99
                    if os.path.getsize(self.casesoffdfile) > 0:
                        with open(self.casesoffdfile, 'r') as output_file:
                            cases_offd = float(output_file.read())
                            output_file.close()
                    
                    
                    o_function = (omegabar_dp)*0.4 + (omegabar_offd)*0.6  
                    # o_function = omegabar_dp
                    f = open("case_results.txt","a")
                    f.write("%6.9f\t%6.9f\t%6.9f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\n" % (o_function, omegabar_dp, omegabar_offd, S1, S2_dp, S2_offd, in_beta_star[0], out_beta_star[0], c1[0], c2[0], c3[0], c4[0], c5[0], c6[0], c7[0], c_m[0], le_r[0], dx_dy_norm[0], u_max[0], cases_dp, cases_offd))
    
            
                    outputs['omegabar_dp'] = omegabar_dp
                    outputs['omegabar_offd']=omegabar_offd
                    outputs['f']=o_function
                    outputs['S2']=S2_dp
                    
                    
            f= open("case_results.txt","w")
            f.write("f\tobardp\tobaroffd\tS1\tS2_dp\tS2_offd\tibeta*\tobeta*\tc1\tc2\tc3\tc4\tc5\tc6\tc7\tc_m\tle_r\tdxdy_norm\tumax\tcases_dp\tcases_offd\n")
            f.close()
            
            prob = om.Problem()
            model = prob.model
            indeps = prob.model.add_subsystem('indeps', om.IndepVarComp())
            indeps.add_output('in_beta_star', self.data[6])
            indeps.add_output('out_beta_star', self.data[7]) 
            indeps.add_output('c1', self.data[8])   
            indeps.add_output('c2', self.data[9])  
            indeps.add_output('c3', 0.5)
            indeps.add_output('c4', self.data[11])   
            indeps.add_output('c5', self.data[12])   
            indeps.add_output('c6', self.data[13]) 
            indeps.add_output('c7', self.data[14])
            indeps.add_output('le_r', self.data[16])
            indeps.add_output('c_m', self.data[15])
            indeps.add_output('dx_dy_norm', self.data[17])
            indeps.add_output('u_max', self.data[18])
            
            model.add_subsystem('runblade', BladeDesignOptimizerMDAO())
            
            prob.driver = om.DOEDriver(om.LatinHypercubeGenerator(samples=10000))
            # prob.driver = om.DOEDriver(om.FullFactorialGenerator(levels=4))
            prob.driver.options['run_parallel'] = True
            prob.driver.options['procs_per_model'] = 1
            
            prob.model.connect('indeps.in_beta_star', 'runblade.in_beta_star')
            prob.model.connect('indeps.out_beta_star', 'runblade.out_beta_star')
            prob.model.connect('indeps.c1', 'runblade.c1')
            prob.model.connect('indeps.c2', 'runblade.c2')
            prob.model.connect('indeps.c3', 'runblade.c3')
            prob.model.connect('indeps.c4', 'runblade.c4')
            prob.model.connect('indeps.c5', 'runblade.c5')
            prob.model.connect('indeps.c6', 'runblade.c6')
            prob.model.connect('indeps.c7', 'runblade.c7')
            prob.model.connect('indeps.le_r', 'runblade.le_r')
            prob.model.connect('indeps.c_m', 'runblade.c_m')
            prob.model.connect('indeps.dx_dy_norm', 'runblade.dx_dy_norm')
            prob.model.connect('indeps.u_max', 'runblade.u_max')
            
            prob.model.add_design_var('indeps.in_beta_star', lower=-5, upper=5, ref=10)
            prob.model.add_design_var('indeps.out_beta_star', lower=-15, upper=10, ref=10)
            prob.model.add_design_var('indeps.c1', lower=-1, upper=1)
            prob.model.add_design_var('indeps.c2', lower=-1, upper=1)
            # prob.model.add_design_var('indeps.c3', lower=-1, upper=1)
            prob.model.add_design_var('indeps.c4', lower=-1, upper=1)
            prob.model.add_design_var('indeps.c5', lower=-1, upper=1)
            prob.model.add_design_var('indeps.c6', lower=-1, upper=1)
            prob.model.add_design_var('indeps.c7', lower=-1, upper=1)
            prob.model.add_design_var('indeps.le_r', lower=4.0, upper=8.0)
            prob.model.add_design_var('indeps.c_m', lower=-0.5, upper=0.5)
            prob.model.add_design_var('indeps.dx_dy_norm', lower=1, upper=3.5)
            prob.model.add_design_var('indeps.u_max', lower=0, upper=0.6)
            
            
            prob.model.add_objective('runblade.f')
        

            prob.setup()
            # prob.run_model()
            prob.run_driver()
            
    
        def postprocess_doe(tblade_fname,spancont_fname,j):
            
            df = pd.read_csv('case_results.txt', sep="\t", engine='python')
            pd.set_option("display.max.columns", None)
            S1 = df["S1"]
            S2 = df["S2_dp"]
            
    ##        print(S1,S2)
    ##        beta_in1 = np.arctan(S1)
    ##        beta_out1 = np.arctan(S2)
    ##        print(beta_in1,type(beta_in1))
    ##        beta_in = beta_in1[1]
    ##        beta_out = beta_out1[1]
            # *************************************************** Currently using beta_in from S1. Should be beta_in*
            
    ##        if beta_in > 0: 
    ##            incidence = df["ibeta*"] + beta_in
    ##            deviation = df["obeta*"] + beta_out
    ##        elif beta_in < 0:
    ##            incidence = df["ibeta*"] - beta_in
    ##            deviation = df["obeta*"] - beta_out
            
        # ****************************************************************************
        # SENSITIVE TO INLET ANGLE SIGN
    ##        incidence = df["ibeta*"] - beta_in
    ##        deviation = df["obeta*"] - beta_out
    ##        df['inc'] = incidence
    ##        df['dev'] = deviation
            
    
            df.drop(df[df["obardp"] < 0].index, inplace = True)
            df.drop(df[df["obaroffd"] < 0].index, inplace = True)
            df.drop(df[df["obardp"] > 1].index, inplace = True)
            df.drop(df[df["obaroffd"] > 1].index, inplace = True)
    
            with PdfPages('Optimization_Results.pdf') as pdf:
                
                _ = plt.figure()
                _ = plt.clf()
                _ = df.plot.scatter(x="obardp",y="obaroffd")
                plt.savefig('obardp_vs_obaroffd.jpg')
                pdf.savefig()
                
                _ = plt.figure()
                _ = plt.clf()
                _ = df.plot.scatter(x="obardp",y="obaroffd")
                _ = plt.xlim([0, 0.1])
                _ = plt.ylim([0, 0.1])
                plt.savefig('obardp_vs_obaroffd_zoom.jpg')
                _ = pdf.savefig()
                
                _ = plt.figure()
                _ = plt.clf()
                df.plot(y=["c1", "c2", "c3", "c4", "c5", "c6", "c7"], use_index=True)
                _ = plt.ylim([-1, 1])
                pdf.savefig()
                
                _ = plt.figure()
                _ = plt.clf()
                df.plot(y=["ibeta*", "obeta*"], use_index=True)
                pdf.savefig()
                
                _ = plt.figure()
                _ = plt.clf()
                df.plot(y=["S2_dp"], use_index=True)
                pdf.savefig()
                
    ##            _ = plt.figure()
    ##            _ = plt.clf()
    ##            df.plot(y=["inc"], use_index=True)
    ##            pdf.savefig()
    ##        
    ##            _ = plt.figure()
    ##            _ = plt.clf()
    ##            df.plot(y=["dev"], use_index=True)
    ##            pdf.savefig()
                
                _ = plt.figure()
                _ = plt.clf()
                df.plot(y=["le_r"], use_index=True)
                pdf.savefig()
                
    ##            _ = plt.figure()
    ##            _ = plt.clf()
    ##            df.plot.scatter(x="S2_dp",y="inc")
    ##            pdf.savefig()
    ##            
    ##            _ = plt.figure()
    ##            _ = plt.clf()
    ##            df.plot.scatter(x="S2_dp",y="dev")
    ##            pdf.savefig()
                
                plt.close('all')
                
        # ****************************************************************************
        # S2 Filter - SENSITIVE TO INLET ANGLE SIGN
                
            # S2_pos = S2_const + 0.6
            # S2_neg = S2_const - 0.6
            df.to_csv('case_results.out')
    ##        df.drop(df[df["S2_dp"] > S2_pos].index, inplace = True)
    ##        df.drop(df[df["S2_dp"] < S2_neg].index, inplace = True)
            # df.drop(df[df["c_m"] > 0.50].index, inplace = True)
    
            # df = df[df['S2_dp'] > S2_neg]
            # df = df[df['S2_dp'] < S2_pos]

            
        if self.step_up == 0:
            for x in self.blades:
                row = x[0].split('_')
                for y,z,v in self.span:
                    working_file = self.path_blades + str(x) +'/'+ str(y)
                    os.chdir(working_file)
                    tblade_fname = '3dbgbinput.' +str(row[0])+ '.dat'
                    spancont_fname = 'spancontrolinputs.' +str(row[0])+ '.dat'
                    tblade_fnamebase = '3dbgbinput.' +str(row[0])+ '.base'
                    spancont_fnamebase = 'spancontrolinputs.' +str(row[0])+ '.base'

                    subprocess.run('cp ' +self.path_tblade+ '/' +tblade_fname+ ' .', shell=True)
                    subprocess.run('cp ' +self.path_tblade+ '/' +spancont_fname+ ' .', shell=True)
                    subprocess.run('cp ' +self.path_tblade+ '/' +tblade_fname+ ' ./' +tblade_fnamebase, shell=True)
                    subprocess.run('cp ' +self.path_tblade+ '/' +spancont_fname+ ' ./' +spancont_fnamebase, shell=True)
                        
    
                    init_doe(tblade_fname,spancont_fname,z)
                    run_doe(self.data,z,tblade_fnamebase, tblade_fname, spancont_fnamebase, spancont_fname,v)
                    postprocess_doe(tblade_fname,spancont_fname,z)
                    
                    old_working = working_file    
                    
        
        
    class CleanUp:
        "Clean-up function for Optimizer Tool. Does NOT remove result files"
        "Designed by Matthew Ha - GTSL | University of Cincinnati"
        
        
        
    class Reset:
        "Reset function for Optimizer Tool. Removes ALL files, resets optimization tool to defaults"
