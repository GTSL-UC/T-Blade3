from root import ParametricBladeCascadeAnalysis as PBCA
ises_path = '/home/hamw/Document_0c/Programs/Mises2.70/bin'
tblade_path = '/home/hamw/Document_0c/Programs/T-Blade3/bin'
simulation1 = PBCA([0.50],['3_blade'],[3454477.23287],ises_path,tblade_path,7,1)
# simulation1 = PBCA([0.75, 0.25, 0.50],['4_blade'],[6923667.25756],7,0)
# simulation1 = PBCA([1.00, 0.00, 0.75, 0.25, 0.50],['4_blade','5_blade'],[4435945.47, 4390566.25],7,1)
# simulation1 = PBCA([1.00, 0.00, 0.75, 0.25, 0.50],['5_blade'],[3494391.68])
simulation1.optimizer()
