#!/bin/bash




TBLADE_PATH/tblade3 3dbgbinput.ZZZ.dat > tblade_optimized.log
TBLADE_PATH/techop blade.XXXX 20 > techop_optimized.log
ISES_PATH/iset_noplt XXXX << EOF > iset_optimized.log
1

2

3
4
0
EOF
cp ises.YYYY_inv ises.XXXX
ISES_PATH/ises XXXX << EOF > ises_inv_optimized_dp.log
15
0
EOF
cp ises.YYYY_Re ises.XXXX
ISES_PATH/ises XXXX << EOF > ises_visc_optimized_dp.log
45
0
EOF
ISES_PATH/iprint XXXX  > iprint_optimized_dp.log
ISES_PATH/iplot XXXX << EOF > iplot_optimized_dp.log
1
1
14
3
14
4
14
5
14
6
14
10
14

3
3
M
0 1.2 150
8

Q
EOF
ps2pdf plot.ps XXXX_dp_iplot.pdf
grep S2 iprint_dp.log | awk {'print $5'} > S2_out_dp.dat
grep Omega iprint_dp.log | awk {'print $4'} > omega_dp.dat
grep rms\(dR\) ises_visc_dp.log | tail -n 1 | awk {'print $1'} > cases_dp.dat
grep S1 iprint_dp.log | awk {'print $3'} > S1_dp.dat
grep S1 iprint_dp.log | awk {'print $5'} > S1.dat

cp ises.YYYY_offd_inv ises.XXXX
ISES_PATH/ises XXXX << EOF > ises_inv_optimized_offd.log
15
0
EOF
cp ises.YYYY_offd_Re ises.XXXX
ISES_PATH/ises XXXX << EOF > ises_visc_optimized_offd.log
45
0
EOF
ISES_PATH/iprint XXXX  > iprint_optimized_offd.log
ISES_PATH/iplot XXXX << EOF > iplot_optimized_offd.log
1
1
14
3
14
4
14
5
14
6
14
10
14

3
3
M
0 1.2 150
8

Q
EOF
ps2pdf plot.ps XXXX_offd_iplot.pdf
grep S2 iprint_offd.log | awk {'print $5'} > S2_out_offd.dat
grep Omega iprint_offd.log | awk {'print $4'} > omega_offd.dat
grep rms\(dR\) ises_visc_offd.log | tail -n 1 | awk {'print $1'} > cases_offd.dat
