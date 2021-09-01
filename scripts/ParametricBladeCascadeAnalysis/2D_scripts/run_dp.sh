TBLADE_PATH/tblade3 3dbgbinput.ZZZ.dat > tblade_dp.log
TBLADE_PATH/techop blade.XXXX 20 > techop_dp.log
ISES_PATH/iset_noplt XXXX << EOF > iset_dp.log
1

2

3
4
0
EOF
cp ises.YYYY_inv ises.XXXX
ISES_PATH/ises XXXX << EOF > ises_inv_dp.log
15
0
EOF
cp ises.YYYY_Re ises.XXXX
ISES_PATH/ises XXXX << EOF > ises_visc_dp.log
45
0
EOF
ISES_PATH/iprint XXXX  > iprint_dp.log
grep S2 iprint_dp.log | awk {'print $5'} > S2_out_dp.dat
grep Omega iprint_dp.log | awk {'print $4'} > omega_dp.dat
grep rms\(dR\) ises_visc_dp.log | tail -n 1 | awk {'print $1'} > cases_dp.dat
grep S1 iprint_dp.log | awk {'print $3'} > S1_dp.dat
grep S1 iprint_dp.log | awk {'print $5'} > S1.dat

sh run_offd.sh
