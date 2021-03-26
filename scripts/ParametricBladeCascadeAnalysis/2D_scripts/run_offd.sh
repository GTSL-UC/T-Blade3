
cp ises.YYYY_offd_inv ises.XXXX
/home/hamw/Documents/SCO2/Programs/Mises2.70/bin/ises XXXX << EOF > ises_inv_offd.log
15
0
EOF
cp ises.YYYY_offd_Re ises.XXXX
/home/hamw/Documents/SCO2/Programs/Mises2.70/bin/ises XXXX << EOF > ises_visc_offd.log
45
0
EOF
/home/hamw/Documents/SCO2/Programs/Mises2.70/bin/iprint XXXX  > iprint_offd.log
grep S2 iprint_offd.log | awk {'print $5'} > S2_out_offd.dat
grep Omega iprint_offd.log | awk {'print $4'} > omega_offd.dat
grep rms\(dR\) ises_visc_offd.log | tail -n 1 | awk {'print $1'} > cases_offd.dat
