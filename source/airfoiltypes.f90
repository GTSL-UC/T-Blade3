!************************************************************
!*****Various types of airfoil definitions used in 3DBGB ****
!************************************************************
!The following airfoil types are included:
! circular
! s809m
! clarky
! NACA 4 digit series
! User input airfoil definition: User can bring in any type of shape in the given format.
! sect1 - The default airfoil type
!************************************************************


!************************************************************
!****** Circular airfoil for the hub of wind turbines *******
!************************************************************

subroutine circle(xb,yb,np)
!---for circular profile (x-a)^2 + y^2 = a^2 where a = 0.5 for non dimensional airfoil---
!---center = 0.5,0 and LE at 0,0. TE at 1,0.
! output:xb,yb and np no. of coordinates
implicit none

integer i,np,nx,uplmt
parameter(nx=500)
real x_ss(nx),y_ss(nx),x_ps(nx),y_ps(nx)
real xn(nx),yn(nx)
real, intent(out) :: xb(nx),yb(nx)

np = 2*np - 1
if(mod(np,2).eq.0)then
 np = np - 1 ! For even no. of points LE = np/2. So, the current formula (np+1)/2 ==> np/2 with this change.
endif
uplmt = (np+1)/2
  !np = 199
!--interpolating SS coordinates-----
  x_ss(1) = 1.00
  do i = 2,uplmt!100
   x_ss(i) = x_ss(1) - (i-1)*(x_ss(1)/(uplmt-1))
  enddo
  do i =1,uplmt!100
   y_ss(i) = sqrt(0.25 - (x_ss(i) - (0.5))**2)
  enddo 
!--interpolating PS coordinates-----
  x_ps(1) = 0.00
  do i = 2,uplmt!100
   x_ps(i) = x_ps(1) + (i-1)*(1.0/(uplmt-1))
  enddo
  do i =1, uplmt!100
   y_ps(i) = -sqrt(0.25 - (x_ps(i) - (0.5))**2)
  enddo
  !
  !np = 2*np-1
  do i = 1, uplmt-1!(((np+1)/2)-1)
   xn(i) = x_ss(i)
   yn(i) = y_ss(i)
  enddo
  !xn((np+1)/2) = 0.
  !yn((np+1)/2) = 0.
  xn(uplmt) = 0.
  yn(uplmt) = 0.
  do i = uplmt+1,np
   xn(i) = x_ps(i-uplmt+1)
   yn(i) = y_ps(i-uplmt+1)
  enddo
  do i=1,np
   xb(i) = xn(i)
   yb(i) = yn(i)
  enddo
return
end subroutine circle

!************************************************************
!******** Wind turbine airfoil definition s809 **************
!**** The TE is made blunt and hence it is called s809m *****
!************************************************************
subroutine s809m(xb,yb,np)
! s809m wind turbine airfoil profile
implicit none

integer i,np,nx
parameter(nx=500)
real x_s809(nx),y_s809(nx)
real, intent(out) :: xb(nx),yb(nx)

! x coordinates
  x_s809(1)     =   0.99500
  x_s809(2)     =   0.99200
  x_s809(3)     =   0.99000
  x_s809(4)     =   0.98500
  x_s809(5)     =   0.98000
  x_s809(6)     =   0.97000
  x_s809(7)     =   0.96000
  x_s809(8)     =   0.95000
  x_s809(9)     =   0.94000
  x_s809(10)    =   0.93000
  x_s809(11)    =   0.92000
  x_s809(12)    =   0.91000
  x_s809(13)    =   0.90000
  x_s809(14)    =   0.89000
  x_s809(15)    =   0.88000
  x_s809(16)    =   0.87000
  x_s809(17)    =   0.86000
  x_s809(18)    =   0.85000
  x_s809(19)    =   0.84000
  x_s809(20)    =   0.83000
  x_s809(21)    =   0.82000
  x_s809(22)    =   0.81000
  x_s809(23)    =   0.80000
  x_s809(24)    =   0.79000
  x_s809(25)    =   0.78000
  x_s809(26)    =   0.77000
  x_s809(27)    =   0.76000
  x_s809(28)    =   0.75000
  x_s809(29)    =   0.74000
  x_s809(30)    =   0.73000
  x_s809(31)    =   0.72000
  x_s809(32)    =   0.71000
  x_s809(33)    =   0.70000
  x_s809(34)    =   0.69000
  x_s809(35)    =   0.68000
  x_s809(36)    =   0.67000
  x_s809(37)    =   0.66000
  x_s809(38)    =   0.65000
  x_s809(39)    =   0.64000
  x_s809(40)    =   0.63000
  x_s809(41)    =   0.62000
  x_s809(42)    =   0.61000
  x_s809(43)    =   0.60000
  x_s809(44)    =   0.59000
  x_s809(45)    =   0.58000
  x_s809(46)    =   0.57000
  x_s809(47)    =   0.56000
  x_s809(48)    =   0.54000
  x_s809(49)    =   0.53000
  x_s809(50)    =   0.52000
  x_s809(51)    =   0.51000
  x_s809(52)    =   0.49000
  x_s809(53)    =   0.48000
  x_s809(54)    =   0.47000
  x_s809(55)    =   0.46000
  x_s809(56)    =   0.45000
  x_s809(57)    =   0.44000
  x_s809(58)    =   0.43000
  x_s809(59)    =   0.42000
  x_s809(60)    =   0.41000
  x_s809(61)    =   0.40000
  x_s809(62)    =   0.39000
  x_s809(63)    =   0.38000
  x_s809(64)    =   0.37000
  x_s809(65)    =   0.36000
  x_s809(66)    =   0.35000
  x_s809(67)    =   0.34000
  x_s809(68)    =   0.33000
  x_s809(69)    =   0.32000
  x_s809(70)    =   0.31000
  x_s809(71)    =   0.30000
  x_s809(72)    =   0.29000
  x_s809(73)    =   0.28000
  x_s809(74)    =   0.27000
  x_s809(75)    =   0.26000
  x_s809(76)    =   0.24000
  x_s809(77)    =   0.23000
  x_s809(78)    =   0.22000
  x_s809(79)    =   0.21000
  x_s809(80)    =   0.20000
  x_s809(81)    =   0.19000
  x_s809(82)    =   0.18000
  x_s809(83)    =   0.17000
  x_s809(84)    =   0.16000
  x_s809(85)    =   0.15000
  x_s809(86)    =   0.14000
  x_s809(87)    =   0.13000
  x_s809(88)    =   0.12000
  x_s809(89)    =   0.11000
  x_s809(90)    =   0.10000
  x_s809(91)    =   0.09000
  x_s809(92)    =   0.08000
  x_s809(93)    =   0.07000
  x_s809(94)    =   0.06000
  x_s809(95)    =   0.05000
  x_s809(96)    =   0.04000
  x_s809(97)    =   0.03000
  x_s809(98)    =   0.02000
  x_s809(99)    =   0.01000
  x_s809(100)   =   0.00000
  x_s809(101)   =   0.01000
  x_s809(102)   =   0.02000
  x_s809(103)   =   0.03000
  x_s809(104)   =   0.04000
  x_s809(105)   =   0.05000
  x_s809(106)   =   0.06000
  x_s809(107)   =   0.07000
  x_s809(108)   =   0.08000
  x_s809(109)   =   0.09000
  x_s809(110)   =   0.10000
  x_s809(111)   =   0.11000
  x_s809(112)   =   0.12000
  x_s809(113)   =   0.13000
  x_s809(114)   =   0.14000
  x_s809(115)   =   0.15000
  x_s809(116)   =   0.16000
  x_s809(117)   =   0.17000
  x_s809(118)   =   0.18000
  x_s809(119)   =   0.19000
  x_s809(120)   =   0.20000
  x_s809(121)   =   0.21000
  x_s809(122)   =   0.22000
  x_s809(123)   =   0.23000
  x_s809(124)   =   0.24000
  x_s809(125)   =   0.26000
  x_s809(126)   =   0.27000
  x_s809(127)   =   0.28000
  x_s809(128)   =   0.29000
  x_s809(129)   =   0.30000
  x_s809(130)   =   0.31000
  x_s809(131)   =   0.32000
  x_s809(132)   =   0.33000
  x_s809(133)   =   0.34000
  x_s809(134)   =   0.35000
  x_s809(135)   =   0.36000
  x_s809(136)   =   0.37000
  x_s809(137)   =   0.38000
  x_s809(138)   =   0.39000
  x_s809(139)   =   0.40000
  x_s809(140)   =   0.41000
  x_s809(141)   =   0.42000
  x_s809(142)   =   0.43000
  x_s809(143)   =   0.44000
  x_s809(144)   =   0.45000
  x_s809(145)   =   0.46000
  x_s809(146)   =   0.47000
  x_s809(147)   =   0.48000
  x_s809(148)   =   0.49000
  x_s809(149)   =   0.51000
  x_s809(150)   =   0.52000
  x_s809(151)   =   0.53000
  x_s809(152)   =   0.54000
  x_s809(153)   =   0.56000
  x_s809(154)   =   0.57000
  x_s809(155)   =   0.58000
  x_s809(156)   =   0.59000
  x_s809(157)   =   0.60000
  x_s809(158)   =   0.61000
  x_s809(159)   =   0.62000
  x_s809(160)   =   0.63000
  x_s809(161)   =   0.64000
  x_s809(162)   =   0.65000
  x_s809(163)   =   0.66000
  x_s809(164)   =   0.67000
  x_s809(165)   =   0.68000
  x_s809(166)   =   0.69000
  x_s809(167)   =   0.70000
  x_s809(168)   =   0.71000
  x_s809(169)   =   0.72000
  x_s809(170)   =   0.73000
  x_s809(171)   =   0.74000
  x_s809(172)   =   0.75000
  x_s809(173)   =   0.76000
  x_s809(174)   =   0.77000
  x_s809(175)   =   0.78000
  x_s809(176)   =   0.79000
  x_s809(177)   =   0.80000
  x_s809(178)   =   0.81000
  x_s809(179)   =   0.82000
  x_s809(180)   =   0.83000
  x_s809(181)   =   0.84000
  x_s809(182)   =   0.85000
  x_s809(183)   =   0.86000
  x_s809(184)   =   0.87000
  x_s809(185)   =   0.88000
  x_s809(186)   =   0.89000
  x_s809(187)   =   0.90000
  x_s809(188)   =   0.91000
  x_s809(189)   =   0.92000
  x_s809(190)   =   0.93000
  x_s809(191)   =   0.94000
  x_s809(192)   =   0.95000
  x_s809(193)   =   0.96000
  x_s809(194)   =   0.97000
  x_s809(195)   =   0.98000
  x_s809(196)   =   0.98500
  x_s809(197)   =   0.99000
  x_s809(198)   =   0.99200
  x_s809(199)   =   0.99500
!---------------------------------
! y coordinates------       
  y_s809(1)     =   0.00618
  y_s809(2)     =   0.00788
  y_s809(3)     =   0.00890
  y_s809(4)     =   0.01003
  y_s809(5)     =   0.01075
  y_s809(6)     =   0.01167
  y_s809(7)     =   0.01216
  y_s809(8)     =   0.01331
  y_s809(9)     =   0.01431
  y_s809(10)    =   0.01531
  y_s809(11)    =   0.01649
  y_s809(12)    =   0.01853
  y_s809(13)    =   0.02052
  y_s809(14)    =   0.02252
  y_s809(15)    =   0.02444
  y_s809(16)    =   0.02629
  y_s809(17)    =   0.02814
  y_s809(18)    =   0.02999
  y_s809(19)    =   0.03182
  y_s809(20)    =   0.03365
  y_s809(21)    =   0.03548
  y_s809(22)    =   0.03730
  y_s809(23)    =   0.03915
  y_s809(24)    =   0.04099
  y_s809(25)    =   0.04284
  y_s809(26)    =   0.04469
  y_s809(27)    =   0.04655
  y_s809(28)    =   0.04844
  y_s809(29)    =   0.05033
  y_s809(30)    =   0.05222
  y_s809(31)    =   0.05411
  y_s809(32)    =   0.05603
  y_s809(33)    =   0.05797
  y_s809(34)    =   0.05990
  y_s809(35)    =   0.06184
  y_s809(36)    =   0.06378
  y_s809(37)    =   0.06575
  y_s809(38)    =   0.06774
  y_s809(39)    =   0.06972
  y_s809(40)    =   0.07170
  y_s809(41)    =   0.07368
  y_s809(42)    =   0.07568
  y_s809(43)    =   0.07767
  y_s809(44)    =   0.07967
  y_s809(45)    =   0.08167
  y_s809(46)    =   0.08366
  y_s809(47)    =   0.08559
  y_s809(48)    =   0.08941
  y_s809(49)    =   0.09132
  y_s809(50)    =   0.09324
  y_s809(51)    =   0.09459
  y_s809(52)    =   0.09728
  y_s809(53)    =   0.09862
  y_s809(54)    =   0.09961
  y_s809(55)    =   0.10013
  y_s809(56)    =   0.10065
  y_s809(57)    =   0.10116
  y_s809(58)    =   0.10168
  y_s809(59)    =   0.10177
  y_s809(60)    =   0.10179
  y_s809(61)    =   0.10181
  y_s809(62)    =   0.10183
  y_s809(63)    =   0.10174
  y_s809(64)    =   0.10135
  y_s809(65)    =   0.10096
  y_s809(66)    =   0.10057
  y_s809(67)    =   0.10018
  y_s809(68)    =   0.09952
  y_s809(69)    =   0.09876
  y_s809(70)    =   0.09800
  y_s809(71)    =   0.09724
  y_s809(72)    =   0.09637
  y_s809(73)    =   0.09525
  y_s809(74)    =   0.09412
  y_s809(75)    =   0.09300
  y_s809(76)    =   0.09038
  y_s809(77)    =   0.08889
  y_s809(78)    =   0.08740
  y_s809(79)    =   0.08591
  y_s809(80)    =   0.08405
  y_s809(81)    =   0.08218
  y_s809(82)    =   0.08030
  y_s809(83)    =   0.07836
  y_s809(84)    =   0.07606
  y_s809(85)    =   0.07380
  y_s809(86)    =   0.07146
  y_s809(87)    =   0.06886
  y_s809(88)    =   0.06608
  y_s809(89)    =   0.06329
  y_s809(90)    =   0.06027
  y_s809(91)    =   0.05691
  y_s809(92)    =   0.05355
  y_s809(93)    =   0.04976
  y_s809(94)    =   0.04569
  y_s809(95)    =   0.04143
  y_s809(96)    =   0.03638
  y_s809(97)    =   0.03105
  y_s809(98)    =   0.02452
  y_s809(99)    =   0.01625
  y_s809(100)   =   -0.00002
  y_s809(101)   =   -0.01194
  y_s809(102)   =   -0.01833
  y_s809(103)   =   -0.02390
  y_s809(104)   =   -0.02907
  y_s809(105)   =   -0.03371
  y_s809(106)   =   -0.03819
  y_s809(107)   =   -0.04248
  y_s809(108)   =   -0.04651
  y_s809(109)   =   -0.05054
  y_s809(110)   =   -0.05431
  y_s809(111)   =   -0.05795
  y_s809(112)   =   -0.06159
  y_s809(113)   =   -0.06495
  y_s809(114)   =   -0.06813
  y_s809(115)   =   -0.07130
  y_s809(116)   =   -0.07437
  y_s809(117)   =   -0.07710
  y_s809(118)   =   -0.07982
  y_s809(119)   =   -0.08255
  y_s809(120)   =   -0.08502
  y_s809(121)   =   -0.08735
  y_s809(122)   =   -0.08967
  y_s809(123)   =   -0.09200
  y_s809(124)   =   -0.09397
  y_s809(125)   =   -0.09775
  y_s809(126)   =   -0.09963
  y_s809(127)   =   -0.10104
  y_s809(128)   =   -0.10237
  y_s809(129)   =   -0.10369
  y_s809(130)   =   -0.10502
  y_s809(131)   =   -0.10584
  y_s809(132)   =   -0.10654
  y_s809(133)   =   -0.10724
  y_s809(134)   =   -0.10794
  y_s809(135)   =   -0.10815
  y_s809(136)   =   -0.10811
  y_s809(137)   =   -0.10807
  y_s809(138)   =   -0.10803
  y_s809(139)   =   -0.10763
  y_s809(140)   =   -0.10683
  y_s809(141)   =   -0.10602
  y_s809(142)   =   -0.10522
  y_s809(143)   =   -0.10427
  y_s809(144)   =   -0.10262
  y_s809(145)   =   -0.10097
  y_s809(146)   =   -0.09931
  y_s809(147)   =   -0.09766
  y_s809(148)   =   -0.09545
  y_s809(149)   =   -0.09077
  y_s809(150)   =   -0.08843
  y_s809(151)   =   -0.08603
  y_s809(152)   =   -0.08342
  y_s809(153)   =   -0.07821
  y_s809(154)   =   -0.07560
  y_s809(155)   =   -0.07297
  y_s809(156)   =   -0.07029
  y_s809(157)   =   -0.06762
  y_s809(158)   =   -0.06495
  y_s809(159)   =   -0.06227
  y_s809(160)   =   -0.05963
  y_s809(161)   =   -0.05702
  y_s809(162)   =   -0.05441
  y_s809(163)   =   -0.05181
  y_s809(164)   =   -0.04920
  y_s809(165)   =   -0.04664
  y_s809(166)   =   -0.04420
  y_s809(167)   =   -0.04175
  y_s809(168)   =   -0.03931
  y_s809(169)   =   -0.03686
  y_s809(170)   =   -0.03448
  y_s809(171)   =   -0.03227
  y_s809(172)   =   -0.03006
  y_s809(173)   =   -0.02784
  y_s809(174)   =   -0.02563
  y_s809(175)   =   -0.02352
  y_s809(176)   =   -0.02159
  y_s809(177)   =   -0.01966
  y_s809(178)   =   -0.01773
  y_s809(179)   =   -0.01580
  y_s809(180)   =   -0.01408
  y_s809(181)   =   -0.01248
  y_s809(182)   =   -0.01087
  y_s809(183)   =   -0.00927
  y_s809(184)   =   -0.00778
  y_s809(185)   =   -0.00653
  y_s809(186)   =   -0.00528
  y_s809(187)   =   -0.00403
  y_s809(188)   =   -0.00296
  y_s809(189)   =   -0.00209
  y_s809(190)   =   -0.00122
  y_s809(191)   =   -0.00042
  y_s809(192)   =   0.00005
  y_s809(193)   =   0.00020
  y_s809(194)   =   0.00069
  y_s809(195)   =   0.00161
  y_s809(196)   =   0.00233
  y_s809(197)   =   0.00337
  y_s809(198)   =   0.00398
  y_s809(199)   =   0.00618
! --------------------------------
  np = 199
  do i=1,np
   xb(i) = x_s809(i)
   yb(i) = y_s809(i)
  enddo

return
end subroutine s809m

!*************************************************************
!***** Propeller airfoil definition called Clarky ************
!*************************************************************
subroutine clarky(xb,yb,np)
! Clarky propeller airfoil profile.
implicit none

integer i,np,nx
parameter(nx=500)
real x_clarky(nx),y_clarky(nx)
real, intent(out) :: xb(nx),yb(nx)

!----x coordinates--------------------
  x_clarky(1)   =   0.98552
  x_clarky(2)   =   0.98538
  x_clarky(3)   =   0.98495
  x_clarky(4)   =   0.98430
  x_clarky(5)   =   0.98349
  x_clarky(6)   =   0.98296
  x_clarky(7)   =   0.97250
  x_clarky(8)   =   0.96205
  x_clarky(9)   =   0.95159
  x_clarky(10)  =   0.94113
  x_clarky(11)  =   0.93067
  x_clarky(12)  =   0.92022
  x_clarky(13)  =   0.90976
  x_clarky(14)  =   0.89930
  x_clarky(15)  =   0.88885
  x_clarky(16)  =   0.87839
  x_clarky(17)  =   0.86793
  x_clarky(18)  =   0.85748
  x_clarky(19)  =   0.84702
  x_clarky(20)  =   0.83656
  x_clarky(21)  =   0.82610
  x_clarky(22)  =   0.81565
  x_clarky(23)  =   0.80519
  x_clarky(24)  =   0.79473
  x_clarky(25)  =   0.78428
  x_clarky(26)  =   0.77382
  x_clarky(27)  =   0.76336
  x_clarky(28)  =   0.75291
  x_clarky(29)  =   0.74245
  x_clarky(30)  =   0.73199
  x_clarky(31)  =   0.72153
  x_clarky(32)  =   0.71108
  x_clarky(33)  =   0.70062
  x_clarky(34)  =   0.69016
  x_clarky(35)  =   0.67971
  x_clarky(36)  =   0.66925
  x_clarky(37)  =   0.65879
  x_clarky(38)  =   0.64834
  x_clarky(39)  =   0.63788
  x_clarky(40)  =   0.62742
  x_clarky(41)  =   0.61696
  x_clarky(42)  =   0.60651
  x_clarky(43)  =   0.59605
  x_clarky(44)  =   0.58559
  x_clarky(45)  =   0.57514
  x_clarky(46)  =   0.56468
  x_clarky(47)  =   0.55422
  x_clarky(48)  =   0.54377
  x_clarky(49)  =   0.53331
  x_clarky(50)  =   0.52285
  x_clarky(51)  =   0.51239
  x_clarky(52)  =   0.50194
  x_clarky(53)  =   0.49148
  x_clarky(54)  =   0.48102
  x_clarky(55)  =   0.47057
  x_clarky(56)  =   0.46011
  x_clarky(57)  =   0.44965
  x_clarky(58)  =   0.43919
  x_clarky(59)  =   0.42874
  x_clarky(60)  =   0.41828
  x_clarky(61)  =   0.40782
  x_clarky(62)  =   0.39737
  x_clarky(63)  =   0.38691
  x_clarky(64)  =   0.37645
  x_clarky(65)  =   0.36600
  x_clarky(66)  =   0.35554
  x_clarky(67)  =   0.34508
  x_clarky(68)  =   0.33462
  x_clarky(69)  =   0.32417
  x_clarky(70)  =   0.31371
  x_clarky(71)  =   0.30325
  x_clarky(72)  =   0.29280
  x_clarky(73)  =   0.28234
  x_clarky(74)  =   0.27188
  x_clarky(75)  =   0.26143
  x_clarky(76)  =   0.25097
  x_clarky(77)  =   0.24051
  x_clarky(78)  =   0.23005
  x_clarky(79)  =   0.21960
  x_clarky(80)  =   0.20914
  x_clarky(81)  =   0.19868
  x_clarky(82)  =   0.18823
  x_clarky(83)  =   0.17777
  x_clarky(84)  =   0.16731
  x_clarky(85)  =   0.15686
  x_clarky(86)  =   0.14640
  x_clarky(87)  =   0.13594
  x_clarky(88)  =   0.12548
  x_clarky(89)  =   0.11503
  x_clarky(90)  =   0.10457
  x_clarky(91)  =   0.094113
  x_clarky(92)  =   0.083656
  x_clarky(93)  =   0.073199
  x_clarky(94)  =   0.062742
  x_clarky(95)  =   0.052285
  x_clarky(96)  =   0.041828
  x_clarky(97)  =   0.031371
  x_clarky(98)  =   0.020914
  x_clarky(99)  =   0.010457
  x_clarky(100) =   0.000000
  x_clarky(101) =   0.010347
  x_clarky(102) =   0.020694
  x_clarky(103) =   0.031041
  x_clarky(104) =   0.041388
  x_clarky(105) =   0.051735
  x_clarky(106) =   0.062082
  x_clarky(107) =   0.072429
  x_clarky(108) =   0.082776
  x_clarky(109) =   0.093123
  x_clarky(110) =   0.10347
  x_clarky(111) =   0.11382
  x_clarky(112) =   0.12416
  x_clarky(113) =   0.13451
  x_clarky(114) =   0.14486
  x_clarky(115) =   0.15520
  x_clarky(116) =   0.16555
  x_clarky(117) =   0.17590
  x_clarky(118) =   0.18625
  x_clarky(119) =   0.19659
  x_clarky(120) =   0.20694
  x_clarky(121) =   0.21729
  x_clarky(122) =   0.22763
  x_clarky(123) =   0.23798
  x_clarky(124) =   0.24833
  x_clarky(125) =   0.25867
  x_clarky(126) =   0.26902
  x_clarky(127) =   0.27937
  x_clarky(128) =   0.28971
  x_clarky(129) =   0.30006
  x_clarky(130) =   0.31041
  x_clarky(131) =   0.32076
  x_clarky(132) =   0.33110
  x_clarky(133) =   0.34145
  x_clarky(134) =   0.35180
  x_clarky(135) =   0.36214
  x_clarky(136) =   0.37249
  x_clarky(137) =   0.38284
  x_clarky(138) =   0.39318
  x_clarky(139) =   0.40353
  x_clarky(140) =   0.41388
  x_clarky(141) =   0.42422
  x_clarky(142) =   0.43457
  x_clarky(143) =   0.44492
  x_clarky(144) =   0.45527
  x_clarky(145) =   0.46561
  x_clarky(146) =   0.47596
  x_clarky(147) =   0.48631
  x_clarky(148) =   0.49655
  x_clarky(149) =   0.50700
  x_clarky(150) =   0.51735
  x_clarky(151) =   0.52769
  x_clarky(152) =   0.53804
  x_clarky(153) =   0.54839
  x_clarky(154) =   0.55874
  x_clarky(155) =   0.56908
  x_clarky(156) =   0.57943
  x_clarky(157) =   0.58978
  x_clarky(158) =   0.60012
  x_clarky(159) =   0.61047
  x_clarky(160) =   0.62082
  x_clarky(161) =   0.63116
  x_clarky(162) =   0.64151
  x_clarky(163) =   0.65186
  x_clarky(164) =   0.66220
  x_clarky(165) =   0.67255
  x_clarky(166) =   0.68290
  x_clarky(167) =   0.69325
  x_clarky(168) =   0.70359
  x_clarky(169) =   0.71394
  x_clarky(170) =   0.72429
  x_clarky(171) =   0.73463
  x_clarky(172) =   0.74498
  x_clarky(173) =   0.75533
  x_clarky(174) =   0.76567
  x_clarky(175) =   0.77602
  x_clarky(176) =   0.78637
  x_clarky(177) =   0.79671
  x_clarky(178) =   0.80706
  x_clarky(179) =   0.81741
  x_clarky(180) =   0.82776
  x_clarky(181) =   0.83810
  x_clarky(182) =   0.84845
  x_clarky(183) =   0.85880
  x_clarky(184) =   0.86914
  x_clarky(185) =   0.87949
  x_clarky(186) =   0.88984
  x_clarky(187) =   0.90018
  x_clarky(188) =   0.91053
  x_clarky(189) =   0.92088
  x_clarky(190) =   0.93123
  x_clarky(191) =   0.94157
  x_clarky(192) =   0.95192
  x_clarky(193) =   0.96227
  x_clarky(194) =   0.97261
  x_clarky(195) =   0.98296
  x_clarky(196) =   0.98395
  x_clarky(197) =   0.98478
  x_clarky(198) =   0.98533
  x_clarky(199) =   0.98552
!---------------------------------      
!   y coordinates  
  y_clarky(1)   =   0.001743
  y_clarky(2)   =   0.002617
  y_clarky(3)   =   0.003396
  y_clarky(4)   =   0.003995
  y_clarky(5)   =   0.004349
  y_clarky(6)   =   0.004480
  y_clarky(7)   =   0.0070868
  y_clarky(8)   =   0.0096936
  y_clarky(9)   =   0.012167
  y_clarky(10)  =   0.014640
  y_clarky(11)  =   0.017086
  y_clarky(12)  =   0.019444
  y_clarky(13)  =   0.021801
  y_clarky(14)  =   0.024159
  y_clarky(15)  =   0.026430
  y_clarky(16)  =   0.028673
  y_clarky(17)  =   0.030915
  y_clarky(18)  =   0.033158
  y_clarky(19)  =   0.035320
  y_clarky(20)  =   0.037434
  y_clarky(21)  =   0.039548
  y_clarky(22)  =   0.041662
  y_clarky(23)  =   0.043776
  y_clarky(24)  =   0.045746
  y_clarky(25)  =   0.047703
  y_clarky(26)  =   0.049661
  y_clarky(27)  =   0.051619
  y_clarky(28)  =   0.053576
  y_clarky(29)  =   0.055398
  y_clarky(30)  =   0.057168
  y_clarky(31)  =   0.058939
  y_clarky(32)  =   0.060709
  y_clarky(33)  =   0.062479
  y_clarky(34)  =   0.064224
  y_clarky(35)  =   0.065767
  y_clarky(36)  =   0.067310
  y_clarky(37)  =   0.068854
  y_clarky(38)  =   0.070397
  y_clarky(39)  =   0.071940
  y_clarky(40)  =   0.073434
  y_clarky(41)  =   0.074715
  y_clarky(42)  =   0.075996
  y_clarky(43)  =   0.077277
  y_clarky(44)  =   0.078559
  y_clarky(45)  =   0.079840
  y_clarky(46)  =   0.081105
  y_clarky(47)  =   0.082089
  y_clarky(48)  =   0.083072
  y_clarky(49)  =   0.084056
  y_clarky(50)  =   0.085040
  y_clarky(51)  =   0.086024
  y_clarky(52)  =   0.087008
  y_clarky(53)  =   0.087724
  y_clarky(54)  =   0.088379
  y_clarky(55)  =   0.089035
  y_clarky(56)  =   0.089690
  y_clarky(57)  =   0.090345
  y_clarky(58)  =   0.091001
  y_clarky(59)  =   0.091452
  y_clarky(60)  =   0.091752
  y_clarky(61)  =   0.092052
  y_clarky(62)  =   0.092352
  y_clarky(63)  =   0.092652
  y_clarky(64)  =   0.092952
  y_clarky(65)  =   0.093129
  y_clarky(66)  =   0.093149
  y_clarky(67)  =   0.093169
  y_clarky(68)  =   0.093101
  y_clarky(69)  =   0.092923
  y_clarky(70)  =   0.092746
  y_clarky(71)  =   0.092464
  y_clarky(72)  =   0.092085
  y_clarky(73)  =   0.091706
  y_clarky(74)  =   0.091188
  y_clarky(75)  =   0.090601
  y_clarky(76)  =   0.090014
  y_clarky(77)  =   0.089202
  y_clarky(78)  =   0.088367
  y_clarky(79)  =   0.087454
  y_clarky(80)  =   0.086310
  y_clarky(81)  =   0.085165
  y_clarky(82)  =   0.083748
  y_clarky(83)  =   0.082218
  y_clarky(84)  =   0.080558
  y_clarky(85)  =   0.078574
  y_clarky(86)  =   0.076588
  y_clarky(87)  =   0.074129
  y_clarky(88)  =   0.071670
  y_clarky(89)  =   0.068802
  y_clarky(90)  =   0.065870
  y_clarky(91)  =   0.062557
  y_clarky(92)  =   0.059168
  y_clarky(93)  =   0.055379
  y_clarky(94)  =   0.051379
  y_clarky(95)  =   0.047069
  y_clarky(96)  =   0.042084
  y_clarky(97)  =   0.036354
  y_clarky(98)  =   0.029378
  y_clarky(99)  =   0.020238
  y_clarky(100) =   0.000470
  y_clarky(101) =   -0.013312
  y_clarky(102) =   -0.017696
  y_clarky(103) =   -0.020588
  y_clarky(104) =   -0.022633
  y_clarky(105) =   -0.024157
  y_clarky(106) =   -0.025183
  y_clarky(107) =   -0.026026
  y_clarky(108) =   -0.026702
  y_clarky(109) =   -0.027186
  y_clarky(110) =   -0.027634
  y_clarky(111) =   -0.027898
  y_clarky(112) =   -0.028161
  y_clarky(113) =   -0.028267
  y_clarky(114) =   -0.028374
  y_clarky(115) =   -0.028364
  y_clarky(116) =   -0.028334
  y_clarky(117) =   -0.028239
  y_clarky(118) =   -0.028087
  y_clarky(119) =   -0.027928
  y_clarky(120) =   -0.027690
  y_clarky(121) =   -0.027453
  y_clarky(122) =   -0.027182
  y_clarky(123) =   -0.026880
  y_clarky(124) =   -0.026579
  y_clarky(125) =   -0.026247
  y_clarky(126) =   -0.02591
  y_clarky(127) =   -0.025573
  y_clarky(128) =   -0.025222
  y_clarky(129) =   -0.024871
  y_clarky(130) =   -0.024519
  y_clarky(131) =   -0.024157
  y_clarky(132) =   -0.023796
  y_clarky(133) =   -0.023434
  y_clarky(134) =   -0.023074
  y_clarky(135) =   -0.022714
  y_clarky(136) =   -0.022354
  y_clarky(137) =   -0.021992
  y_clarky(138) =   -0.021631
  y_clarky(139) =   -0.021270
  y_clarky(140) =   -0.020908
  y_clarky(141) =   -0.020547
  y_clarky(142) =   -0.020186
  y_clarky(143) =   -0.019828
  y_clarky(144) =   -0.019469
  y_clarky(145) =   -0.019111
  y_clarky(146) =   -0.018753
  y_clarky(147) =   -0.018394
  y_clarky(148) =   -0.018036
  y_clarky(149) =   -0.017678
  y_clarky(150) =   -0.017319
  y_clarky(151) =   -0.016961
  y_clarky(152) =   -0.016603
  y_clarky(153) =   -0.016244
  y_clarky(154) =   -0.015886
  y_clarky(155) =   -0.015528
  y_clarky(156) =   -0.015172
  y_clarky(157) =   -0.014815
  y_clarky(158) =   -0.014459
  y_clarky(159) =   -0.014102
  y_clarky(160) =   -0.013746
  y_clarky(161) =   -0.013389
  y_clarky(162) =   -0.013032
  y_clarky(163) =   -0.012674
  y_clarky(164) =   -0.012317
  y_clarky(165) =   -0.011959
  y_clarky(166) =   -0.011602
  y_clarky(167) =   -0.011244
  y_clarky(168) =   -0.010886
  y_clarky(169) =   -0.010528
  y_clarky(170) =   -0.010170
  y_clarky(171) =   -0.0098118
  y_clarky(172) =   -0.0094537
  y_clarky(173) =   -0.0090968
  y_clarky(174) =   -0.0087410
  y_clarky(175) =   -0.0083852
  y_clarky(176) =   -0.0080294
  y_clarky(177) =   -0.0076736
  y_clarky(178) =   -0.0073195
  y_clarky(179) =   -0.0069701
  y_clarky(180) =   -0.0066208
  y_clarky(181) =   -0.0062715
  y_clarky(182) =   -0.0059222
  y_clarky(183) =   -0.0055724
  y_clarky(184) =   -0.0052221
  y_clarky(185) =   -0.0048719
  y_clarky(186) =   -0.0045216
  y_clarky(187) =   -0.0041675
  y_clarky(188) =   -0.0038058
  y_clarky(189) =   -0.0034441
  y_clarky(190) =   -0.0030824
  y_clarky(191) =   -0.0026885
  y_clarky(192) =   -0.0022879
  y_clarky(193) =   -0.0018851
  y_clarky(194) =   -0.0014126
  y_clarky(195) =   -0.0009400
  y_clarky(196) =   -0.0007020
  y_clarky(197) =   -0.0001120
  y_clarky(198) =    0.000743
  y_clarky(199) =    0.001743
! --------------------------------
  np = 199
  do i = 1,np
   xb(i) = x_clarky(i)
   yb(i) = y_clarky(i)
!   print*,xb(i),yb(i)
  enddo

return
end subroutine clarky

!*************************************************************
!***** Counter Propeller airfoil definition called Clarky ************
!*************************************************************
subroutine negclarky(xb,yb,np)
! Clarky propeller airfoil profile.
implicit none

integer i,np,nx
parameter(nx=500)
real x_clarky(nx),y_clarky(nx)
real, intent(out) :: xb(nx),yb(nx)

!----x coordinates--------------------
  x_clarky(1)   =   0.98552
  x_clarky(2)   =   0.98538
  x_clarky(3)   =   0.98495
  x_clarky(4)   =   0.98430
  x_clarky(5)   =   0.98349
  x_clarky(6)   =   0.98296
  x_clarky(7)   =   0.97250
  x_clarky(8)   =   0.96205
  x_clarky(9)   =   0.95159
  x_clarky(10)  =   0.94113
  x_clarky(11)  =   0.93067
  x_clarky(12)  =   0.92022
  x_clarky(13)  =   0.90976
  x_clarky(14)  =   0.89930
  x_clarky(15)  =   0.88885
  x_clarky(16)  =   0.87839
  x_clarky(17)  =   0.86793
  x_clarky(18)  =   0.85748
  x_clarky(19)  =   0.84702
  x_clarky(20)  =   0.83656
  x_clarky(21)  =   0.82610
  x_clarky(22)  =   0.81565
  x_clarky(23)  =   0.80519
  x_clarky(24)  =   0.79473
  x_clarky(25)  =   0.78428
  x_clarky(26)  =   0.77382
  x_clarky(27)  =   0.76336
  x_clarky(28)  =   0.75291
  x_clarky(29)  =   0.74245
  x_clarky(30)  =   0.73199
  x_clarky(31)  =   0.72153
  x_clarky(32)  =   0.71108
  x_clarky(33)  =   0.70062
  x_clarky(34)  =   0.69016
  x_clarky(35)  =   0.67971
  x_clarky(36)  =   0.66925
  x_clarky(37)  =   0.65879
  x_clarky(38)  =   0.64834
  x_clarky(39)  =   0.63788
  x_clarky(40)  =   0.62742
  x_clarky(41)  =   0.61696
  x_clarky(42)  =   0.60651
  x_clarky(43)  =   0.59605
  x_clarky(44)  =   0.58559
  x_clarky(45)  =   0.57514
  x_clarky(46)  =   0.56468
  x_clarky(47)  =   0.55422
  x_clarky(48)  =   0.54377
  x_clarky(49)  =   0.53331
  x_clarky(50)  =   0.52285
  x_clarky(51)  =   0.51239
  x_clarky(52)  =   0.50194
  x_clarky(53)  =   0.49148
  x_clarky(54)  =   0.48102
  x_clarky(55)  =   0.47057
  x_clarky(56)  =   0.46011
  x_clarky(57)  =   0.44965
  x_clarky(58)  =   0.43919
  x_clarky(59)  =   0.42874
  x_clarky(60)  =   0.41828
  x_clarky(61)  =   0.40782
  x_clarky(62)  =   0.39737
  x_clarky(63)  =   0.38691
  x_clarky(64)  =   0.37645
  x_clarky(65)  =   0.36600
  x_clarky(66)  =   0.35554
  x_clarky(67)  =   0.34508
  x_clarky(68)  =   0.33462
  x_clarky(69)  =   0.32417
  x_clarky(70)  =   0.31371
  x_clarky(71)  =   0.30325
  x_clarky(72)  =   0.29280
  x_clarky(73)  =   0.28234
  x_clarky(74)  =   0.27188
  x_clarky(75)  =   0.26143
  x_clarky(76)  =   0.25097
  x_clarky(77)  =   0.24051
  x_clarky(78)  =   0.23005
  x_clarky(79)  =   0.21960
  x_clarky(80)  =   0.20914
  x_clarky(81)  =   0.19868
  x_clarky(82)  =   0.18823
  x_clarky(83)  =   0.17777
  x_clarky(84)  =   0.16731
  x_clarky(85)  =   0.15686
  x_clarky(86)  =   0.14640
  x_clarky(87)  =   0.13594
  x_clarky(88)  =   0.12548
  x_clarky(89)  =   0.11503
  x_clarky(90)  =   0.10457
  x_clarky(91)  =   0.094113
  x_clarky(92)  =   0.083656
  x_clarky(93)  =   0.073199
  x_clarky(94)  =   0.062742
  x_clarky(95)  =   0.052285
  x_clarky(96)  =   0.041828
  x_clarky(97)  =   0.031371
  x_clarky(98)  =   0.020914
  x_clarky(99)  =   0.010457
  x_clarky(100) =   0.000000
  x_clarky(101) =   0.010347
  x_clarky(102) =   0.020694
  x_clarky(103) =   0.031041
  x_clarky(104) =   0.041388
  x_clarky(105) =   0.051735
  x_clarky(106) =   0.062082
  x_clarky(107) =   0.072429
  x_clarky(108) =   0.082776
  x_clarky(109) =   0.093123
  x_clarky(110) =   0.10347
  x_clarky(111) =   0.11382
  x_clarky(112) =   0.12416
  x_clarky(113) =   0.13451
  x_clarky(114) =   0.14486
  x_clarky(115) =   0.15520
  x_clarky(116) =   0.16555
  x_clarky(117) =   0.17590
  x_clarky(118) =   0.18625
  x_clarky(119) =   0.19659
  x_clarky(120) =   0.20694
  x_clarky(121) =   0.21729
  x_clarky(122) =   0.22763
  x_clarky(123) =   0.23798
  x_clarky(124) =   0.24833
  x_clarky(125) =   0.25867
  x_clarky(126) =   0.26902
  x_clarky(127) =   0.27937
  x_clarky(128) =   0.28971
  x_clarky(129) =   0.30006
  x_clarky(130) =   0.31041
  x_clarky(131) =   0.32076
  x_clarky(132) =   0.33110
  x_clarky(133) =   0.34145
  x_clarky(134) =   0.35180
  x_clarky(135) =   0.36214
  x_clarky(136) =   0.37249
  x_clarky(137) =   0.38284
  x_clarky(138) =   0.39318
  x_clarky(139) =   0.40353
  x_clarky(140) =   0.41388
  x_clarky(141) =   0.42422
  x_clarky(142) =   0.43457
  x_clarky(143) =   0.44492
  x_clarky(144) =   0.45527
  x_clarky(145) =   0.46561
  x_clarky(146) =   0.47596
  x_clarky(147) =   0.48631
  x_clarky(148) =   0.49655
  x_clarky(149) =   0.50700
  x_clarky(150) =   0.51735
  x_clarky(151) =   0.52769
  x_clarky(152) =   0.53804
  x_clarky(153) =   0.54839
  x_clarky(154) =   0.55874
  x_clarky(155) =   0.56908
  x_clarky(156) =   0.57943
  x_clarky(157) =   0.58978
  x_clarky(158) =   0.60012
  x_clarky(159) =   0.61047
  x_clarky(160) =   0.62082
  x_clarky(161) =   0.63116
  x_clarky(162) =   0.64151
  x_clarky(163) =   0.65186
  x_clarky(164) =   0.66220
  x_clarky(165) =   0.67255
  x_clarky(166) =   0.68290
  x_clarky(167) =   0.69325
  x_clarky(168) =   0.70359
  x_clarky(169) =   0.71394
  x_clarky(170) =   0.72429
  x_clarky(171) =   0.73463
  x_clarky(172) =   0.74498
  x_clarky(173) =   0.75533
  x_clarky(174) =   0.76567
  x_clarky(175) =   0.77602
  x_clarky(176) =   0.78637
  x_clarky(177) =   0.79671
  x_clarky(178) =   0.80706
  x_clarky(179) =   0.81741
  x_clarky(180) =   0.82776
  x_clarky(181) =   0.83810
  x_clarky(182) =   0.84845
  x_clarky(183) =   0.85880
  x_clarky(184) =   0.86914
  x_clarky(185) =   0.87949
  x_clarky(186) =   0.88984
  x_clarky(187) =   0.90018
  x_clarky(188) =   0.91053
  x_clarky(189) =   0.92088
  x_clarky(190) =   0.93123
  x_clarky(191) =   0.94157
  x_clarky(192) =   0.95192
  x_clarky(193) =   0.96227
  x_clarky(194) =   0.97261
  x_clarky(195) =   0.98296
  x_clarky(196) =   0.98395
  x_clarky(197) =   0.98478
  x_clarky(198) =   0.98533
  x_clarky(199) =   0.98552
!---------------------------------      
!   y coordinates  
  y_clarky(1)   =   0.001743
  y_clarky(2)   =   0.002617
  y_clarky(3)   =   0.003396
  y_clarky(4)   =   0.003995
  y_clarky(5)   =   0.004349
  y_clarky(6)   =   0.004480
  y_clarky(7)   =   0.0070868
  y_clarky(8)   =   0.0096936
  y_clarky(9)   =   0.012167
  y_clarky(10)  =   0.014640
  y_clarky(11)  =   0.017086
  y_clarky(12)  =   0.019444
  y_clarky(13)  =   0.021801
  y_clarky(14)  =   0.024159
  y_clarky(15)  =   0.026430
  y_clarky(16)  =   0.028673
  y_clarky(17)  =   0.030915
  y_clarky(18)  =   0.033158
  y_clarky(19)  =   0.035320
  y_clarky(20)  =   0.037434
  y_clarky(21)  =   0.039548
  y_clarky(22)  =   0.041662
  y_clarky(23)  =   0.043776
  y_clarky(24)  =   0.045746
  y_clarky(25)  =   0.047703
  y_clarky(26)  =   0.049661
  y_clarky(27)  =   0.051619
  y_clarky(28)  =   0.053576
  y_clarky(29)  =   0.055398
  y_clarky(30)  =   0.057168
  y_clarky(31)  =   0.058939
  y_clarky(32)  =   0.060709
  y_clarky(33)  =   0.062479
  y_clarky(34)  =   0.064224
  y_clarky(35)  =   0.065767
  y_clarky(36)  =   0.067310
  y_clarky(37)  =   0.068854
  y_clarky(38)  =   0.070397
  y_clarky(39)  =   0.071940
  y_clarky(40)  =   0.073434
  y_clarky(41)  =   0.074715
  y_clarky(42)  =   0.075996
  y_clarky(43)  =   0.077277
  y_clarky(44)  =   0.078559
  y_clarky(45)  =   0.079840
  y_clarky(46)  =   0.081105
  y_clarky(47)  =   0.082089
  y_clarky(48)  =   0.083072
  y_clarky(49)  =   0.084056
  y_clarky(50)  =   0.085040
  y_clarky(51)  =   0.086024
  y_clarky(52)  =   0.087008
  y_clarky(53)  =   0.087724
  y_clarky(54)  =   0.088379
  y_clarky(55)  =   0.089035
  y_clarky(56)  =   0.089690
  y_clarky(57)  =   0.090345
  y_clarky(58)  =   0.091001
  y_clarky(59)  =   0.091452
  y_clarky(60)  =   0.091752
  y_clarky(61)  =   0.092052
  y_clarky(62)  =   0.092352
  y_clarky(63)  =   0.092652
  y_clarky(64)  =   0.092952
  y_clarky(65)  =   0.093129
  y_clarky(66)  =   0.093149
  y_clarky(67)  =   0.093169
  y_clarky(68)  =   0.093101
  y_clarky(69)  =   0.092923
  y_clarky(70)  =   0.092746
  y_clarky(71)  =   0.092464
  y_clarky(72)  =   0.092085
  y_clarky(73)  =   0.091706
  y_clarky(74)  =   0.091188
  y_clarky(75)  =   0.090601
  y_clarky(76)  =   0.090014
  y_clarky(77)  =   0.089202
  y_clarky(78)  =   0.088367
  y_clarky(79)  =   0.087454
  y_clarky(80)  =   0.086310
  y_clarky(81)  =   0.085165
  y_clarky(82)  =   0.083748
  y_clarky(83)  =   0.082218
  y_clarky(84)  =   0.080558
  y_clarky(85)  =   0.078574
  y_clarky(86)  =   0.076588
  y_clarky(87)  =   0.074129
  y_clarky(88)  =   0.071670
  y_clarky(89)  =   0.068802
  y_clarky(90)  =   0.065870
  y_clarky(91)  =   0.062557
  y_clarky(92)  =   0.059168
  y_clarky(93)  =   0.055379
  y_clarky(94)  =   0.051379
  y_clarky(95)  =   0.047069
  y_clarky(96)  =   0.042084
  y_clarky(97)  =   0.036354
  y_clarky(98)  =   0.029378
  y_clarky(99)  =   0.020238
  y_clarky(100) =   0.000470
  y_clarky(101) =   -0.013312
  y_clarky(102) =   -0.017696
  y_clarky(103) =   -0.020588
  y_clarky(104) =   -0.022633
  y_clarky(105) =   -0.024157
  y_clarky(106) =   -0.025183
  y_clarky(107) =   -0.026026
  y_clarky(108) =   -0.026702
  y_clarky(109) =   -0.027186
  y_clarky(110) =   -0.027634
  y_clarky(111) =   -0.027898
  y_clarky(112) =   -0.028161
  y_clarky(113) =   -0.028267
  y_clarky(114) =   -0.028374
  y_clarky(115) =   -0.028364
  y_clarky(116) =   -0.028334
  y_clarky(117) =   -0.028239
  y_clarky(118) =   -0.028087
  y_clarky(119) =   -0.027928
  y_clarky(120) =   -0.027690
  y_clarky(121) =   -0.027453
  y_clarky(122) =   -0.027182
  y_clarky(123) =   -0.026880
  y_clarky(124) =   -0.026579
  y_clarky(125) =   -0.026247
  y_clarky(126) =   -0.02591
  y_clarky(127) =   -0.025573
  y_clarky(128) =   -0.025222
  y_clarky(129) =   -0.024871
  y_clarky(130) =   -0.024519
  y_clarky(131) =   -0.024157
  y_clarky(132) =   -0.023796
  y_clarky(133) =   -0.023434
  y_clarky(134) =   -0.023074
  y_clarky(135) =   -0.022714
  y_clarky(136) =   -0.022354
  y_clarky(137) =   -0.021992
  y_clarky(138) =   -0.021631
  y_clarky(139) =   -0.021270
  y_clarky(140) =   -0.020908
  y_clarky(141) =   -0.020547
  y_clarky(142) =   -0.020186
  y_clarky(143) =   -0.019828
  y_clarky(144) =   -0.019469
  y_clarky(145) =   -0.019111
  y_clarky(146) =   -0.018753
  y_clarky(147) =   -0.018394
  y_clarky(148) =   -0.018036
  y_clarky(149) =   -0.017678
  y_clarky(150) =   -0.017319
  y_clarky(151) =   -0.016961
  y_clarky(152) =   -0.016603
  y_clarky(153) =   -0.016244
  y_clarky(154) =   -0.015886
  y_clarky(155) =   -0.015528
  y_clarky(156) =   -0.015172
  y_clarky(157) =   -0.014815
  y_clarky(158) =   -0.014459
  y_clarky(159) =   -0.014102
  y_clarky(160) =   -0.013746
  y_clarky(161) =   -0.013389
  y_clarky(162) =   -0.013032
  y_clarky(163) =   -0.012674
  y_clarky(164) =   -0.012317
  y_clarky(165) =   -0.011959
  y_clarky(166) =   -0.011602
  y_clarky(167) =   -0.011244
  y_clarky(168) =   -0.010886
  y_clarky(169) =   -0.010528
  y_clarky(170) =   -0.010170
  y_clarky(171) =   -0.0098118
  y_clarky(172) =   -0.0094537
  y_clarky(173) =   -0.0090968
  y_clarky(174) =   -0.0087410
  y_clarky(175) =   -0.0083852
  y_clarky(176) =   -0.0080294
  y_clarky(177) =   -0.0076736
  y_clarky(178) =   -0.0073195
  y_clarky(179) =   -0.0069701
  y_clarky(180) =   -0.0066208
  y_clarky(181) =   -0.0062715
  y_clarky(182) =   -0.0059222
  y_clarky(183) =   -0.0055724
  y_clarky(184) =   -0.0052221
  y_clarky(185) =   -0.0048719
  y_clarky(186) =   -0.0045216
  y_clarky(187) =   -0.0041675
  y_clarky(188) =   -0.0038058
  y_clarky(189) =   -0.0034441
  y_clarky(190) =   -0.0030824
  y_clarky(191) =   -0.0026885
  y_clarky(192) =   -0.0022879
  y_clarky(193) =   -0.0018851
  y_clarky(194) =   -0.0014126
  y_clarky(195) =   -0.0009400
  y_clarky(196) =   -0.0007020
  y_clarky(197) =   -0.0001120
  y_clarky(198) =    0.000743
  y_clarky(199) =    0.001743
! --------------------------------
  np = 199
  do i = 1,np
   xb(i) = x_clarky(i)
   yb(i) = -y_clarky(i) ! Counter Rotating Airfoil for Clarky
!   print*,xb(i),yb(i)
  enddo

return
end subroutine negclarky

!************************************************************************
!****NACA 4 series airfoil definition************************************
!************************************************************************
     SUBROUTINE MakeFoil(naca,xbot,ybot,xtop,ytop,np,x)
      
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: naca, np
      INTEGER :: i
      REAL, DIMENSION(np), INTENT(OUT) :: xtop, ytop, xbot, ybot
      REAL, DIMENSION(np), INTENT(IN) :: x
      REAL, DIMENSION(np) :: yc, yt
      REAL :: t, m, p, th
      LOGICAL :: NACA4, isquiet
           
      ! Initializing t
      t = 0.0

      ! Get isquiet status
      call get_quiet_status(isquiet)
            
      if (.not. isquiet) print*,"Using NACA airfoil subroutine"
      ! Initialize variables
      NACA4 = .FALSE.
          ! Identify the airfoils as NACA 4 or 5 digit or decomposed NACA 4 digit
      IF(naca .GE.  0001 .AND. naca .LT.  10000) NACA4 = .TRUE.


      ! Decompose the NACA number
      IF(NACA4) THEN
        ! NACA 4 series
        m = floor( real(naca) / 1000.)
        p = floor((real(naca) - m * 1000.) / 100.)
        t = real(naca) - m * 1000. - p * 100.

        m = m / 100.
        t = t / 100.
        p = p / 10.
        !print* ,'m,t,p: ',m,t,p
        !stop
      
      ENDIF

      DO i = 1,np
        ! Calculate the thicknes distribution for both 4 and 5 series NACA
        yt(i) = 5.*t*(0.29690*sqrt(x(i))-0.12600*x(i)-0.35160*x(i)**2+0.28430* &
                     x(i)**3-0.10150*x(i)**4)

        ! Calculate NACA 4 series camber
        IF(NACA4) THEN
          IF(ABS(p).LE.1e-15) THEN
            yc(i) = 0.
          ELSEIF(x(i).LE.p) THEN
            yc(i) = m / (p**2) * (2.0*p*x(i)-x(i)**2)
          ELSE
            yc(i) = m / ((1.0-p)**2) * ((1.0-2.0*p) + 2.0*p*x(i)-x(i)**2)
          ENDIF
        ENDIF
        ENDDO
 
      ! Calculate theta from the chord line
      ! DO i = 1,np
        ! IF (i .EQ. np) THEN
          ! th = -atan((yc(i)-yc(i+1)) / (x(i)-x(i+1)))
        ! ELSE
          ! th =  atan((yc(i+1)-yc(i)) / (x(i+1)-x(i)))
        ! ENDIF
      DO i = 2,np 
        !IF (i .EQ. np) THEN
        !  th = -atan((yc(i-1)-yc(i)) / (x(i-1)-x(i)))
        !ELSE
          th =  atan((yc(i)-yc(i-1)) / (x(i)-x(i-1)))
        !ENDIF

        ! Calculate the airfoil shape
        xbot(i) = x(i)  + yt(i) * sin(th)
        ybot(i) = yc(i) - yt(i) * cos(th)        
        xtop(i) = x(i)  - yt(i) * sin(th)
        ytop(i) = yc(i) + yt(i) * cos(th)

      ENDDO
      
      RETURN
      END subroutine
 
!******************************************************************
!******User input airfoil definition*******************************
!******************************************************************
! File Format:
! airfoil name
! Number of airfoil coordinates
! "x     y"
! x1 y1
! x2 y2
! .. ..
! xn yn
! End of File
!******************************************************************
subroutine datafile(airfoil,xb,yb,np)
! reading an airfoil data from an external file
implicit none

integer i,np,nx
parameter(nx=500)
character *20 fname1,temp,airfoil
real x_file1(nx),y_file1(nx)
real, intent(out) :: xb(nx),yb(nx)
logical :: isquiet

call get_quiet_status(isquiet)

! Stagger and chord values from the 3dbgbinput file....3/4/13
fname1 = trim(airfoil)//'.dat'
open(3,file=fname1,status='unknown')
read(3,*)temp ! name of the airfoil
if (.not. isquiet) write(*,*)'Reading the airfoil coordinates from: ',fname1
read(3,*)np ! Number of airfoil coordinates.
read(3,*)temp !Skipping "x     y"
if (.not. isquiet) print*,'Number of airfoil coordinates in the file:',np
do i = 1, np
 read(3,*,end=5)x_file1(i),y_file1(i)
 xb(i) = x_file1(i)
 yb(i) = y_file1(i)
enddo
5 close (3)

return
end subroutine datafile

!****************************************************************************
!****************************************************************************
!******'sect1' airfoil definition which is the default airfoil in bladegen.f90
!****************************************************************************
!---- naca 4 digit camber + thickness
!
      subroutine cambnaca(u, cam, cam_u, fmxcm, xmxcm)
! 
      if(u.lt.xmxcm) then
       cam = (fmxcm/xmxcm**2)*(2.0*xmxcm*u-u*u)
       cam_u = 0.0
      else
       cam = (fmxcm/(1.0-xmxcm)**2)*((1.0-2.0*xmxcm)+2.0*xmxcm*u-u*u)
       cam_u = 0.0
      endif
!
      return
      end
!
      subroutine thicknaca(u,thk,fmxthk)
!
      thk = 5.*fmxthk*(0.29690*sqrt(u)-(0.12600+0.35160*u-0.28430* &
                   u**2+0.10150*u**3)*u)
!
      return
      end

!
!---- Wennerstrom thickness profile with circular le/te
!
      subroutine thickwen(uin, thk, lethk, tethk, mxthk, umxthin,thkmultip)
!---Thickness multiplier added by Ahmed Nemnem-----

      real lethk, tethk, mxthk, uscale, thkmultip
!
      if(umxthin.lt.0.5) then
       thk1 = tethk
       thk2 = lethk
       umxth = 1.0-umxthin
       ub = (1.0-uin)
      else
       thk1 = lethk
       thk2 = tethk
       umxth = umxthin
       ub = uin
      endif
!
      aa = -(0.5*mxthk- 0.5*thk1)/(2.0*umxth**3)
      bb = 0.0
      cc = 3.0*(0.5*mxthk - 0.5*thk1)/(2.0*umxth)
      dd = 0.5*thk1
      ee = 3.0*(0.5*mxthk - 0.5*thk1)/(2.0*umxth**2*(1.0-umxth))
      ee = ee - (0.5*mxthk - 0.5*thk2)/(1.0-umxth)**3
      ff = -3.0*(0.5*mxthk - 0.5*thk1)/(2.0*umxth**2)
      gg = 0.0
      hh = 0.5*mxthk
!
      uscale   = 1.0 + 0.5*(lethk+tethk)
      u  = -0.5*lethk + uscale*uin       
!
      if(umxthin.lt.0.5) then
       ub = (1.0-u)
      else
       ub = u
      endif
!
      if(u.le.0.0) then
       thk = sqrt(0.25*lethk*lethk-u**2)
      elseif(u .ge. (1.0+0.5*tethk)) then 
!----- this is checked because of precision
       thk = 0.
      elseif(u.ge.1.0) then
       thk = sqrt(0.25*tethk*tethk-(1.0-u)**2)
      else
       if(ub.lt.umxth) then
        thk = (aa*ub**3 + bb*ub**2 + cc*ub + dd)*(thkmultip + 1)
       else
        ub1 = ub-umxth
        thk = (ee*ub1**3 + ff*ub1**2 + gg*ub1 + hh)*(thkmultip + 1)
       endif
      endif
!
      return
      end
!
!---- Wennerstrom thickness profile with elliptical le/te
!
      subroutine thickellip(i,uin,thk,lethk,tethk,mxthk,umxthin,rr1,rr2,thkmultip &
      ,u_le,uin_le,i_le,oo,i_te)
!-------Thickness multiplier added by Ahmed Nemnem.
      real lethk, tethk, mxthk, uscale, thkmultip
      real u_le,uin_le
      integer i_le,oo,i,i_te, thick

      ! Get thickness multiplier switch value
      call get_thick_status(thick)

      !print*,'lethk =',lethk
      !print*,'tethk =',tethk
      !print*,'mxthk =',mxthk
      !print*,'umxthin =',umxthin
      !print*,'rr1 =',rr1
      !print*,'rr2 =',rr2

      i_le = 0
      i_te = 0
      if(umxthin.lt.0.5) then
       thk1 = tethk
       thk2 = lethk
       umxth = 1.0-umxthin
      else
       thk1 = lethk
       thk2 = tethk
       umxth = umxthin
      endif
!
      aa = -(0.5*mxthk- 0.5*thk1)/(2.0*umxth**3)
      bb = 0.0
      cc = 3.0*(0.5*mxthk - 0.5*thk1)/(2.0*umxth)
      dd = 0.5*thk1
      ee = 3.0*(0.5*mxthk - 0.5*thk1)/(2.0*umxth**2*(1.0-umxth))
      ee = ee - (0.5*mxthk - 0.5*thk2)/(1.0-umxth)**3
      ff = -3.0*(0.5*mxthk - 0.5*thk1)/(2.0*umxth**2)
      gg = 0.0
      hh = 0.5*mxthk
!
      if(umxthin.lt.0.5) then
       ub1 = 1.0-umxth
       y1  = ee*ub1**3 + ff*ub1**2 + gg*ub1 + hh
       yp1 = -(3.*ee*ub1**2 + 2.*ff*ub1 + gg)
       y2  = dd
       yp2 = cc
      else
       y1  = dd
       yp1 = cc
       ub1 = 1.0-umxth
       y2  = ee*ub1**3 + ff*ub1**2 + gg*ub1 + hh
       yp2 = -(3.*ee*ub1**2 + 2.*ff*ub1 + gg)
      endif
!
      yyp1 = y1*yp1
      x1   = sqrt((yyp1*rr1**2)**2+rr1**2*y1**2)-yyp1*rr1**2
      ale  = yyp1*rr1**2+x1
      ble  = ale/rr1
!
      yyp2 = y2*yp2
      x2   = sqrt((yyp2*rr2**2)**2+rr2**2*y2**2)-yyp2*rr2**2
      ate  = yyp2*rr2**2+x2
      bte  = ate/rr2
!
      uscale   = 1.0 + x1 + x2
      u = -x1 + uscale*uin       
!
      if(umxthin.lt.0.5) then
       ub = (1.0-u)
      else
       ub = u
      endif
! 
      !print*,'x1 =',x1
      !print*,'uscale =',uscale
      !print*,'uin before =',uin
      !print*,'u before =',u
      !print*,'uin_le before =',uin_le
! 00000000000000000000000000000 nemnem
      if(uin == 0) then
         oo = 10
         uin_le=100
       end if   
! 0000000000000000000000000000000000000    
      if(u.le.0.0) then
          if (thick /= 0) then
              thk = sqrt((ale**2-(u+x1-ale)**2)/rr1**2)
          elseif (thick == 0) then
              thk = sqrt((ale**2-(u+x1-ale)**2)/rr1**2)*(thkmultip + 1)
          end if
      elseif(u .ge.(1.+x2)) then !  this is checked because of precision
          thk = 0.
      elseif(u.ge.1.0) then
          if (thick /= 0) then
              thk = sqrt((ate**2-(u-(1.+x2)+ate)**2)/rr2**2)
          else if (thick == 0) then
              thk = sqrt((ate**2-(u-(1.+x2)+ate)**2)/rr2**2)*(thkmultip + 1)
          end if
      else
       if(ub.lt.umxth) then
        thk = (aa*ub**3 + bb*ub**2 + cc*ub + dd)*(thkmultip + 1)
        i_te = i
        !print*,"i_te = ",i_te
       else
        ub1 = ub-umxth
        thk = (ee*ub1**3 + ff*ub1**2 + gg*ub1 + hh)*(thkmultip+1) !======
!00000000000000000000000000000000000000000000000000000000000000000
        if (oo==10) then 
         !print*,'oo=',oo
         u_le = ub1
         uin_le = uin
         i_le = i
         !print*,'i_le sub =',i_le
         !print*,'uin_le sub=',uin_le
         oo = oo+10
        endif
        !print*,'oo=',oo
!000000000000000000000000000000000000000000000000000000000000000000 
       endif
      endif
!
      return
      end
!
!
!---- cubic camberline
!
      subroutine cambcubic(u,cam, cam_u, s1,s2)
!
      dd = 0.0
      cc = s1
      aa = s2+s1
      bb = -s1-aa
!
      cam = aa*u**3+bb*u**2+cc*u+dd
      cam_u = 3.*aa*u**2+2.*bb*u+cc
!
      return
      end
!
!
!--- mixed camberline
!
      subroutine cambmix(u,cam, cam_u, s1,s2,fl1,fl2)

      al1 = atan(s1)
      al2 = atan(s2)
!
      u1  = fl1*cos(al1)
!      print*,u1
      c1  = fl1*sin(al1)
      u2  = 1.0 - fl2*cos(al2)
!      print*,u2
      c2  =     - fl2*sin(al2)
!
      xb = u2-u1
      dd = c1
      cc = s1
      aa = (s2+s1-2.*(c2-dd)/xb)/xb**2
      bb = (-s1*xb-aa*xb**3+c2-dd)/xb**2
!
      if(u.le.u1) then
       cam = u*s1
       cam_u = s1
      elseif(u.ge.u2) then
       cam = -(1.0-u)*s2
       cam_u = s2
      else
       ub = u-u1
       cam = aa*ub**3+bb*ub**2+cc*ub+dd         
       cam_u = 3.*aa*ub**2+2.*bb*ub+cc
       curv = 6.*aa*ub+2.*bb
      endif
!
!      print*,cam
!      print*,cam_u 
!       print*,curv     
      return
      end       
!**********************************************************************************
!circular TE add on
subroutine circularTE(xbot,ybot,xtop,ytop,np)
! ! This adds a circular TE to any bladeshape.
implicit none
integer ncirc,np,np_side,i

real*8 mtop,mbot,mtop_normal,mbot_normal,ctop,cbot,radius
real*8 xcenter,ycenter,xmean,ymean,xTEnew,yTEnew,deltax
real*8, allocatable, dimension(:) :: xcirc,ycirc  
real*8, intent(inout):: xbot(np),ybot(np),xtop(np),ytop(np)    
logical :: isquiet

np_side = np

! Get isquiet status
call get_quiet_status(isquiet)


! file1 = 'uvnaca.dat'
! open(1,file=file1,status='unknown')
! write(1,*)'naca airfoil'
! write(1,*) np
! write(1,*) 'u         v'
! do i = 1, np
! write(1,*)xbot(i),ybot(i)
! enddo
! !write(1,*)
! close(1)
!stop
!----------------------------------------------------------
!Adding circular TE
!----------------------------------------------------------
!Top Curve -----------------------
!Slope of the top curve at the TE
mtop = (ytop(np_side)-ytop(np_side - 1))/(xtop(np_side)-xtop(np_side - 1))
!slope of the normal
mtop_normal = -1/mtop
!Y-intercept of the line passing through the 1st point of the top curve
!and the center of the circle. (The normal to the tangent at the 1st point on TE)
!Line eqn.| ytop(1) = mtop_normal*xtop(1) + ctop
ctop = ytop(np_side) - mtop_normal*xtop(np_side)
!Bottom Curve --------------------
!Slope of bottom curve at the TE
mbot = (ybot(np_side)-ybot(np_side - 1))/(xbot(np_side)-xbot(np_side - 1))
!slope of the normal
mbot_normal = -1/mbot
!Y-intercept of the line passing through the 1st point of the bottom curve
!and the center of the circle. (The normal to the tangent at the 1st point on TE)
!Line eqn. | ybot(1) = mbot_normal*xbot(1) + cbot
cbot = ybot(np_side) - mbot_normal*xbot(np_side - 1)
! Calculating the center by finding the intersection point of the 2 normals
xcenter = (ctop - cbot)/(mbot_normal - mtop_normal)
ycenter = mtop_normal*xcenter + ctop
! Radius of the circle
radius = sqrt((ytop(np_side) - ycenter)**2 + (xtop(np_side) - xcenter)**2)
!radius = radius - 0.00001
! Mid point of the TE points
xmean = xtop(np_side)
ymean = 0.5*(ytop(np_side) + ybot(np_side))
if (.not. isquiet) print*,'radius: ',radius
if (.not. isquiet) print*,xmean,ymean,' |xmean,ymean'
!stop
!The new TE. The point on the circular TE
xTEnew = xcenter + radius
yTEnew = ymean
! Calculating ncirc-1 equidistant points between the mid point and the new TE point
ncirc = 5
if (allocated(xcirc)) deallocate(xcirc)
if (allocated(ycirc)) deallocate(ycirc)
allocate(xcirc(ncirc-1),ycirc(ncirc-1))
deltax = (xTEnew - xmean)/ncirc
do i = 1 , ncirc - 1
  xcirc(i) = xmean + (i)*(deltax)
  ! Equation of a circle | (x+a)^2 + y^2 = r^2
  ycirc(i) = sqrt((radius)**2 - (xcirc(i) - xcenter)**2)
  print*,xcirc(i),ycirc(i)
enddo
print*,xTEnew,yTEnew
!To conserve the total number of points on the curve the (ncirc-1) points' indices behind the TE 
!must be reassigned to the new (ncirc-1) points on the circle.
!TOP curve
xtop(np_side) = xTEnew
ytop(np_side) = yTEnew
do i = np-ncirc+1, np-1
  xtop(i) = xcirc(i+ncirc-np)
  ytop(i) = ycirc(i+ncirc-np)
enddo
!BOT curve
xbot(np_side) = xtop(np_side)
ybot(np_side) = ytop(np_side)
do i = np-ncirc+1, np-1
  xbot(i) =  xcirc(i+ncirc-np)
  ybot(i) = -ycirc(i+ncirc-np)
enddo
deallocate(xcirc,ycirc)
!Rescaling the airfoil so that the chord is unity.
xbot = xbot/xTEnew
xtop = xtop/xTEnew

return
end subroutine
