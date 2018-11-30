subroutine readinput(fname)
! reads the 3dbgbinput file
use globvar
use file_operations
implicit none

character*256 :: fname, temp, temp2, tempr1, fname1, fname2, fname3, beta_switch_2
character(len = :), allocatable :: log_file
integer :: er, temp_int, stat, n_temp, n_temp1, n_temp2, nopen
real*8 inBetaInci, outBetaDevn
real*8, allocatable :: temp_in(:)
real*8              :: temp_offsets(2)
real*8, parameter   :: tol = 1E-8
logical             :: equal, beta_value(5), ang_spl_value(5), file_open

if (allocated(x_le          )) deallocate(x_le          )
if (allocated(x_te          )) deallocate(x_te          )
if (allocated(r_le          )) deallocate(r_le          )
if (allocated(r_te          )) deallocate(r_te          )
if (allocated(in_beta       )) deallocate(in_beta       )
if (allocated(out_beta      )) deallocate(out_beta      )
if (allocated(mrel1         )) deallocate(mrel1         )
if (allocated(chord         )) deallocate(chord         )
if (allocated(thk_c         )) deallocate(thk_c         )
if (allocated(inci          )) deallocate(inci          )
if (allocated(devn          )) deallocate(devn          )
if (allocated(sec_flow_ang  )) deallocate(sec_flow_ang  )
if (allocated(phi_s_in      )) deallocate(phi_s_in      )
if (allocated(phi_s_out     )) deallocate(phi_s_out     )
if (allocated(stagger       )) deallocate(stagger       )
if (allocated(chordm        )) deallocate(chordm        )
if (allocated(msle          )) deallocate(msle          )
if (allocated(s1le          )) deallocate(s1le          )
if (allocated(s2le          )) deallocate(s2le          )
if (allocated(s1te          )) deallocate(s1te          )
if (allocated(s2te          )) deallocate(s2te          )
if (allocated(sang          )) deallocate(sang          )
if (allocated(stk_u         )) deallocate(stk_u         )
if (allocated(stk_v         )) deallocate(stk_v         )
if (allocated(total_camber  )) deallocate(total_camber  )
if (allocated(mprime_ble    )) deallocate(mprime_ble    )
if (allocated(mprime_bte    )) deallocate(mprime_bte    )
if (allocated(BGgrid_all    )) deallocate(BGgrid_all    )
if (allocated(jcellblade_all)) deallocate(jcellblade_all)
if (allocated(etawidth_all  )) deallocate(etawidth_all  )
if (allocated(axchrd        )) deallocate(axchrd        )
allocate (x_le(nspan))
allocate (x_te(nspan))
allocate (r_le(nspan))
allocate (r_te(nspan))
allocate (in_beta(nspan))
allocate (out_beta(nspan))
allocate (mrel1(nspan))
allocate (chord(nspan))
allocate (thk_c(nspan))
allocate (inci(nspan))
allocate (devn(nspan))
allocate (sec_flow_ang(nspan))
allocate (phi_s_in(nspan))
allocate (phi_s_out(nspan))
allocate (stagger(nspan))
allocate (chordm(nspan))
allocate (msle(nspan))
allocate (s1le(nspan))
allocate (s2le(nspan))
allocate (s1te(nspan))
allocate (s2te(nspan))
allocate (sang(nspan))
allocate (stk_u(nspan))
allocate (stk_v(nspan))
allocate (total_camber(nspan))
allocate (mprime_ble(nspan))
allocate (mprime_bte(nspan))
allocate (BGgrid_all(nspan))
allocate (jcellblade_all(nspan))
allocate (etawidth_all(nspan))
allocate(axchrd(nspan))

! constants
abs_zero = 0.0000000000000000

open(1, file = fname, status = 'unknown')
rewind(1)
call log_file_exists(log_file, nopen, file_open)
print*, fname
write(nopen,*) ''
write(nopen,*) fname
call close_log_file(nopen, file_open)
!write(*, *)
!write(*, *) 'Reading inputs from -88dbgbinput file'
!write(*, *)
!---reading parameters from input file----
read(1, *)temp
!reading the casename
read(1, *)fext
!write(*, *)'case:', fext
casename = trim(fext)
read(1, *)temp
read(1, *)ibrow
!write(*, *)'bladerow #:', ibrow
write(ibrowc, '(i3)')ibrow
!print*, ibrowc
!write(*, *)
read(1, *)temp
read(1, *) nbls ! number of blades in this row
!print*, 'Number of blades in this row:', nbls
read(1, '(A)') temp
units = temp(24:25)
read(1, *)scf, temp
temp = adjustl(trim(temp))
read(temp, *, iostat = er) theta_offset
if (er .ne. 0) then
    theta_offset = 0.
    rewind(1)
    do i = 1, 8
        read(1, *) temp
    enddo
endif
!write(*, *)'bsf:', scf
!write(*, *) 
read(1, *)temp
read(1, *)nsl
!print*, 'Number of streamlines:', nsl
!write(*, *)
read(1, *)temp


!
! Input angle switch
! Error trap added -11/21/18 (Mayank Sharma @UC)
!
!---------------------------------------------------------------------------------------------------------------------------------------------
spanwise_angle_spline = .False.
spanwise_inci_dev_spline = .False.

!
! Read the input file angle switch 
! Also read secondary argument for angle spanwise splines if present 
!
read(1, '(A)') beta_switch_2


!
! All possible valid inputs are stored as logical variables in an array
!
beta_value  = [index(beta_switch_2, '0') .ne. 0, index(beta_switch_2, '1') .ne. 0, index(beta_switch_2, '2') .ne. 0, &
               index(beta_switch_2, '3') .ne. 0, index(beta_switch_2, '4') .ne. 0]


!
! Check for all possible valid inputs of the input angle switch
!
!
! Case 1 - All AXIAL angles
if (beta_value(1) .and. .not. beta_value(2) .and. .not. beta_value(3) .and. .not. beta_value(4) .and. .not. beta_value(5)) then
    n_temp1 = index(beta_switch_2, '0')
    read(beta_switch_2(n_temp1:n_temp1 + 1),*,iostat=stat) beta_switch

! Case 2 - All RADIAL angles    
elseif (.not. beta_value(1) .and. beta_value(2) .and. .not. beta_value(3) .and. .not. beta_value(4) .and. .not. beta_value(5)) then
    n_temp1 = index(beta_switch_2, '1')
    read(beta_switch_2(n_temp1:n_temp1 + 1),*,iostat=stat) beta_switch

! Case 3 - AXIAL inlet angles and RADIAL outlet angles
elseif (.not. beta_value(1) .and. .not. beta_value(2) .and. beta_value(3) .and. .not. beta_value(4) .and. .not. beta_value(5)) then
    n_temp1 = index(beta_switch_2, '2')
    read(beta_switch_2(n_temp1:n_temp1 + 1),*,iostat=stat) beta_switch

! Case 4 - RADIAL inlet angles and AXIAL outlet angles
elseif (.not. beta_value(1) .and. .not. beta_value(2) .and. .not. beta_value(3) .and. beta_value(4) .and. .not. beta_value(5)) then
    n_temp1 = index(beta_switch_2, '3')
    read(beta_switch_2(n_temp1:n_temp1 + 1),*,iostat=stat) beta_switch

! Case 5 - Wing flag is turned on
elseif (.not. beta_value(1) .and. .not. beta_value(2) .and. .not. beta_value(3) .and. .not. beta_value(4) .and. beta_value(5)) then
    beta_switch = 0
    wing_flag   = 1

! Case 6 - Invalid input for the input angle switch
!          Warn user and stop execution
elseif (.not. beta_value(1) .and. .not. beta_value(2) .and. .not. beta_value(3) .and. .not. beta_value(4) .and. .not. beta_value(5)) then
    print *, 'FATAL ERROR: Invalid argument for beta_switch'
    print *, 'Valid arguments are 0, 1, 2, 3 and 4 (refer to T-Blade3 documentation)'
    print *, ''
    stop
endif


! 
! Secondary input file angle switch
! Used to determine which angles to fit spanwise splines through
! Error trap added - 11/21/18 (Mayank Sharma @UC)
!
!---------------------------------------------------------------------------------------------------------------------------------------------


!
! All possible valid secondary inputs are stored as logical variables in an array
!
ang_spl_value   = [len(beta_switch_2(:n_temp1)) .eq. len(trim(beta_switch_2)), &
                   index(beta_switch_2, 'inletspline') .ne. 0, index(beta_switch_2, 'outletspline') .ne. 0, &
                   index(beta_switch_2, 'inoutspline') .ne. 0, index(beta_switch_2, 'inci_dev_spline') .ne. 0]


call log_file_exists(log_file, nopen, file_open)

!
! Check for all possible valid inputs of the secondary argument
!
!
! Case 1 - No splining required
if (ang_spl_value(1) .and. .not. ang_spl_value(2) .and. .not. ang_spl_value(3) .and. .not. ang_spl_value(4) .and.     &
    .not. ang_spl_value(5)) then
    read(1,*) temp

! Case 2 - spline inlet angles only   
elseif (.not. ang_spl_value(1) .and. ang_spl_value(2) .and. .not. ang_spl_value(3) .and. .not. ang_spl_value(4) .and. &
        .not. ang_spl_value(5)) then

    n_temp2 = index(beta_switch_2, 'inletspline')
    anglespline = trim(beta_switch_2(n_temp2:))
    spanwise_angle_spline = .true.

    print *, ''
    print *, 'Angles defined spanwise as a B-spline using control points'
    print *, ''
    print *, trim(anglespline)
    write(nopen,*) ''
    write(nopen,*) 'Angles defined spanwise as a B-spline using control points'
    write(nopen,*) ''
    write(nopen,*) trim(anglespline)
    read(1,*) temp

! Case 3 -spline outlet angles only
elseif (.not. ang_spl_value(1) .and. .not. ang_spl_value(2) .and. ang_spl_value(3) .and. .not. ang_spl_value(4) .and. &
        .not. ang_spl_value(5)) then

    n_temp2 = index(beta_switch_2, 'outletspline')
    anglespline = trim(beta_switch_2(n_temp2:))
    spanwise_angle_spline = .true.

    print *, ''
    print *, 'Angles defined spanwise as a B-spline using control points'
    print *, ''
    print *, trim(anglespline)
    write(nopen,*) ''
    write(nopen,*) 'Angles defined spanwise as a B-spline using control points'
    write(nopen,*) ''
    write(nopen,*) trim(anglespline)
    read(1,*) temp

! Case 4 - spline inlet and outlet angles
elseif (.not. ang_spl_value(1) .and. .not. ang_spl_value(2) .and. .not. ang_spl_value(3) .and. ang_spl_value(4) .and. &
        .not. ang_spl_value(5)) then

    n_temp2 = index(beta_switch_2, 'inoutspline')
    anglespline = trim(beta_switch_2(n_temp2:))
    spanwise_angle_spline = .true.

    print *, ''
    print *, 'Angles defined spanwise as a B-spline using control points'
    print *, ''
    print *, trim(anglespline)
    write(nopen,*) ''
    write(nopen,*) 'Angles defined spanwise as a B-spline using control points'
    write(nopen,*) ''
    write(nopen,*) trim(anglespline)
    read(1,*) temp

! Case 5 - spline incidence and deviation
elseif (.not. ang_spl_value(1) .and. .not. ang_spl_value(2) .and. .not. ang_spl_value(3) .and. .not. ang_spl_value(4) &
        .and. ang_spl_value(5)) then

    n_temp2 = index(beta_switch_2, 'inci_dev_spline')
    anglespline = trim(beta_switch_2(n_temp2:))
    spanwise_inci_dev_spline = .true.

    print *, ''
    print *, 'Incidence and Deviation defined spanwise as a B-spline using control points'
    print *, ''
    print *, trim(anglespline)
    write(nopen,*) ''
    write(nopen,*) 'Incidence and Deviation defined spanwise as a B-spline using control points'
    write(nopen,*) ''
    write(nopen,*) trim(anglespline)
    read(1,*) temp

! Case 6 - invalid input
!          warn user and stop execution
elseif (.not. ang_spl_value(1) .and. .not. ang_spl_value(2) .and. .not. ang_spl_value(3) .and. .not. ang_spl_value(4) &
        .and. .not. ang_spl_value(5)) then
    print *, ''
    print *, 'FATAL ERROR: Invalid argument for anglespline'
    print *, 'anglespline can be left blank (refer to T-Blade3 documentation)'
    print *, 'Valid arguments are "inletspline", "outletspline", "inoutspline" or "inci_dev_spline"'
    print *, ''
    stop
end if


call close_log_file(nopen, file_open)





! 
! Curvature control switch
! Error trap added - 11/21/18 (Mayank Sharma @UC)
!
!---------------------------------------------------------------------------------------------------------------------------------------------
read(1,*)curv, spanwise_spline              


! Invalid input for the camber definition switch
! Warn user and stop execution 
if (curv .ne. 0 .and. curv .ne. 1) then

    print *, ''
    print *, 'FATAL ERROR: Invalid argument for camber definition switch'
    print *, 'Valid arguments are 0 or 1 (refer to T-Blade3 documentation)'
    print *, ''
    stop

end if


! Invalid input for curvature control switch
! Warn user and stop execution
if (trim(spanwise_spline) .ne. 'spanwise_spline' .and. trim(spanwise_spline) .ne. 'Airfoil') then

    print *, ''
    print *, 'FATAL ERROR: Invalid argument for camber definition switch'
    print *, 'Valid argument for using spancontrolinputs is "spanwise_spline" (refer to T-Blade3 documentation)'
    print *, ''
    stop

end if 

! Read next line in the input file if spanwise_spline has been specified
if (trim(spanwise_spline).eq.'spanwise_spline')then
    read(1,*)temp
endif







! 
! Thickness distribution switch
! Error trap added - 11/21/18 (Mayank Sharma @UC)
!
!---------------------------------------------------------------------------------------------------------------------------------------------
read(1, *)thick_distr, temp2  

! Invalid input for the thickness distribution switch
! Warn user and stop execution
if (thick_distr .ne. 0 .and. thick_distr .ne. 1 .and. thick_distr .ne. 2 .and. thick_distr .ne. 3 .and. thick_distr .ne. 4) then

    print *, ''
    print *, 'FATAL ERROR: Invalid argument for thickness distribution switch'
    print *, 'Valid arguments are 0, 1, 2, 3, or 4 (refer to T-Blade3 documentation)'
    print *, ''
    stop

end if

! Read next line in the input file
if (len(trim(adjustl(temp2))) .eq. 3) then
    thick_distr_3_flag = trim(adjustl(temp2))
    read(1, *)temp
endif






!
! Thickness multiplier switch
! Error trap added - 11/21/18 (Mayank Sharma @UC)
!
!---------------------------------------------------------------------------------------------------------------------------------------------
read(1, *)thick       

! Invalid input for the thickness multiplier switch
! Warn user and stop execution
if (thick .ne. 0 .and. thick .ne. 1) then

    print *, ''
    print *, 'FATAL ERROR: Invalid argument for thickness multiplier switch'
    print *, 'Valid arguments are 0 or 1 (refer to T-Blade3 documentation)'
    print *, ''
    stop

end if






!
! LE spline control switch
! Error trap added - 11/21/18 (Mayank Sharma @UC)
!
!---------------------------------------------------------------------------------------------------------------------------------------------
! Read next line
read(1, *)temp
read(1, *)LE         

! Invalid input for the LE spline control switch
! Warn user and stop execution
if (LE .ne. 0 .and. LE .ne. 1) then

    print *, ''
    print *, 'FATAL ERROR: Invalid argument for LE spline control switch'
    print *, 'Valid arguments are 0 or 1 (refer to T-Blade3 documentation)'
    print *, ''
    stop

end if





 
!
! Non-dimensional actual chord control switch
! Error trap added - 11/21/18 (Mayank Sharma @UC)
!
!---------------------------------------------------------------------------------------------------------------------------------------------
! Read next line
read(1, *)temp
read(1, *)chord_switch ! non-dimensional actual chord switch

! Invalid input for the non-dimensional actual chord switch
! Warn user and stop execution
if (chord_switch .ne. 0 .and. chord_switch .ne. 1 .and. chord_switch .ne. 2) then

    print *, ''
    print *, 'FATAL ERROR: Invalid arguments for non-dimensional actual chord switch'
    print *, 'Valid arguments are 0, 1 or 2 (refer to T-Blade3 documentation)'
    print *, ''
    stop

end if






!
! True lean and sweep switch
! Error trap added - 11/21/18 (Mayank Sharma @UC)
!
!---------------------------------------------------------------------------------------------------------------------------------------------
! Read next line
read(1, *)temp
read(1, *)leansweep_switch 

! If there is an invalid input for the true lean and sweep switch
! Warn user and stop execution
if (leansweep_switch .eq. 0) then
    trueleansweep = ''
else if (leansweep_switch .eq. 1) then
    trueleansweep = '1'
else
    print *, ''
    print *, 'FATAL ERROR: Invalid argument for leansweep_switch'
    print *, 'Valid arguments are 0 or 1 (refer to the T-Blade3 documentation)'
    print *, ''
    stop
end if






!
! Clustering distribution switch
! Error trap added - 11/21/18 (Mayank Sharma @UC)
!
!---------------------------------------------------------------------------------------------------------------------------------------------
! Read next line
read(1, *)temp
read(1, *)clustering_switch, clustering_parameter

! Invalid input for the clustering distribution switch
! Warn user and stop execution
if (clustering_switch .ne. 0 .and. clustering_switch .ne. 1 .and. clustering_switch .ne. 2 .and. &
    clustering_switch .ne. 3 .and. clustering_switch .ne. 4) then

    print *, ''
    print *, 'FATAL ERROR: Invalid argument for clustering_switch'
    print *, 'Valid arguments are 0, 1, 2 or 3 (refer to the T-Blade3 documentation)'
    print *, ''
    stop

end if

! Read next lines
read(1, *)temp
read(1, *)temp
!
!---- blade file names
do i = 1, nsl
    write(ibrowc1, '(i3)')i
    blext(i) = trim(adjustl(ibrowc1))//'.'//trim(adjustl(ibrowc))//'.'//fext
enddo
nspn = nsl
!print*, '   in_betaZ*    out_betaZ*'
do js = 1, nspn
    if (spanwise_angle_spline)then   ! Not reading it here since it is splined spanwise
        read(1, *, end = 35)tempr, tempr, tempr, &
        mrel1(js), chord(js), thk_c(js), inci(js), devn(js), sec_flow_ang(js)
    elseif (spanwise_inci_dev_spline) then 
        !reading inlet & outlet angles from table but not adding incidence and deviation from the table
        read(1, *, end = 35)tempr, in_beta(js), out_beta(js), mrel1(js), chord(js), thk_c(js), inci(js), devn(js), sec_flow_ang(js)
    else ! Reading the inlet and outlet angles from this table
        read(1, *, end = 35)tempr, in_beta(js), out_beta(js), mrel1(js), chord(js), thk_c(js), inci(js), devn(js), sec_flow_ang(js)
        !Adding incidence and deviation angles-------------------3/10/11
        !print*, in_beta(js), out_beta(js)   
    endif
enddo



if (.not.spanwise_angle_spline .and. .not.spanwise_inci_dev_spline) then
   do js = 1, nspn
      in_beta( js) =  inBetaInci(in_beta(js),               inci(js))
      out_beta(js) = outBetaDevn(in_beta(js), out_beta(js), devn(js))
   enddo
endif

!write(*, *)
! Reading the LE/TE curve definition---------
read(1, *)temp
read(1, *)temp 
read(1, *)npoints
read(1, *)temp
! write(*, *)'LE/TE defined by a curve with no. of points as:', npoints
! write(*, *)'xLE    rLE     xTE     rTE'
do i = 1, npoints
    read(1, *)xle(i), rle(i), xte(i), rte(i)
enddo
read(1, *)temp
read(1, *)temp
read(1, *)temp
read(1, *)temp
read(1, *)temp
read(1, *)stack_switch
read(1, *)temp
if (allocated(umxthk_all)) deallocate(umxthk_all)
Allocate(umxthk_all(nsl))
if (LE.ne.0) then
    do js = 1, nspn
        read(1, *)tempr, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), tempr, tempr,  &
                  jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
        !print*, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
    enddo
elseif (LE == 0) then
        if (allocated(lethk_all)) deallocate(lethk_all)
        if (allocated(tethk_all)) deallocate(tethk_all)
    Allocate(lethk_all(nsl))
    Allocate(tethk_all(nsl))
    do js = 1, nspn
        read(1, *)tempr, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), lethk_all(js), &
                  tethk_all(js), jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
        ! print*, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), lethk_all(js), tethk_all(js), jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
    enddo
endif
read(1, *)temp
read(1, *)cpbsv, bsv1, bsv2
read(1, *)temp
do i = 1, cpbsv
    read(1, *)spanbsv(i), bf1(i), bf2(i)
enddo
read(1, *)temp
read(1, *)stack! Reading the stacking value



!
! Read sweep spline control points and call ESP override subroutine 
!
read(1, *)temp
read(1, '(A)')temp 
read(temp(12:12), *)cpdeltam
if(trim(trueleansweep).ne.'')then
    chrdsweep = 1
    read(1, *)temp
    do i = 1, cpdeltam
        read(1, *)spanmp(i), xcpdelm(i)
    enddo
else
    read(1, *)temp
    do i = 1, cpdeltam
        read(1, *)spanmp(i), xcpdelm(i)
    enddo
endif

if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cpdeltam))
temp_in = spanmp(1:cpdeltam)
call override_span_del_m_ctrl(cpdeltam, temp_in)
spanmp(1:cpdeltam) = temp_in

if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cpdeltam))
temp_in = xcpdelm(1:cpdeltam)
call override_span_del_m(cpdeltam, temp_in)
xcpdelm(1:cpdeltam) = temp_in



!
! Read lean spline control points and call ESP override subroutine
!
read(1, *)temp
read(1, '(A)')temp 
read(temp(12:12), *)cpdeltheta
if(trim(trueleansweep).ne.'')then
    chrdlean = 1
    read(1, *)temp
    do i = 1, cpdeltheta
        read(1, *)spantheta(i), xcpdeltheta(i)
    enddo
else
    read(1, *)temp
    do i = 1, cpdeltheta
        read(1, *)spantheta(i), xcpdeltheta(i)
    enddo
endif

if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cpdeltheta))
temp_in = spantheta(1:cpdeltheta)
call override_span_del_theta_ctrl(cpdeltheta, temp_in)
spantheta(1:cpdeltheta) = temp_in
 
if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cpdeltheta))
temp_in = xcpdeltheta(1:cpdeltheta) 
call override_span_del_theta(cpdeltheta, temp_in)
xcpdeltheta(1:cpdeltheta) = temp_in



!
! Read inBeta* spline control points and call ESP override subroutine
! inBeta* can be used to spline either inlet flow angle or incidence
!
read(1, *)temp
read(1, *)cpinbeta 
read(1, *)temp
do i = 1, cpinbeta
    read(1, *)spaninbeta(i), xcpinbeta(i)
enddo

if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cpinbeta))
temp_in = spaninbeta(1:cpinbeta)
call override_span_in_beta_ctrl(cpinbeta, temp_in)
spaninbeta(1:cpinbeta) = temp_in

if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cpinbeta))
temp_in = xcpinbeta(1:cpinbeta)
call override_span_in_beta(cpinbeta, temp_in)
xcpinbeta(1:cpinbeta) = temp_in



!
! Read outBeta* spline control points and call ESP override subroutine
! outBeta* can be used to spline either exit flow angle or deviation
!
read(1, *)temp
read(1, *)cpoutbeta
read(1, *)temp
do i = 1, cpoutbeta
    read(1, *)spanoutbeta(i), xcpoutbeta(i)
enddo

if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cpoutbeta))
temp_in = spanoutbeta(1:cpoutbeta)
call override_span_out_beta_ctrl(cpoutbeta, temp_in)
spanoutbeta(1:cpoutbeta) = temp_in

if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cpoutbeta))
temp_in = xcpoutbeta(1:cpoutbeta)
call override_span_out_beta(cpoutbeta, temp_in)
xcpoutbeta(1:cpoutbeta) = temp_in



!
! Read chord multiplier spline control points and call ESP override subroutine
! TODO: chord_multiplier > 1?
!
read(1, *)temp
read(1, *)cpchord
read(1, *)temp
do i = 1, cpchord
    read(1, *)spanchord(i), xcpchord(i)
    xcpchord(i) = xcpchord(i) + 1.0
enddo

if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cpchord))
temp_in = spanchord(1:cpchord)
call override_span_chord_ctrl(cpchord, temp_in)
spanchord(1:cpchord) = temp_in

if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cpchord))
temp_in = xcpchord(1:cpchord)
call override_span_chord(cpchord, temp_in)
do i = 1,cpchord
    equal = (abs(xcpchord(i) - temp_in(i)) .le. tol)
    if (.not. equal) exit
end do
if (.not. equal) then
    xcpchord(1:cpchord) = temp_in + 1
else
    xcpchord(1:cpchord) = temp_in
end if



! 
! Read tm/c spline control points and call ESP override subroutine
! TODO: tm/c > 1?
!
read(1, *)temp
read(1, *)cptm_c ! control points for tm/c
read(1, *)temp

!next line to always use the thickness tm/c as it is a multiplier (default = 1):
if ((thick_distr .ne. 0) .and. .not. is2d) then
    tm_c_spline = .True.
else
    tm_c_spline = .False.
endif    

do i = 1, cptm_c
    read(1, *)spantm_c(i), xcptm_c(i)
    xcptm_c(i) = xcptm_c(i) + 1.0
enddo

if (allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cptm_c))
temp_in = spantm_c(1:cptm_c)
call override_span_thk_c_ctrl(cptm_c, temp_in)
spantm_c(1:cptm_c) = temp_in

if(allocated(temp_in)) deallocate(temp_in)
allocate(temp_in(cptm_c))
temp_in = xcptm_c(1:cptm_c)
call override_span_thk_c(cptm_c, temp_in)
do i = 1,cptm_c
    equal = (abs(xcptm_c(i) - temp_in(i)) .le. tol)
    if (.not. equal) exit
end do
if (.not. equal) then
    xcptm_c(1:cptm_c) = temp_in + 1
else
    xcptm_c(1:cptm_c) = temp_in
end if



! 
! Read hub and tip offsets and call ESP override subroutine
! TODO: Negative hub offset?
!
read(1, *)temp 
read(1, *)hub
read(1, *)temp
read(1, *)tip

temp_offsets(1) = hub
temp_offsets(2) = tip
call override_offsets(temp_offsets)
hub = temp_offsets(1)
tip = temp_offsets(2)



!
! Read streamline data from input file
!
read(1, *)temp
do while(temp.ne.'x_s')
    read(1, *)temp
enddo

!
! Calculating m prime coordinates
! Using x, r coordinates as the input for streamlines
!
do ia = 1, nsl
    nsp(ia) = 0
    do while(.true.)
        read(1, *)trarray(1), trarray(2)
        !print*, trarray(1), trarray(2)
        if(trarray(2).ne.0)then
            nsp(ia) = nsp(ia) + 1
            xm(nsp(ia), ia) = trarray(1)
            rm(nsp(ia), ia) = trarray(2)
            !print*, ia, nsl
            ! print*, xm(nsp(ia), ia), rm(nsp(ia), ia)
            else
            exit
        endif
    enddo
enddo



! Close input file
35 close(1)

return


end subroutine readinput
!**********************************************************************************
!**********************************************************************************



!**********************************************************************************
!**********************************************************************************
! subroutine readcontrolinput(fname5)
subroutine readcontrolinput(row_type, path)
!**********************************************************************************
! reads the controlinput.dat when the appropriate swittches are activated.
use globvar
use file_operations
implicit none

! character*(*) fname5
character*(*) path
character*256 temp, fname4, row_type
character*256 fname5
character(len = :), allocatable :: log_file
integer :: phantom_n, nopen
logical :: file_open

!if(curv.ne.0.or.thick.ne.0.or.LE.ne.0.or.thick_distr.ne.0)then
print*
call log_file_exists(log_file, nopen, file_open)
write(nopen,*) ''
call close_log_file(nopen, file_open)
!print*, 'using the controlinput file ....'
! Reading the input file of curv, thk, and LE for the bladegen:
fname5 = trim(path)//'controlinputs.'//trim(row_type)//'.dat'
! print*, fname5
open(11, file = fname5)
rewind(11)
!----------------------------------------------------------------------
! Reading curvature:
!----------------------------------------------------------------------
if (allocated(ncp_curv)) deallocate(ncp_curv)
if (allocated(curv_cp )) deallocate(curv_cp )
Allocate(ncp_curv(nsl))
Allocate(curv_cp(20, 2*nsl))
read (11, *) temp   ! the test case name (added 6 23 2013)
read (11, *) temp
current = 0
do i = 1, nsl
    read (11, *) temp
    read (11, *) temp
    read (11, *) ncp_curv(i)
    ncp_curv(i) = ncp_curv(i) + 2
        if (allocated(xcp)) deallocate(xcp)
        if (allocated(ycp)) deallocate(ycp)
    Allocate(xcp(ncp_curv(i)))
    Allocate(ycp(ncp_curv(i)))
    !print*, 'xcp = ', xcp
    !print*, 'ycp = ', ycp 
    current = i 
    !print*, 'current', current
    read (11, *) temp
    write(radialsec, *)current ! write out control points to a file to plot. Kiran 8/9/13
    ! reading the control points: 
    do j = 1, (ncp_curv(i)-2)
        read(11, *) xcp(j+1), ycp(j+1)
    enddo

    if(isdev) then
        fname4 = 'curvature_ctrl_pts.'//trim(adjustl(radialsec))//'.'//trim(casename)//'.txt'
        open(12, file = fname4)
        write(12, *)trim(casename)
        write(12, *)'Curvature Control points for camber'
        write(12, *)'u      v '   
        do j = 1, (ncp_curv(i)-2)
            write(12, *)xcp(j+1), ycp(j+1)
            !print*, xcp(j+3), ycp(j+3)
        enddo
        close(12)
    endif ! endif for developers

    ! Fixed control points (leading and trailing edges)
    xcp(1) = 2*xcp(2)-xcp(3)
    ycp(1) = 2*ycp(2)-ycp(3)
    xcp(ncp_curv(i)) = 2*xcp(ncp_curv(i)-1)-xcp(ncp_curv(i)-2)
    ycp(ncp_curv(i)) = 2*ycp(ncp_curv(i)-1)-ycp(ncp_curv(i)-2) 
    do k = 1, ncp_curv(i)
        curv_cp(k, 2*i-1) = xcp(k)
        curv_cp(k, 2*i) = ycp(k)
    enddo
    deallocate(xcp)
    deallocate(ycp)
enddo

!----------------------------------------------------------------------
! Reading Thickness
!----------------------------------------------------------------------
if (allocated(ncp_thk)) deallocate(ncp_thk)
if (allocated(thk_cp )) deallocate(thk_cp )
Allocate(ncp_thk(nsl))
Allocate(thk_cp(20, 2*nsl))
if (thick_distr.eq.4) then
    phantom_n = 0
elseif (thick_distr.eq.3) then
    phantom_n = 2
else
    phantom_n = 4
endif
read (11, *) temp
do i = 1, nsl
    read (11, *) temp
    read (11, *) temp
    read (11, *) ncp_thk(i)
    ncp_thk(i) = ncp_thk(i) + phantom_n
    if (allocated(xcp)) deallocate(xcp)
    if (allocated(ycp)) deallocate(ycp)
    Allocate(xcp(ncp_thk(i)))
    Allocate(ycp(ncp_thk(i)))
    read (11, *) temp
    ! reading the control points: 
    !do j = 1, (ncp_thk(i)-2)
    do j = 1, (ncp_thk(i)-phantom_n)
        !read(11, *), xcp(j+1), ycp(j+1)  ! for cubic spline
        read(11, *) xcp(j+(phantom_n/2)), ycp(j+(phantom_n/2))  ! for quartic spline
        !print*, xcp(j+1), ycp(j+1)
    enddo
    if (thick_distr.eq.4) then
    elseif (thick_distr.eq.3) then
        xcp(1) = 0.
        ycp(1) = 0.
        xcp(ncp_thk(i)) = 0.
        ycp(ncp_thk(i)) = 0.
    else
        ! Fixed control points (leading and trailing edges)
        ! xcp(1) = 2*xcp(2)-xcp(3)	!phantom control points cubic spline
        ! ycp(1) = 2*ycp(2)-ycp(3)
        ! xcp(ncp_thk(i)) = 2*xcp(ncp_thk(i)-1)-xcp(ncp_thk(i)-2) 	
        ! ycp(ncp_thk(i)) = 2*ycp(ncp_thk(i)-1)-ycp(ncp_thk(i)-2)
        ! 2 phantom points when dealing with quartic bspline:	 
        xcp(1) = 2*xcp(3)-xcp(5)    !phantom control points quartic spline
        ycp(1) = 2*ycp(3)-ycp(5)
        xcp(2) = 2*xcp(3)-xcp(4)    !phantom control points quartic spline
        ycp(2) = 2*ycp(3)-ycp(4)
        xcp(ncp_thk(i)) = 2*xcp(ncp_thk(i)-2)-xcp(ncp_thk(i)-4) 
        ycp(ncp_thk(i)) = 2*ycp(ncp_thk(i)-2)-ycp(ncp_thk(i)-4)
        xcp(ncp_thk(i)-1) = 2*xcp(ncp_thk(i)-2)-xcp(ncp_thk(i)-3)
        ycp(ncp_thk(i)-1) = 2*ycp(ncp_thk(i)-2)-ycp(ncp_thk(i)-3)
    endif
    do k = 1, ncp_thk(i)
        thk_cp(k, 2*i-1) = xcp(k)
        thk_cp(k, 2*i) = ycp(k)
    enddo
enddo

!----------------------------------------------------------------------
! Reading Leading edge parameters:
!----------------------------------------------------------------------
if (allocated(sting_l_all)) deallocate(sting_l_all)
Allocate(sting_l_all(nsl))
if(LE .ne.0) then
    if (allocated(lethk_all        )) deallocate(lethk_all        )
    if (allocated(tethk_all        )) deallocate(tethk_all        )
    if (allocated(s_all            )) deallocate(s_all            )
    if (allocated(ee_all           )) deallocate(ee_all           )
    if (allocated(C_le_x_top_all   )) deallocate(C_le_x_top_all   )
    if (allocated(C_le_x_bot_all   )) deallocate(C_le_x_bot_all   )
    if (allocated(C_le_y_top_all   )) deallocate(C_le_y_top_all   )
    if (allocated(C_le_y_bot_all   )) deallocate(C_le_y_bot_all   )
    if (allocated(LE_vertex_ang_all)) deallocate(LE_vertex_ang_all)
    if (allocated(LE_vertex_dis_all)) deallocate(LE_vertex_dis_all)
    if (allocated(sting_h_all      )) deallocate(sting_h_all      )
    Allocate(lethk_all(nsl))
    Allocate(tethk_all(nsl))
    Allocate(s_all(nsl))
    Allocate(ee_all(nsl))
    Allocate(C_le_x_top_all(nsl))
    Allocate(C_le_x_bot_all(nsl))
    Allocate(C_le_y_top_all(nsl))
    Allocate(C_le_y_bot_all(nsl))
    Allocate(LE_vertex_ang_all(nsl))
    Allocate(LE_vertex_dis_all(nsl))
    !Allocate(sting_l_all(nsl))
    Allocate(sting_h_all(nsl, 2))
    ! 5 ... is the number of parameters to read
    read (11, *) temp
    read (11, *) temp
    read (11, *) LEdegree, no_LE_segments
    call log_file_exists(log_file, nopen, file_open)
    print*, 'LEdegree = ', LEdegree, 'no_LE_segments = ', no_LE_segments
    write(nopen,*) 'LEdegree = ', LEdegree, 'no_LE_segments = ', no_LE_segments
    call close_log_file(nopen, file_open)
    do i = 1, nsl
        read (11, *) temp
        read (11, *) temp
        read (11, *) lethk_all(i), tethk_all(i), s_all(i), ee_all(i), C_le_x_top_all(i), C_le_x_bot_all(i), &
        C_le_y_top_all(i), C_le_y_bot_all(i), LE_vertex_ang_all(i), LE_vertex_dis_all(i), &
        sting_l_all(i), sting_h_all(i, 1), sting_h_all(i, 2)
        ! print*, 'C_le_y_top_all(i), C_le_y_bot_all(i), LE_vertex_ang_all(i), LE_vertex_dis_all(i)', C_le_y_top_all(i), C_le_y_bot_all(i), LE_vertex_ang_all(i), LE_vertex_dis_all(i)
    enddo
    if (thick_distr .eq. 4) then
        if (allocated(te_angle_cp)) deallocate(te_angle_cp)
        Allocate(te_angle_cp(nsl))
        read (11, *) temp
        read (11, *) te_flag
        read (11, *) temp
        do i = 1, nsl
            read(11, *) te_angle_cp(i)
        enddo
        call log_file_exists(log_file, nopen, file_open)
        print*, 'TE Angle'
        write(*, '(F10.5)') (te_angle_cp(i), i = 1, nsl)
        write(nopen,*) 'TE Angle'
        write(nopen, '(F10.5)') (te_angle_cp(i), i = 1, nsl)
        call close_log_file(nopen, file_open)
    endif
elseif (thick_distr .eq. 4) then ! end if for LE spline parameters
    if (allocated(te_angle_cp)) deallocate(te_angle_cp)
    Allocate(te_angle_cp(nsl))
    read (11, *) temp
    read (11, *) te_flag
    read (11, *) temp
    do i = 1, nsl
        read(11, *) te_angle_cp(i)
    enddo
    call log_file_exists(log_file, nopen, file_open)
    print*, 'TE Angle'
    write(*, '(F10.5)') (te_angle_cp(i), i = 1, nsl)
    write(nopen,*) 'TE Angle'
    write(nopen, '(F10.5)') (te_angle_cp(i), i = 1, nsl)
    call close_log_file(nopen, file_open)
endif
close(11)
!endif ! end if for curvature, thickness, thickness dist and LE spline definition.

return
end subroutine readcontrolinput
!**********************************************************************************
!**********************************************************************************



!**********************************************************************************
!**********************************************************************************
! subroutine read_spanwise_input(file_name)
subroutine read_spanwise_input(row_type, path)
!Description(Syed Moez 03/02/2014):-------------------------------------------------------------------------
!This subroutine is used to used to read the input file called
!spancontrolinputs. It is triggered if any of the switches for LE, 
!thickness or curvature is activated and the word "spline" is typed
!after the curvature switch.
use globvar
use file_operations
implicit none
character*256 row_type
! character*(*) file_name
character*(*) path
character*256 file_name
character(len = :), allocatable   :: log_file
!opening files to read inputs
real :: span_dum
real*8, allocatable, dimension(:) :: temp, temp_exact
integer                           :: temp_thk_flag(3)
integer jj
integer     :: i_local, nopen
logical     :: file_open

file_name = trim(path)//'spancontrolinputs.'//trim(row_type)//'.dat'
open(10, file = file_name)
rewind(10)

do i = 1, 5
    read(10, *)
end do

!--------------------------------------------------------------------------
!Reading curvature and chord control points
read(10, *) ncp_span_curv, ncp_chord
if(control_inp_flag .eq. 1 .and. ncp_span_curv .ne. nsl) then
    print*, 'ERROR: In auxiliary file inputs, number of spanwise curvature specifications &
             must equal number of streamlines if spanwise spline is not used.'
    stop
endif
!number of chord and curvature control points will always be the same
ncp_curvature = ncp_chord
!Initializing values for variables defined by Ahmed
if (allocated(ncp_curv)) deallocate(ncp_curv)
if (allocated(curv_cp )) deallocate(curv_cp )
! If either one of curv, thick, LE ot thickn_distr are not zero:
Allocate(ncp_curv(nsl))
if(control_inp_flag .eq. 1) then
    Allocate(curv_cp(20, 2*nsl))
endif
do i = 1, nsl
    ncp_curv(i) = ncp_curvature + 2
enddo

!Including phantom points												
ncp_span_curv1 = ncp_span_curv+2

if (isold) then
    ncp_chord_curv = ncp_chord-2+ncp_curvature+1-1
else
    ncp_chord_curv = ncp_chord-2+ncp_curvature+1
endif
if (allocated(cp_chord_curv)) deallocate(cp_chord_curv)
allocate(cp_chord_curv(ncp_span_curv, ncp_chord_curv))
!LINE 7
read(10, *)
do i = 1, ncp_span_curv
    read(10, *) cp_chord_curv(i, 1:ncp_chord_curv)
    if(control_inp_flag .eq. 1) then
        if (allocated(xcp)) deallocate(xcp)
        if (allocated(ycp)) deallocate(ycp)
        Allocate(xcp(ncp_curv(i)))
        Allocate(ycp(ncp_curv(i)))
        ! read(10, *) span_dum, xcp(3:ncp_curv(i)-2), ycp(2:ncp_curv(i)-1)
        span_dum = cp_chord_curv(i, 1)
        xcp(3:ncp_curv(i)-2) = cp_chord_curv(i, 2:ncp_chord-1)
        ycp(2:ncp_curv(i)-1) = cp_chord_curv(i, ncp_chord:ncp_chord_curv)
        xcp(1) = 2*xcp(2)-xcp(3)
        xcp(2) = 0.
        ycp(1) = 2*ycp(2)-ycp(3)
        xcp(ncp_curv(i)-1) = 1.
        xcp(ncp_curv(i)) = 2*xcp(ncp_curv(i)-1)-xcp(ncp_curv(i)-2)
        ycp(ncp_curv(i)) = 2*ycp(ncp_curv(i)-1)-ycp(ncp_curv(i)-2)
        do k = 1, ncp_curv(i)
            curv_cp(k, 2*i-1) = xcp(k)
            curv_cp(k, 2*i) = ycp(k)
        enddo
    endif
enddo

if(control_inp_flag .eq. 2) then
if (allocated(temp)) deallocate(temp)
allocate(temp(ncp_span_curv))
jj = 1
do i = 1, ncp_span_curv
   temp(i) = cp_chord_curv(i,jj)
enddo
call override_span_curv_ctrl(ncp_span_curv, temp)
do i = 1, ncp_span_curv
   cp_chord_curv(i,jj) = temp(i)
enddo

! Variable override subroutine calls for ESP integration
 if (isold .eqv. .false.) then
    jj = 1 + ncp_chord-2 + 1
    do i = 1,ncp_span_curv
        temp(i) = cp_chord_curv(i,jj)
    end do
    call override_cur1(ncp_span_curv, temp)
    do i = 1,ncp_span_curv
        cp_chord_curv(i,jj) = temp(i)
    end do
 end if
     
 if (ncp_curvature >= 1) then
    if (isold) then 
        jj = 1 + ncp_chord-2 + 2-1
    else
        jj = 1 + ncp_chord-2 + 2
    end if
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_cur2(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

 if (ncp_curvature >= 2) then
    if (isold) then
        jj = 1 + ncp_chord-2 + 3-1
    else
        jj = 1 + ncp_chord-2 + 3
    end if
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_cur3(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

 if (ncp_curvature >= 3) then
    if (isold) then
        jj = 1 + ncp_chord-2 + 4-1
    else
        jj = 1 + ncp_chord-2 + 4
    end if
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_cur4(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

 if (ncp_curvature >= 4) then
    if (isold) then
        jj = 1 + ncp_chord-2 + 5-1
    else
        jj = 1 + ncp_chord-2 + 5
    end if
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_cur5(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

 if (ncp_curvature >= 5) then
    if (isold) then
        jj = 1 + ncp_chord-2 + 6-1
    else
        jj = 1 + ncp_chord-2 + 6
    end if
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_cur6(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

 if (ncp_curvature >= 6) then
    if (isold) then
        jj = 1 + ncp_chord-2 + 7-1
    else
        jj = 1 + ncp_chord-2 + 7
    end if
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_cur7(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

 if (ncp_chord >= 1) then
    jj = 1 + 2-1
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_u2(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

 if (ncp_chord >= 2) then
    jj = 1 + 3-1
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_u3(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

 if (ncp_chord >= 3) then
    jj = 1 + 4-1
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_u4(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

 if (ncp_chord >= 4) then
    jj = 1 +5-1
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_u5(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

 if (ncp_chord >= 5) then
    jj = 1 +6-1
    do i = 1, ncp_span_curv
       temp(i) = cp_chord_curv(i,jj)
    enddo
    call override_u6(ncp_span_curv, temp)
    do i = 1, ncp_span_curv
       cp_chord_curv(i,jj) = temp(i)
    enddo
 endif

!deallocate(temp)
endif

if(thick .ne. 0 .or. LE .ne. 0 .or. thick_distr .eq. 3  .or. thick_distr .eq. 4) then
    !--------------------------------------------------------------------------
    !Reading thickness control points
    read(10, *)
    read(10, *)
    if (thick_distr .eq. 4) then
        read(10, *) ncp_span_thk, ncp_chord_thickness, te_flag, le_opt_flag, te_opt_flag
        print*, 'TE flag:', te_flag
        print*, 'LE optimization flag:', le_opt_flag
        print*, 'TE optimization flag:', te_opt_flag
        call log_file_exists(log_file, nopen,  file_open)
        write(nopen,*) 'TE flag:', te_flag
        write(nopen,*) 'LE optimization flag:', le_opt_flag
        write(nopen,*) 'TE optimization flag:', te_opt_flag
        call close_log_file(nopen, file_open)
    else
        read(10, *)ncp_span_thk, ncp_chord_thickness
    endif
    if(control_inp_flag .eq. 1 .and. ncp_span_thk .ne. nsl) then
        print*, 'FATAL ERROR: In auxiliary file inputs, number of spanwise thickness specifications &
                 must equal number of streamlines if spanwise spline is not used.'
        stop
    endif
    !number of chord and thickness control points will always be the same
    ncp_thickness = ncp_chord_thickness
    ncp_span_thk1 = ncp_span_thk+2

    ncp_chord_thk = ncp_chord_thickness-2+ncp_thickness-2+1
    if (thick_distr .eq. 3) ncp_chord_thk = ncp_chord_thickness-2+ncp_thickness+2+1
    if (thick_distr .eq. 4) ncp_chord_thk = ncp_chord_thickness+ncp_thickness+1

    !Initializing values for variables defined by Ahmed
    if (allocated(ncp_thk)) deallocate(ncp_thk)
    if (allocated(thk_cp )) deallocate(thk_cp )
    Allocate(ncp_thk(nsl))
    Allocate(thk_cp(20, 2*nsl))
    if (thick_distr .eq. 3) then
        do i = 1, nsl
            ncp_thk(i) = ncp_thickness+4
        enddo
    elseif (thick_distr .eq. 4) then
        do i = 1, nsl
            ncp_thk(i) = ncp_thickness
        enddo
    else
        do i = 1, nsl
            ncp_thk(i) = ncp_thickness+4
        enddo
    endif

    if (allocated(cp_chord_thk)) deallocate(cp_chord_thk)
    allocate(cp_chord_thk(ncp_span_thk, ncp_chord_thk))

    if (allocated(le_angle_cp)) deallocate(le_angle_cp)
    Allocate(le_angle_cp(ncp_span_thk))
    if (allocated(te_angle_cp)) deallocate(te_angle_cp)
    Allocate(te_angle_cp(ncp_span_thk))

    read(10, *)
    do i = 1, ncp_span_thk
        if (allocated(xcp)) deallocate(xcp)
        if (allocated(ycp)) deallocate(ycp)
        Allocate(xcp(ncp_thk(i)))
        Allocate(ycp(ncp_thk(i)))
        if (thick_distr .eq. 4) then
            read(10, *) cp_chord_thk(i, 1:ncp_chord_thk), le_angle_cp(i), te_angle_cp(i)
            ! TODO: Push to develop
            do j = 2 + ncp_chord_thickness,ncp_chord_thk
                cp_chord_thk(i,j) = 0.5*cp_chord_thk(i,j)
            end do
            if(control_inp_flag .eq. 1) then
                xcp = cp_chord_thk(i, 2:ncp_chord_thickness+1)
                ycp = cp_chord_thk(i, ncp_chord_thickness+2:ncp_chord_thk)
            endif
        elseif (thick_distr .eq. 3) then
            read(10, *) cp_chord_thk(i, 1:ncp_chord_thk)
            if(control_inp_flag .eq. 1) then
                xcp(1) = 0.
                xcp(2) = 0.
                xcp(3:ncp_thk(i)-2) = cp_chord_thk(i, 2:ncp_chord_thickness-1)
                xcp(ncp_thk(i)-1) = 1.
                xcp(ncp_thk(i)) = 0.
                ycp(1) = 0.
                ycp(2:ncp_thk(i)-1) = cp_chord_thk(i, ncp_chord_thickness:ncp_chord_thk)
                ycp(ncp_thk(i)) = 0.
            endif
        else
            read(10, *) cp_chord_thk(i, 1:ncp_chord_thickness)
            if(control_inp_flag .eq. 1) then
                xcp(1) = 2*xcp(3)-xcp(5)
                ycp(1) = 2*ycp(3)-ycp(5)
                xcp(2) = 2*xcp(3)-xcp(4)
                ycp(2) = 2*ycp(3)-ycp(4)
                xcp(ncp_thk(i)) = 2*xcp(ncp_thk(i)-2)-xcp(ncp_thk(i)-4) 
                ycp(ncp_thk(i)) = 2*ycp(ncp_thk(i)-2)-ycp(ncp_thk(i)-4)
                xcp(ncp_thk(i)-1) = 2*xcp(ncp_thk(i)-2)-xcp(ncp_thk(i)-3)
                ycp(ncp_thk(i)-1) = 2*ycp(ncp_thk(i)-2)-ycp(ncp_thk(i)-3)
            endif
        endif

        do k = 1, ncp_thk(i)
            thk_cp(k, 2*i-1) = xcp(k)
            thk_cp(k, 2*i) = ycp(k)
        enddo
    enddo

    if (control_inp_flag .eq. 2 .and. isold .eqv. .false. .and. thick_distr .eq. 4) then

        if (allocated(temp_exact)) deallocate(temp_exact)
        allocate(temp_exact(ncp_span_thk))

        ! Override exact_thickness_flags
        temp_thk_flag(1) = te_flag
        temp_thk_flag(2) = le_opt_flag
        temp_thk_flag(3) = te_opt_flag
        call override_thk_flags(temp_thk_flag)
        te_flag     = temp_thk_flag(1)
        le_opt_flag = temp_thk_flag(2)
        te_opt_flag = temp_thk_flag(3)
      
        ! Override span_thk_ctrl
        do i = 1,ncp_span_thk
            temp_exact(i) = cp_chord_thk(i,1)
        end do
        call override_span_thk_ctrl(ncp_span_thk,temp_exact)
        do i = 1,ncp_span_thk
            cp_chord_thk(i,1) = temp_exact(i)
        end do
        
        ! Override exact_u1
        if (ncp_chord_thickness >= 1) then
            jj = 1 + 1
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_u1(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do    
        end if

        ! Override exact_u2
        if (ncp_chord_thickness >= 2) then 
            jj = 1 + 2
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_u2(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if

        ! Override exact_u3
        if (ncp_chord_thickness >= 3) then
            jj = 1 + 3
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_u3(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if

        ! Override exact_u4
        if (ncp_chord_thickness >= 4) then
            jj = 1 + 4
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_u4(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if

        ! Override exact_u5
        if (ncp_chord_thickness >= 5) then
            jj = 1 + 5
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_u5(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if

        ! Override exact_u6
        if (ncp_chord_thickness >= 6) then
            jj = 1 + 6
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_u6(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if

        ! Override exact_u7
        if (ncp_chord_thickness == 7) then
            jj = 1 + 7
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_u7(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if

        ! Override exact_thk1
        if (ncp_thickness >= 1) then
            jj = 1 + ncp_chord_thickness + 1
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_thk1(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do    
        end if

        ! Override exact_thk2
        if (ncp_thickness >= 2) then
            jj = 1 + ncp_chord_thickness + 2
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_thk2(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if

        ! Override exact_thk3
        if (ncp_thickness >= 3) then
            jj = 1 + ncp_chord_thickness + 3
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_thk3(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if

        ! Override exact_thk4
        if (ncp_thickness >= 4) then
            jj = 1 + ncp_chord_thickness + 4
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_thk4(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if
   
        ! Override exact_thk5
        if (ncp_thickness >= 5) then
            jj = 1 + ncp_chord_thickness + 5
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_thk5(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if
   
        ! Override exact_thk6
        if (ncp_thickness >= 6) then
            jj = 1 + ncp_chord_thickness + 6
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_thk6(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if

        ! Override exact_thk7
        if (ncp_thickness >= 7) then
            jj = 1 + ncp_chord_thickness + 7
            do i = 1,ncp_span_thk
                temp_exact(i) = cp_chord_thk(i,jj)
            end do
            call override_exact_thk7(ncp_span_thk,temp_exact)
            do i = 1,ncp_span_thk
                cp_chord_thk(i,jj) = temp_exact(i)
            end do
        end if
    
        ! Override le_angle_cp
        do i = 1,ncp_span_thk
            temp_exact(i) = le_angle_cp(i)
        end do
        call override_exact_lethk(ncp_span_thk,temp_exact)
        do i = 1,ncp_span_thk
            le_angle_cp(i) = temp_exact(i)
        end do

        ! Override te_angle_cp
        do i = 1,ncp_span_thk
            temp_exact(i) = te_angle_cp(i)
        end do
        call override_exact_tethk(ncp_span_thk,temp_exact)
        do i = 1,ncp_span_thk
            te_angle_cp(i) = temp_exact(i)
        end do

    end if
    
    if (LE .ne. 0) then
        !--------------------------------------------------------------------------
        !Reading LE control points
        do i = 1, 3
            read(10, *)  
        end do
        read(10, *)LE_deg, LE_seg
        read(10, *)
        read(10, *)ncp_span_LE

        ncp_LE = 13 !there are 13 different control points
        !Giving the same variable name as in controlinputs
        LEdegree = LE_deg
        no_LE_segments = LE_seg

        ncp_span_LE1 = ncp_span_LE+2

        if (allocated(cp_LE)) deallocate(cp_LE)
        allocate(cp_LE(ncp_span_LE, ncp_LE+1))


        read(10, *)
        do i = 1, ncp_span_LE
            read(10, *)cp_LE(i, 1:ncp_LE+1)
        end do
    endif
        ! if (thick_distr .eq. 4) then
            ! if (allocated(te_angle_cp)) deallocate(te_angle_cp)
            ! Allocate(te_angle_cp(nsl))
            ! read (10, *), temp
            ! read (10, *), te_flag
            ! read (10, *), temp
            ! do i = 1, nsl
                ! read(10, *), te_angle_cp(i)
            ! enddo
        ! endif
    endif
close(10)

!---------------------------------------------------------------------------

call log_file_exists(log_file, nopen, file_open)
print*, 'spanwise input file read successfully'
write(nopen,*) 'spanwise input file read successfully'
call close_log_file(nopen, file_open)
!--------------------------------------------------------------------------------------------------
!This part of the program is used to check if the input data has been read correctly.
!It is not required and is therefore commented. Uncomment to create controlinupts_check.dat file 
!which should give the exact input file if this subroutine works correctly

!open(20, file = 'controlinupts_check.dat')
!write(20, *)'  span    u2     u3     u4     u5     u6    curv2  curv3  curv4  curv5  curv6  curv7'
!do i = 1, ncp_span_curv
!write(20, 10)cp_chord_curv(i, 1:ncp_chord_curv)
!end do

!write(20, *)'  span    thk2     thk3     thk4     thk5     thk6'
!do i = 1, ncp_span_thk
!write(20, 10)cp_thk(i, 1:ncp_thickness-1)
!end do

!write(20, *)
!do i = 1, ncp_span_LE
!write(20, 10)cp_LE(i, 1:ncp_LE+1)
!10 format(14f10.3)
!end do
!--------------------------------------------------------------------------------------------------

end subroutine read_spanwise_input
