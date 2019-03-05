!
! This subroutine arranges the values of the control points obtained
! by creating spanwise cubic B-splines in subroutine spanwise_variation() 
! into the specific format for curv_cp, thk_cp and LE arrays
! These arrays are used for calculation of blade sections in
! subroutine bladegen() in bladegen.f90
!
!-------------------------------------------------------------------------------------------------------------------------------
subroutine span_output ()
    use globvar
    implicit none

    integer                     :: cp_start

    
    ! Allocate curvature control points array
    if (allocated(curv_cp)) deallocate(curv_cp)
    Allocate(curv_cp(20,2*na))

    ! Populate curvature control points array
    k = 1

    do i = 1,na
      if (k <= na) then

        ! Defining fixed control points 
        curv_cp(2, 2*k - 1)             = 0.0
        curv_cp(ncp_chord + 1, 2*k - 1) = 1.0
        
        ! Store spanwise curvature control points from spanwise_variation()
        do j = 3,ncp_chord
            curv_cp(j, 2*k - 1) = bspline_chord_curv(i,j - 1)
        end do
        
        ! Adding two phantom points required for cubic B-spline
        curv_cp(1, 2*k - 1)            = 2*curv_cp(2,2*k - 1) - curv_cp(3,2*k - 1)
        curv_cp(ncp_chord + 2,2*k - 1) = 2*curv_cp(ncp_chord + 1,2*k - 1) - curv_cp(ncp_chord,2*k - 1)
        
        ! Defining fixed control points 
        if (isold) then
            curv_cp(2,2*k) = 0.0
            cp_start       = 3
        else
            cp_start       = 2
        endif
        
        ! Store spanwise curvature control points from spanwise_variation()
        do j = cp_start,ncp_curvature + 1
            curv_cp(j,2*k) = bspline_chord_curv(i,ncp_chord + j - cp_start)
        end do
        
        !Adding two phantom points required for cubic B-spline
        curv_cp(1,2*k)                  = 2*curv_cp(2,2*k) - curv_cp(3,2*k)
        curv_cp(ncp_curvature + 2,2*k)  = 2*curv_cp(ncp_curvature + 1,2*k) - curv_cp(ncp_curvature,2*k)
        
        k = k + 1

      end if    ! if (k <= na)
    end do  ! i = 1,na


    !
    ! Populate thickness control points array
    ! Quartic spline thickness
    !
    if (thick .ne. 0) then
        
        ! Allocate thickness control points array
        if (allocated(thk_cp)) deallocate(thk_cp)
        allocate(thk_cp(20,2*na))
        
        ! Populate thickness control points array 
        k = 1

        do i = 1,na
          if (k <= na) then

            ! Defining fixed control points 
            thk_cp(3, 2*k - 1)                       = 0.0
            thk_cp(ncp_chord_thickness + 2, 2*k - 1) = 1.0
            
            ! Store spanwise thickness control points from spanwise_variation()
            do j = 4,ncp_chord_thickness + 1
                thk_cp(j, 2*k - 1) = bspline_thk(i, j - 2)
            end do
            
            !Adding four phantom points required for creating a quartic spline
            thk_cp(1,2*k - 1)                       = 2*thk_cp(3,2*k - 1) - thk_cp(5,2*k - 1)
            thk_cp(2,2*k - 1)                       = 2*thk_cp(3,2*k - 1) - thk_cp(4,2*k - 1)
            thk_cp(ncp_chord_thickness + 4,2*k - 1) = 2*thk_cp(ncp_chord_thickness + 2,2*k - 1) - thk_cp(ncp_chord_thickness,    2*k - 1)
            thk_cp(ncp_chord_thickness + 3,2*k - 1) = 2*thk_cp(ncp_chord_thickness + 2,2*k - 1) - thk_cp(ncp_chord_thickness + 1,2*k - 1)

            ! Defining fixed control points 
            thk_cp(3,2*k)                 = 0.0
            thk_cp(ncp_thickness + 2,2*k) = 0.0
            
            ! Store spanwise thickness control points from spanwise_variation()
            do j = 4,ncp_thickness + 1
                thk_cp(j,2*k) = bspline_thk(i,ncp_chord_thickness + j - 4)
            end do
            
            !Adding four phantom points required for creating a quartic spline
            thk_cp(1,2*k)                 = 2*thk_cp(3,2*k) - thk_cp(5,2*k)
            thk_cp(2,2*k)                 = 2*thk_cp(3,2*k) - thk_cp(4,2*k)
            thk_cp(ncp_thickness + 4,2*k) = 2*thk_cp(ncp_thickness + 2,2*k) - thk_cp(ncp_thickness,    2*k)
            thk_cp(ncp_thickness + 3,2*k) = 2*thk_cp(ncp_thickness + 2,2*k) - thk_cp(ncp_thickness + 1,2*k)
            
            k = k + 1

          end if    ! if (k <= na)
        end do  ! i = 1,na

    ! Direct thickness distribution
    else if (thick_distr .eq. 3) then
        
        ! Allocate thickness control points array
        if (allocated(thk_cp)) deallocate(thk_cp)
        Allocate(thk_cp(20,2*na))

        ! Populate thickness control points array
        k = 1

        do i = 1,na
            if (k <= na) then
                
                ! Store spanwise thickness control points from spanwise_variation()
                do j = 1,ncp_chord_thickness - 2
                    thk_cp(j,2*k - 1) = bspline_thk(i,j + 1)
                end do
                
                do j=1,ncp_thickness - 2
                    thk_cp(j,2*k) = bspline_thk(i,ncp_chord_thickness + j - 1)
                end do

                k = k + 1

            end if  ! if (k <= na)
        end do  ! i = 1,na

    ! Exact thickness distribution
    else if(thick_distr .eq. 4) then
        
        ! Allocate thickness control points array
        if (allocated(thk_cp)) deallocate(thk_cp)
        Allocate(thk_cp(20, 2*na))

        ! Populate thickness control points array
        k = 1
        do i = 1, na
            if (k <= na)then

                ! Store spanwise thickness control points from spanwise_variation()
                do j = 1, ncp_thickness
                    thk_cp(j,2*k - 1) = bspline_thk(i, 2*j)
                    thk_cp(j,2*k)     = bspline_thk(i, 2*j + 1)
                enddo

                k = k + 1

            end if  ! if (k <= na)
        end do  ! i = 1,na

    ! Modified NACA thickness distribution
    else if (thick_distr == 5) then

        ! Allocate thickness control points array
        if (allocated(thk_cp)) deallocate(thk_cp)
        allocate(thk_cp(20,2*na))

        ! Populate thickness control points array
        thk_cp(1,1:na)      = bspline_thk(:,2)
        thk_cp(2,1:na)      = bspline_thk(:,3)
        thk_cp(3,1:na)      = bspline_thk(:,4)
        thk_cp(4,1:na)      = bspline_thk(:,5)

    end if  ! thick_distr



    ! Populate spline LE control points array
    if(LE .ne. 0) then
        
        ! Allocate necessary control points arrays
        if (allocated(sting_l_all)) deallocate(sting_l_all)
        allocate(sting_l_all(na))
          !if(LE .ne.0) then
        if (allocated(lethk_all)) deallocate(lethk_all)
        allocate(lethk_all(na))
        if (allocated(tethk_all)) deallocate(tethk_all)
        allocate(tethk_all(na))
        if (allocated(s_all)) deallocate(s_all)
        allocate(s_all(na))
        if (allocated(ee_all)) deallocate(ee_all)
        allocate(ee_all(na))
        if (allocated(C_le_x_top_all)) deallocate(C_le_x_top_all)
        allocate(C_le_x_top_all(na))
        if (allocated(C_le_x_bot_all)) deallocate(C_le_x_bot_all)
        allocate(C_le_x_bot_all(na))
        if (allocated(C_le_y_top_all)) deallocate(C_le_y_top_all)
        allocate(C_le_y_top_all(na))
        if (allocated(C_le_y_bot_all)) deallocate(C_le_y_bot_all)
        allocate(C_le_y_bot_all(na))
        if (allocated(LE_vertex_ang_all)) deallocate(LE_vertex_ang_all)
        allocate(LE_vertex_ang_all(na))
        if (allocated(LE_vertex_dis_all)) deallocate(LE_vertex_dis_all)
        allocate(LE_vertex_dis_all(na))
        if (allocated(sting_h_all)) deallocate(sting_h_all)
        allocate(sting_h_all(na,2))
            
        ! Store spanwise spline LE control points from spanwise_variation()
        k = 1

        do i = 1,na
            if (k <= na) then
                
                lethk_all(k)         = bspline_LE(i,2)
                tethk_all(k)         = bspline_LE(i,3)
                s_all(k)             = bspline_LE(i,4)
                ee_all(k)            = bspline_LE(i,5)
                C_le_x_top_all(k)    = bspline_LE(i,6)
                C_le_x_bot_all(k)    = bspline_LE(i,7)
                C_le_y_top_all(k)    = bspline_LE(i,8)
                C_le_y_bot_all(k)    = bspline_LE(i,9)
                LE_vertex_ang_all(k) = bspline_LE(i,10)
                LE_vertex_dis_all(k) = bspline_LE(i,11)
                sting_l_all(k)       = bspline_LE(i,12)
                sting_h_all(k,1)     = bspline_LE(i,13)
                sting_h_all(k,2)     = bspline_LE(i,14)
            
                k = k + 1

            end if  ! if (k <= na)
        end do  ! if i = 1,na
        !end if
    endif
    
    
    ! Write curvature control points to file
    open(150, file = 'curv_cp.dat')
        do i = 1,20
            write(150,'(22F15.2)') curv_cp(i,1:2*na)
        end do
    close(150)


end subroutine span_output
!-------------------------------------------------------------------------------------------------------------------------------




















