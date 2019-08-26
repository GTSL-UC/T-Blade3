#ifdef  STAND_ALONE
    program bgb3d ! 3d blade geometry builder
        use globvar
        implicit none

        character(256) :: fname, row_type
        character(32 ) :: arg2, arg3, arg4
        
        
        !
        ! Get first command line argument
        ! Main input filename 
        !
        call getarg(1, fname)
        
        ! Number of command line arguments
        narg = iargc()

        ! Get second command line argument
        if(narg.ge.2)then
            call getarg(2, arg2)
        else
            arg2 = ''
        endif

        ! Get third command line argument
        if(narg.eq.3)then
            call getarg(3, arg3)
        else
            arg3 = ''
        endif

        ! Get fourth command line argument
        if(narg.eq.4)then
            call getarg(4, arg4)
        else
            arg4 = ''
        endif

        ! Get main input file name
        k = (index(fname, '.')+1)
        do i = k, len(trim(fname))
            if (fname(i:i) == '.') then
                j = i-1
                continue
            endif
        enddo
        row_type = fname(k:j)

        ! Call T-Blade3 driver subroutine
        call bgb3d_sub(fname, 'spancontrolinputs.'//trim(row_type)//'.dat', arg2, arg3, arg4)


    end program bgb3d

    ! ESP override subroutine templates
    !subroutine     override_cur1(n, a)
    !   real*8 a(*)
    !end subroutine override_cur1
    !subroutine     override_cur2(n, a)
    !   real*8 a(*)
    !end subroutine override_cur2
    !subroutine     override_cur3(n, a)
    !   real*8 a(*)
    !end subroutine override_cur3
    !subroutine     override_cur4(n, a)
    !   real*8 a(*)
    !end subroutine override_cur4
    !subroutine     override_cur5(n, a)
    !   real*8 a(*)
    !end subroutine override_cur5
    !subroutine     override_cur6(n, a)
    !   real*8 a(*)
    !end subroutine override_cur6
    !subroutine     override_cur7(n, a)
    !   real*8 a(*)
    !end subroutine override_cur7
    !subroutine     override_u2(n, a)
    !   real*8 a(*)
    !end subroutine override_u2
    !subroutine     override_u3(n, a)
    !   real*8 a(*)
    !end subroutine override_u3
    !subroutine     override_u4(n, a)
    !   real*8 a(*)
    !end subroutine override_u4
    !subroutine     override_u5(n, a)
    !   real*8 a(*)
    !end subroutine override_u5
    !subroutine     override_u6(n, a)
    !   real*8 a(*)
    !end subroutine override_u6
    !subroutine     override_span_del_m_ctrl(n, a)
    !   real*8 a(*)
    !end subroutine override_span_del_m_ctrl
    !subroutine     override_span_del_theta_ctrl(n, a)
    !   real*8 a(*)
    !end subroutine override_span_del_theta_ctrl
    !subroutine     override_span_in_beta_ctrl(n, a)
    !   real*8 a(*)
    !end subroutine override_span_in_beta_ctrl
    !subroutine     override_span_out_beta_ctrl(n, a)
    !   real*8 a(*)
    !end subroutine override_span_out_beta_ctrl
    !subroutine     override_span_chord_ctrl(n, a)
    !   real*8 a(*)
    !end subroutine override_span_chord_ctrl
    !subroutine     override_span_thk_c_ctrl(n, a)
    !   real*8 a(*)
    !end subroutine override_span_thk_c_ctrl
    !subroutine     override_span_del_m(n, a)
    !   real*8 a(*)
    !end subroutine override_span_del_m
    !subroutine     override_span_del_theta(n, a)
    !   real*8 a(*)
    !end subroutine override_span_del_theta
    !subroutine     override_span_in_beta(n, a)
    !   real*8 a(*)
    !end subroutine override_span_in_beta
    !subroutine     override_span_out_beta(n, a)
    !   real*8 a(*)
    !end subroutine override_span_out_beta
    !subroutine     override_span_chord(n, a)
    !   real*8 a(*)
    !end subroutine override_span_chord
    !subroutine     override_span_thk_c(n, a)
    !  real*8 a(*)
    !end subroutine override_span_thk_c
    !subroutine     override_span_u_max(n, a)
    !  real*8 a(*)
    !end subroutine override_span_u_max
    !subroutine     override_span_curv_ctrl(n, a)
    !   real*8 a(*)
    !end subroutine override_span_curv_ctrl
    !subroutine     override_span_thk_ctrl(n, a)
    !  real*8 a(*)
    !end subroutine override_span_thk_ctrl
    !subroutine     override_offsets(a)
    !  integer a(*)
    !end subroutine override_offsets
    !subroutine     override_naca_le_radius(n, a)
    !  real*8 a(*)
    !end subroutine override_naca_le_radius
    !subroutine     override_naca_u_max(n, a)
    !  real*8 a(*)
    !end subroutine override_naca_u_max
    !subroutine     override_naca_t_max(n, a)
    !  real*8 a(*)
    !end subroutine override_naca_t_max
    !subroutine     override_naca_t_te(n, a)
    !  real*8 a(*)
    !end subroutine override_naca_t_te

#else
    program bgb3d ! 3d blade geometry builder
        use globvar
        implicit none

        character(256) :: fname, row_type
        character(32 ) :: arg2, arg3, arg4
        
        
        !
        ! Get first command line argument
        ! Main input filename 
        !
        call getarg(1, fname)
        
        ! Number of command line arguments
        narg = iargc()

        ! Get second command line argument
        if(narg.ge.2)then
            call getarg(2, arg2)
        else
            arg2 = ''
        endif

        ! Get third command line argument
        if(narg.eq.3)then
            call getarg(3, arg3)
        else
            arg3 = ''
        endif

        ! Get fourth command line argument
        if(narg.eq.4)then
            call getarg(4, arg4)
        else
            arg4 = ''
        endif

        ! Get main input file name
        k = (index(fname, '.')+1)
        do i = k, len(trim(fname))
            if (fname(i:i) == '.') then
                j = i-1
                continue
            endif
        enddo
        row_type = fname(k:j)

        ! Call T-Blade3 driver subroutine
        call bgb3d_sub(fname, 'spancontrolinputs.'//trim(row_type)//'.dat', arg2, arg3, arg4)


    end program bgb3d

     ESP override subroutine templates
    subroutine     override_cur1(n, a)
       real*8 a(*)
    end subroutine override_cur1
    subroutine     override_cur2(n, a)
       real*8 a(*)
    end subroutine override_cur2
    subroutine     override_cur3(n, a)
       real*8 a(*)
    end subroutine override_cur3
    subroutine     override_cur4(n, a)
       real*8 a(*)
    end subroutine override_cur4
    subroutine     override_cur5(n, a)
       real*8 a(*)
    end subroutine override_cur5
    subroutine     override_cur6(n, a)
       real*8 a(*)
    end subroutine override_cur6
    subroutine     override_cur7(n, a)
       real*8 a(*)
    end subroutine override_cur7
    subroutine     override_u2(n, a)
       real*8 a(*)
    end subroutine override_u2
    subroutine     override_u3(n, a)
       real*8 a(*)
    end subroutine override_u3
    subroutine     override_u4(n, a)
       real*8 a(*)
    end subroutine override_u4
    subroutine     override_u5(n, a)
       real*8 a(*)
    end subroutine override_u5
    subroutine     override_u6(n, a)
       real*8 a(*)
    end subroutine override_u6
    subroutine     override_span_del_m_ctrl(n, a)
       real*8 a(*)
    end subroutine override_span_del_m_ctrl
    subroutine     override_span_del_theta_ctrl(n, a)
       real*8 a(*)
    end subroutine override_span_del_theta_ctrl
    subroutine     override_span_in_beta_ctrl(n, a)
       real*8 a(*)
    end subroutine override_span_in_beta_ctrl
    subroutine     override_span_out_beta_ctrl(n, a)
       real*8 a(*)
    end subroutine override_span_out_beta_ctrl
    subroutine     override_span_chord_ctrl(n, a)
       real*8 a(*)
    end subroutine override_span_chord_ctrl
    subroutine     override_span_thk_c_ctrl(n, a)
       real*8 a(*)
    end subroutine override_span_thk_c_ctrl
    subroutine     override_span_del_m(n, a)
       real*8 a(*)
    end subroutine override_span_del_m
    subroutine     override_span_del_theta(n, a)
       real*8 a(*)
    end subroutine override_span_del_theta
    subroutine     override_span_in_beta(n, a)
       real*8 a(*)
    end subroutine override_span_in_beta
    subroutine     override_span_out_beta(n, a)
       real*8 a(*)
    end subroutine override_span_out_beta
    subroutine     override_span_chord(n, a)
       real*8 a(*)
    end subroutine override_span_chord
    subroutine     override_span_thk_c(n, a)
      real*8 a(*)
    end subroutine override_span_thk_c
    subroutine     override_span_u_max(n, a)
      real*8 a(*)
    end subroutine override_span_u_max
    subroutine     override_span_curv_ctrl(n, a)
       real*8 a(*)
    end subroutine override_span_curv_ctrl
    subroutine     override_span_thk_ctrl(n, a)
      real*8 a(*)
    end subroutine override_span_thk_ctrl
    subroutine     override_offsets(a)
      integer a(*)
    end subroutine override_offsets
    subroutine     override_naca_le_radius(n, a)
      real*8 a(*)
    end subroutine override_naca_le_radius
    subroutine     override_naca_u_max(n, a)
      real*8 a(*)
    end subroutine override_naca_u_max
    subroutine     override_naca_t_max(n, a)
      real*8 a(*)
    end subroutine override_naca_t_max
    subroutine     override_naca_t_te(n, a)
      real*8 a(*)
    end subroutine override_naca_t_te

#endif
