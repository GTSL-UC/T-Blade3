module globvar
    implicit none



    ! Global string variables
    character(256)                  :: fext, line2, argp1, ok, anglespline
    character(16)                   :: blrow, radialsec
    character(32)                   :: blext(100), casename, spanwise_spline, develop, xygrid, xyzstreamlines
    character(10)                   :: ibrowc, ibrowc1
    character(20)                   :: airfoil(100)
    character(20),      allocatable :: throat_pos(:) 
    character(80)                   :: file1, file2, file3, file4, file5, file6, file7, file8, file9, file10, file11
    character(2)                    :: units
    character(1)                    :: trueleansweep
    character(15)                   :: istr1, istr2, H(13)


    
    ! Global integer constants
    integer,    parameter           :: nspan = 200, nrow = 1, nx = 500, nax = 50, jk = 12, amount_data = 15

    ! Global integer variables
    integer                         :: ibrow, ile, ite, j, LEdegree, no_LE_segments, csng
    integer                         :: f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11
    integer                         :: i, js, k, p, nsec, switch, ia, na, stack, current, narg
    integer                         :: nspn, n, nsl, nsp_hub, nsp_tip
    integer                         :: nwork, lenc, nd, np, ndep, radial, chord_switch, stack_switch, leansweep_switch, &
                                       clustering_switch
    integer                         :: beta_switch, curv, thick, LE, thick_distr, wing_flag, control_inp_flag
    integer                         :: cpbsv, bsv1, bsv2, bf1(100), bf2(100)
    integer                         :: n_normal_distance, chrdsweep, chrdlean
    integer                         :: m, ncp_span_curv, ncp_span_curv1, ncp_span_thk, ncp_span_thk1, ncp_span_LE, &
                                       ncp_span_LE1
    integer                         :: ncp_thickness, ncp_LE, ncp_chord, ncp_chord_thickness, ncp_curvature, ncp_chord_curv, &
                                       ncp_chord_thk, num_points, num_sec, delta1, bladerow, LE_deg, LE_seg
    integer                         :: nsp(nspan), curve, npoints, nbls, i_slope, i_slope_nonoffset_hub, ii, nspline, ncp1, k_tip, &
                                       rad_in_flag(nspan), rad_out_flag(nspan)
    integer                         :: cpdeltam, cpdeltheta, cpinbeta, cpoutbeta, cpchord, cptm_c
    integer                         :: n_inter_intervals, nsp_interpolated_hub, spline_switch
    integer,            allocatable :: ncp_curv(:), ncp_thk(:), throat_index(:), BGgrid_all(:)
    
    
    
    ! Global real data type variables
    real                            :: sinl, sext, thkc, chrdx, mr1, stak_u, stak_v, x1hub, x1tip, r1hub, r1tip
    real                            :: xb(nx), yb(nx), mp(nx,nax), xstk, xstk1, abs_zero
    real                            :: xa(nx,nax), ya(nx,nax), xms(nx,nax), rms(nx,nax)
    real                            :: xxa, yya, scf, lref
    real                            :: tempr, xstck
    real                            :: xmsle(nspan), rmsle(nspan), xmste(nspan), rmste(nspan)
    real                            :: span(nspan), del_out_beta(nspan), out_beta_new(nspan)
    real                            :: mste(nspan), stgr, theta_offset
    real                            :: clustering_parameter
    real                            :: le_throat, te_throat
    real                            :: pi, dtor, xcg(nspan), ycg(nspan), chrd(nspan), xcen, ycen, xb_stk, yb_stk, xb_stack(nspan), &
                                       yb_stack(nspan)
    real                            :: beta1star(jk), beta2star(jk)
    real                            :: beta1star_new(nspan), beta2star_new(nspan)
    real                            :: rad(jk), rbar(nspan), thk(jk), thkc_new(nspan) 
    real                            :: spanbsv(100)
    real                            :: xm_slope, rm_slope, xslope_LE, xslope_TE, rslope_LE, rslope_TE
    real                            :: xm(nx,nax), rm(nx,nax), xi, xsle, xste, ri, rsle, rste
    real                            :: trarray(3), A1(nspan), B1(nspan), A2(nspan), B2(nspan)
    real                            :: xi1, ri1
    real                            :: xle_nonoffset_hub(nx), xte_nonoffset_hub(nx), rle_nonoffset_hub(nx), &
                                       rte_nonoffset_hub(nx)
    real                            :: xles_nonoffset_hub(nx), xtes_nonoffset_hub(nx), rles_nonoffset_hub(nx), &
                                       rtes_nonoffset_hub(nx)
    real                            :: sle_nonoffset_hub(nx), ste_nonoffset_hub(nx)
    real                            :: xm_nonoffset_hub(nx), rm_nonoffset_hub(nx), xms_nonoffset_hub(nx), rms_nonoffset_hub(nx)
    real                            :: mp_nonoffset_hub(nx), s1le_nonoffset_hub, s2le_nonoffset_hub, x_le_nonoffset_hub, &
                                       r_le_nonoffset_hub
    real                            :: s1te_nonoffset_hub, s2te_nonoffset_hub, x_te_nonoffset_hub, r_te_nonoffset_hub
    real                            :: msle_nonoffset_hub, mste_nonoffset_hub, span_nonoffset_hub, chordm_nonoffset_hub
    real                            :: xmsle_nonoffset_hub, rmsle_nonoffset_hub, xmste_nonoffset_hub, rmste_nonoffset_hub
    real                            :: phi_s_in_nonoffset_hub, phi_s_out_nonoffset_hub
    real                            :: xcpdelm(100), xcpdeltheta(100), xcpinbeta(100), xcpoutbeta(100), staggspline
    real                            :: spanmp(100), xcpdelmp(100), spaninbeta(100), spanoutbeta(100)
    real                            :: spantheta(100), chords(nspan), inbeta_s(nspan), outbeta_s(nspan), thk_tm_c_spl(nspan), &
                                       inci_s(nspan), intersec(nspan), dev_s(nspan)
    real                            :: xcpchord(100), xcptm_c(100), xcpumax(100), spanchord(100), spantm_c(100)
    real                            :: hub, tip, sweep1, radius_tolerance, hub_inf_offset, tip_inf_offset
    real                            :: delta
    real                            :: xle(nx), xte(nx), rle(nx), rte(nx), xles(nx), xtes(nx), rles(nx), rtes(nx), sle(nx), &
                                       ste(nx)
    real                            :: rrle, rrte, a, b, xii, xmm, rii, rmm, xxle, xxte, xx1, rr1, xx2, rr2
    real                            :: res1, res2, J11, J12, J21, J22, detJ, dels1, dels2
    real                            :: y_spl_end(nx)
    real                            :: xc(nx), yc(nx)
    real                            :: xbs(nx), ybs(nx), pitch
    real                            :: mble, mbte, mles, mtes, mslehub
    real                            :: xnorm(nx,nax), rnorm(nx,nax)
    real                            :: deltan, dxn(nx,nax), drn(nx,nax)
    real                            :: xhub(nx,nax), rhub(nx,nax), mphub(nx,nax)
    real                            :: xtip(nx,nax), rtip(nx,nax), mptip(nx,nax)
    real                            :: xt(nx,1), rt(nx,1)

    ! Global 1D real allocatable arrays
    real,               allocatable :: x_le(:), x_te(:), r_le(:), r_te(:), in_beta(:), out_beta(:), phi_s_in(:),        &
                                       phi_s_out(:), msle(:), chord(:), mrel1(:), thk_c(:), inci(:), devn(:),           &
                                       sec_flow_ang(:), stagger(:), chordm(:), sang(:), stk_u(:), stk_v(:),             &
                                       jcellblade_all(:), etawidth_all(:), axchrd(:), total_camber(:), xcp(:),          &
                                       ycp(:), mprime_ble(:), mprime_bte(:), lethk_all(:), tethk_all(:), s_all(:),      &
                                       ee_all(:), umxthk_all(:), sting_l_all(:), C_le_x_top_all(:),                     &
                                       C_le_x_bot_all(:), C_le_y_top_all(:), C_le_y_bot_all(:), LE_vertex_ang_all(:),   &
                                       LE_vertex_dis_all(:), throat_3D(:), mouth_3D(:), exit_3D(:), hub_slope(:),       &
                                       le_angle_cp(:), te_angle_cp(:), s1le(:), s2le(:), s1te(:), s2te(:), section(:)

    ! Global 2D real allocatable arrays
    real,               allocatable :: bladedata(:,:), bladedata_before(:,:), splinedata(:,:), curv_cp(:,:), thk_cp(:,:),   &
                                       sec_radius(:,:), sting_h_all(:,:), intersec_coord(:,:), mblade_grid(:,:),            &
                                       thblade_grid(:,:), cp_chord_curv(:,:), bspline_chord_curv(:,:), cp_chord_thk(:,:),   &
                                       bspline_thk(:,:), cp_LE(:,:), bspline_LE(:,:)

    ! Global logical variables
    logical                         :: hub_inflate = .false., tip_inflate = .false.
    logical                         :: TE_der_actual = .false., TE_der_norm = .false.
    logical                         :: isdev, tm_c_spline, is_xyzstreamlines, spanwise_angle_spline, spanwise_inci_dev_spline, &
                                       is2d, isquiet
    logical                         :: u_max_spline = .false.
    logical                         :: axial_LE, radial_LE
    logical                         :: using_cur1


    
    ! Common memory block
    common / bladesectionpoints /xxa(nx,nax),yya(nx,nax)




















end module globvar
