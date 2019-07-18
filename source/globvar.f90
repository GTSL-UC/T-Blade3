MODULE globvar
implicit none

integer ibrow,ile,ite,j,LEdegree,no_LE_segments,csng
!
character*256 :: fext,line2,argp1,ok
character*16 blrow,radialsec
character*32 blext(100),casename,spanwise_spline
character*256 anglespline
character*10 ibrowc,ibrowc1
character*20 airfoil(100)
character*20, allocatable :: throat_pos(:) 
character*32 develop, xygrid, xyzstreamlines
character*80 file1,file2,file3,file4,file5,file6,file7,file8,file9,file10,file11
character(len=2) :: units
character(len=1) :: trueleansweep
!
integer f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11
integer i,js,k,p,nsec,switch,ia,na,jk,stack,current, narg
integer nrow,nspn,nx,nax,n,nsl,nspan,nsp_hub,nsp_tip
integer nwork,lenc,nd,np,ndep,radial,chord_switch,stack_switch,leansweep_switch,clustering_switch
integer beta_switch,curv,thick,LE,thick_distr, wing_flag, control_inp_flag
integer cpbsv,bsv1,bsv2,bf1(100),bf2(100)
integer amount_data,n_normal_distance,chrdsweep,chrdlean
!-----------------------------------------------------------------------------------------------------------------------------
integer m,ncp_span_curv,ncp_span_curv1,ncp_span_thk,ncp_span_thk1,ncp_span_LE,ncp_span_LE1
integer ncp_thickness,ncp_LE,ncp_chord,ncp_chord_thickness,ncp_curvature,ncp_chord_curv,ncp_chord_thk, &
        num_points,num_sec,delta1,bladerow,LE_deg,LE_seg
!--------------------------------------------------------------------------------------------------------------------------
!
parameter (nspan=200,nrow=1,nx=500,nax=50,jk=12,amount_data= 15)

integer nsp(nspan),curve,npoints,nbls,i_slope,i_slope_nonoffset_hub,ii,nspline,ncp1,k_tip, rad_in_flag(nspan), rad_out_flag(nspan)
integer cpdeltam,cpdeltheta,cpinbeta,cpoutbeta,cpchord,cptm_c
integer, allocatable, dimension(:) :: ncp_curv,ncp_thk,throat_index,BGgrid_all
integer n_inter_intervals,nsp_interpolated_hub, spline_switch
!
real*8, allocatable, dimension(:,:) :: bladedata,splinedata
real*8 sinl,sext,thkc ,chrdx,mr1,stak_u,stak_v, x1hub,x1tip,r1hub,r1tip
real*8 xb(nx),yb(nx),mp(nx,nax),xstk,xstk1,abs_zero
real*8 xa(nx,nax),ya(nx,nax),xms(nx,nax),rms(nx,nax)
real*8 xxa,yya,scf,lref
real*8 tempr,xstck
real*8 xmsle(nspan),rmsle(nspan),xmste(nspan),rmste(nspan)
real*8 span(nspan),del_out_beta(nspan),out_beta_new(nspan)
real*8 mste(nspan),stgr, theta_offset
real*8, allocatable, dimension(:) :: x_le,x_te,r_le,r_te,in_beta,out_beta,phi_s_in,phi_s_out,msle
real*8, allocatable, dimension(:) :: chord,mrel1,thk_c,inci,devn,sec_flow_ang,stagger,chordm,sang,stk_u,stk_v
real*8, allocatable, dimension(:) :: jcellblade_all, etawidth_all,axchrd
real*8, allocatable, dimension(:) :: total_camber,xcp,ycp,mprime_ble,mprime_bte
real*8, allocatable, dimension(:,:) :: curv_cp,thk_cp,sec_radius,sting_h_all
real*8, allocatable, dimension(:) :: lethk_all,tethk_all,s_all,ee_all,umxthk_all,sting_l_all
real*8, allocatable, dimension(:) :: C_le_x_top_all,C_le_x_bot_all,C_le_y_top_all,C_le_y_bot_all
real*8, allocatable, dimension(:) :: LE_vertex_ang_all,LE_vertex_dis_all
real*8, allocatable, dimension(:,:) :: intersec_coord
real*8, allocatable, dimension(:) :: throat_3D,mouth_3D,exit_3D
real*8, allocatable, dimension(:) :: hub_slope, le_angle_cp, te_angle_cp
real*8 :: clustering_parameter
real*8  le_throat,te_throat
real*8 pi,dtor,xcg(nspan),ycg(nspan),chrd(nspan), xcen, ycen, xb_stk, yb_stk, xb_stack(nspan), yb_stack(nspan)
!
real*8 beta1star(jk),beta2star(jk)
real*8 beta1star_new(nspan),beta2star_new(nspan)
real*8 rad(jk),rbar(nspan),thk(jk),thkc_new(nspan) 
real*8 spanbsv(100)
real*8 xm_slope,rm_slope,xslope_LE,xslope_TE,rslope_LE,rslope_TE

real*8 xm(nx,nax),rm(nx,nax),xi,xsle,xste,ri,rsle,rste
real*8 trarray(3),A1(nspan),B1(nspan),A2(nspan),B2(nspan)
real*8 xi1,ri1
! 
real*8 xle_nonoffset_hub(nx),xte_nonoffset_hub(nx),rle_nonoffset_hub(nx),rte_nonoffset_hub(nx)
real*8 xles_nonoffset_hub(nx),xtes_nonoffset_hub(nx),rles_nonoffset_hub(nx),rtes_nonoffset_hub(nx)
real*8 sle_nonoffset_hub(nx),ste_nonoffset_hub(nx)
real*8 xm_nonoffset_hub(nx),rm_nonoffset_hub(nx),xms_nonoffset_hub(nx),rms_nonoffset_hub(nx)
real*8 mp_nonoffset_hub(nx),s1le_nonoffset_hub,s2le_nonoffset_hub,x_le_nonoffset_hub,r_le_nonoffset_hub
real*8 s1te_nonoffset_hub,s2te_nonoffset_hub,x_te_nonoffset_hub,r_te_nonoffset_hub
real*8 msle_nonoffset_hub,mste_nonoffset_hub,span_nonoffset_hub,chordm_nonoffset_hub
real*8 xmsle_nonoffset_hub,rmsle_nonoffset_hub,xmste_nonoffset_hub,rmste_nonoffset_hub
real*8 phi_s_in_nonoffset_hub,phi_s_out_nonoffset_hub

!
real*8 xcpdelm(100),xcpdeltheta(100),xcpinbeta(100),xcpoutbeta(100),staggspline
real*8 spanmp(100),xcpdelmp(100),spaninbeta(100),spanoutbeta(100)
real*8 spantheta(100),chords(nspan),inbeta_s(nspan),outbeta_s(nspan),thk_tm_c_spl(nspan),inci_s(nspan),intersec(nspan),dev_s(nspan)
real*8 xcpchord(100),xcptm_c(100),xcpumax(100),spanchord(100),spantm_c(100)
real*8 hub,tip, sweep1,radius_tolerance

real*8 xle(nx),xte(nx),rle(nx),rte(nx),xles(nx),xtes(nx),rles(nx),rtes(nx),sle(nx),ste(nx)
real*8 rrle,rrte,a,b,xii,xmm,rii,rmm,xxle,xxte,xx1,rr1,xx2,rr2
real*8 res1,res2,J11,J12,J21,J22,detJ,dels1,dels2
real*8, allocatable, dimension(:) :: s1le,s2le,s1te,s2te
real*8 y_spl_end(nx)
real*8 xc(nx),yc(nx)
real*8 xbs(nx), ybs(nx),pitch
real*8 mble,mbte,mles,mtes,mslehub
real*8 xnorm(nx,nax),rnorm(nx,nax)
real*8 deltan, dxn(nx,nax), drn(nx,nax)
real*8 xhub(nx,nax),rhub(nx,nax),mphub(nx,nax)
real*8 xtip(nx,nax),rtip(nx,nax),mptip(nx,nax)
real*8 xt(nx,1),rt(nx,1)

! Grid variables
! Mayank Sharma - 6/26/2019
!integer,    allocatable         :: np_grid(:)
!real,       allocatable         :: xblade_grid(:,:), yblade_grid(:,:), chrdx_grid(:), thkc_grid(:), msle_grid(:), &
!                                   mste_grid(:), mble_grid(:), mbte_grid(:)
real,   allocatable             :: xblade_grid(:,:), yblade_grid(:,:), zblade_grid(:,:)
real,   allocatable             :: mblade_grid(:,:), thblade_grid(:,:)

!-----------------------------------------------------------------------------------------------------
real*8,dimension(:,:),allocatable::cp_chord_curv,bspline_chord_curv,cp_chord_thk,bspline_thk,cp_LE,bspline_LE
real*8,dimension(:),allocatable::section
real*8::delta
character*15::istr1,istr2,H(13)
!-----------------------------------------------------------------------------------------------------
!Function variables
!real*8 inBetaInci, outBetaDevn

logical isdev, tm_c_spline, is_xyzstreamlines, spanwise_angle_spline,spanwise_inci_dev_spline, &
is2d, isquiet!, isxygrid
logical :: u_max_spline = .false.

common / bladesectionpoints /xxa(nx,nax),yya(nx,nax)
!

END MODULE globvar

!**************************************************************
!**************************************************************

MODULE gridvar
implicit none

integer R1cell,R2cell,R3cell,R4cell,jcells,nx,ny,ng
integer i_thetamax,i_loc(1), p1,p2,p3,p4,p5
parameter(nx=1000,ng=5,ny=100)
real*8, allocatable, dimension(:):: xline,yline
real*8, allocatable,dimension(:) :: xb1,yb1,xb2,yb2
real*8, allocatable, dimension(:):: xmeanline,ymeanline
real*8, allocatable, dimension(:):: xbnew1,ybnew1,xbnew2,ybnew2,xbmean,ybmean
real*8 theta_LE,theta_TE,theta_max,dtheta1,dtheta2,value1,value2,value3,value4,value5
real*8 mprime_LE,mprime_TE,mprime_thetamax,dmprime1,dmprime2,atan2,valueMax,maxcurv,mincurv

! sting LE variables
integer nstingtop,nstingbot,nsting1,nsting2,njoint,nstinggrid
real*8 xgtrans,xoffset,yoffset,delx
real*8 stingtop_cellsize,stingbot_cellsize,slopetop,slopebot,ctop,cbot
real*8, allocatable, dimension(:) :: xbstingtop,ybstingtop,xbstingbot,ybstingbot
real*8, allocatable, dimension(:) :: xbot,ybot,xst,yst
real*8, allocatable, dimension(:,:) :: Xgstingtop,Ygstingtop,Xgstingbot,Ygstingbot
real*8, allocatable, dimension(:,:) :: Xgstingdash,Ygstingdash,Xgstingblade,Ygstingblade
!character*32 fext
! offset variables
real*8 s(nx),dxds(nx),dyds(nx),xnorm(nx),ynorm(nx),xn1,yn1,xn2,yn2
real*8 a,b,offset,deltan,Dr,dxn(nx),dyn(nx),hyp

END MODULE gridvar
