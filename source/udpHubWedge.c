/*
 ************************************************************************
 *                                                                      *
 * udpHubWedge -- udp file to generate a wedge for a blade              *       
 *                                                                      *
 *       Written by Simon Livingston @ University of Cincinnati         *
 *       Modified by Mayank Sharma @ University of Cincinnati           *
 *       Patterned after code written by John Dannenhoffer @            *
 *       Syracuse University                                            *
 *       Patterned after code written by Bob Haimes  @ MIT              *
 *                                                                      *
 ************************************************************************
 */

/*
 * Copyright (C) 2011/2017  John F. Dannenhoffer, III (Syracuse University)
 *
 * This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA  02110-1301  USA
 */

#define NUMUDPARGS 39
#include "udpUtilities.h"

/* shorthands for accessing argument values and velocities */
#define NCP(                IUDP)   ((int    *) (udps[IUDP].arg[ 0].val))[0]
#define FILENAME(           IUDP)   ((char   *) (udps[IUDP].arg[ 1].val))
#define AUXNAME(            IUDP)   ((char   *) (udps[IUDP].arg[ 2].val))
#define AVERAGE(            IUDP)   ((char   *) (udps[IUDP].arg[ 3].val))
#define ARG_2(              IUDP)   ((char   *) (udps[IUDP].arg[ 4].val))
#define SPAN_CHORD_CTRL(    IUDP,I) ((double *) (udps[IUDP].arg[ 5].val))[I]
#define SPAN_THK_C_CTRL(    IUDP,I) ((double *) (udps[IUDP].arg[ 6].val))[I]
#define SPAN_IN_BETA_CTRL(  IUDP,I) ((double *) (udps[IUDP].arg[ 7].val))[I]
#define SPAN_OUT_BETA_CTRL( IUDP,I) ((double *) (udps[IUDP].arg[ 8].val))[I]
#define SPAN_CHORD(         IUDP,I) ((double *) (udps[IUDP].arg[ 9].val))[I]
#define SPAN_THK_C(         IUDP,I) ((double *) (udps[IUDP].arg[10].val))[I]
#define SPAN_U_MAX(         IUDP,I) ((double *) (udps[IUDP].arg[11].val))[I]
#define SPAN_IN_BETA(       IUDP,I) ((double *) (udps[IUDP].arg[12].val))[I]
#define SPAN_OUT_BETA(      IUDP,I) ((double *) (udps[IUDP].arg[13].val))[I]
#define U2(                 IUDP,I) ((double *) (udps[IUDP].arg[14].val))[I]
#define U3(                 IUDP,I) ((double *) (udps[IUDP].arg[15].val))[I]
#define U4(                 IUDP,I) ((double *) (udps[IUDP].arg[16].val))[I]
#define U5(                 IUDP,I) ((double *) (udps[IUDP].arg[17].val))[I]
#define U6(                 IUDP,I) ((double *) (udps[IUDP].arg[18].val))[I]
#define CUR1(               IUDP,I) ((double *) (udps[IUDP].arg[19].val))[I]
#define CUR2(               IUDP,I) ((double *) (udps[IUDP].arg[20].val))[I]
#define CUR3(               IUDP,I) ((double *) (udps[IUDP].arg[21].val))[I]
#define CUR4(               IUDP,I) ((double *) (udps[IUDP].arg[22].val))[I]
#define CUR5(               IUDP,I) ((double *) (udps[IUDP].arg[23].val))[I]
#define CUR6(               IUDP,I) ((double *) (udps[IUDP].arg[24].val))[I]
#define CUR7(               IUDP,I) ((double *) (udps[IUDP].arg[25].val))[I]
#define SPAN_CURV_CTRL(     IUDP,I) ((double *) (udps[IUDP].arg[26].val))[I]
#define SPAN_DEL_M_CTRL(    IUDP,I) ((double *) (udps[IUDP].arg[27].val))[I]
#define SPAN_DEL_THETA_CTRL(IUDP,I) ((double *) (udps[IUDP].arg[28].val))[I]
#define SPAN_DEL_M(         IUDP,I) ((double *) (udps[IUDP].arg[29].val))[I]
#define SPAN_DEL_THETA(     IUDP,I) ((double *) (udps[IUDP].arg[30].val))[I]
#define SPAN_THK_CTRL(      IUDP,I) ((double *) (udps[IUDP].arg[31].val))[I]
#define OFFSETS(            IUDP,I) ((double *) (udps[IUDP].arg[32].val))[I] 
#define HUB_INF_OFFSET(     IUDP,I) ((double *) (udps[iUDP].arg[33].val))[I]
#define TIP_INF_OFFSET(     IUDP,I) ((double *) (udps[iUDP].arg[34].val))[I]
#define NACA_LE_RADIUS(     IUDP,I) ((double *) (udps[IUDP].arg[35].val))[I]
#define NACA_U_MAX(         IUDP,I) ((double *) (udps[IUDP].arg[36].val))[I]
#define NACA_T_MAX(         IUDP,I) ((double *) (udps[IUDP].arg[37].val))[I]
#define NACA_T_TE(          IUDP,I) ((double *) (udps[IUDP].arg[38].val))[I]

/* data about possible arguments */
static char*  argNames[NUMUDPARGS] = {"ncp",                    "filename",         "auxname",          "average",
                                      "arg_2",                  "span_chord_ctrl",  "span_thk_c_ctrl",  "span_in_beta_ctrl",
                                      "span_out_beta_ctrl",     "span_chord",       "span_thk_c",       "span_u_max",
                                      "span_in_beta",           "span_out_beta",    "u2",               "u3",               
                                      "u4",                     "u5",               "u6",               "cur1",             
                                      "cur2",                   "cur3",             "cur4",             "cur5",             
                                      "cur6",                   "cur7",             "span_curv_ctrl",   "span_del_m_ctrl",  
                                      "span_del_theta_ctrl",    "span_del_m",       "span_del_theta",   "span_thk_ctrl",    
                                      "offsets",                "hub_inf_offset",   "tip_inf_offset",   "naca_le_radius",   
                                      "naca_u_max",             "naca_t_max",       "naca_t_te",        };
static int    argTypes[NUMUDPARGS] = {ATTRINT,      ATTRSTRING, ATTRSTRING, ATTRSTRING, 
                                      ATTRSTRING,   ATTRREAL,   ATTRREAL,   ATTRREAL,   
                                      ATTRREAL,     ATTRREAL,   ATTRREAL,   ATTRREAL,   
                                      ATTRREAL,     ATTRREAL,   ATTRREAL,   ATTRREAL,   
                                      ATTRREAL,     ATTRREAL,   ATTRREAL,   ATTRREAL,   
                                      ATTRREAL,     ATTRREAL,   ATTRREAL,   ATTRREAL,   
                                      ATTRREAL,     ATTRREAL,   ATTRREAL,   ATTRREAL,   
                                      ATTRREAL,     ATTRREAL,   ATTRREAL,   ATTRREAL,   
                                      ATTRREAL,     ATTRREAL,   ATTRREAL,   ATTRREAL,       
                                      ATTRREAL,     ATTRREAL,   ATTRREAL,   };
static int    argIdefs[NUMUDPARGS] = {33,       0,          0,          0,
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,      
                                      0,        0,          0,          };
static double argDdefs[NUMUDPARGS] = {33.,      0.,         0.,         0.,
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         0.,     
                                      0.,       0.,         0.,         };

/* get utility routines: udpErrorStr, udpInitialize, udpReset, udpSet,
                         udpGet, udpVel, udpClean, udpMesh */
#include "udpUtilities.c"
#include <assert.h>

#ifdef GRAFIC
   #include "grafic.h"
   #define DEBUG
#endif
#define DEBUG

/***********************************************************************/
/*                                                                     */
/* declarations                                                        */
/*                                                                     */
/***********************************************************************/

#define           HUGEQ           99999999.0
#define           PIo2            1.5707963267948965579989817
#define           EPS06           1.0e-06
#define           EPS12           1.0e-12
#define           MIN(A,B)        (((A) < (B)) ? (A) : (B))
#define           MAX(A,B)        (((A) < (B)) ? (B) : (A))
#define           SQR(A)          ((A) * (A))

extern void   bgb3d_sub_(char fname[], char sname[], char arg2[], char arg3[], char arg4[],
                         int len_fname, int len_sname, int len_arg2, int len_arg3, int len_arg4);

void   override_span_chord_ctrl_(    int *nspn, double span_chord_ctrl[     ]);
void   override_span_thk_c_ctrl_(    int *nspn, double span_thk_c_ctrl[     ]);
void   override_span_in_beta_ctrl_(  int *nspn, double span_in_beta_ctrl[   ]);
void   override_span_out_beta_ctrl_( int *nspn, double span_out_beta_ctrl[  ]);
void   override_span_chord_(         int *nspn, double span_chord[          ]);
void   override_span_thk_c_(         int *nspn, double span_thk_c[          ]);
void   override_span_u_max_(         int *nspn, double span_u_max[          ]);
void   override_span_in_beta_(       int *nspn, double span_in_beta[        ]);
void   override_span_out_beta_(      int *nspn, double span_out_beta[       ]);
void   override_u2_(                 int *nspn, double u2[                  ]);
void   override_u3_(                 int *nspn, double u3[                  ]);
void   override_u4_(                 int *nspn, double u4[                  ]);
void   override_u5_(                 int *nspn, double u5[                  ]);
void   override_cur1_(               int *nspn, double cur1[                ]);
void   override_cur2_(               int *nspn, double cur2[                ]);
void   override_cur3_(               int *nspn, double cur3[                ]);
void   override_cur4_(               int *nspn, double cur4[                ]);
void   override_cur5_(               int *nspn, double cur5[                ]);
void   override_cur6_(               int *nspn, double cur6[                ]);
void   override_cur7_(               int *nspn, double cur7[                ]);
void   override_span_curv_ctrl_(     int *nspn, double span_curv_ctrl[      ]);
void   override_span_del_m_ctrl_(    int *nspn, double span_del_m_ctrl[     ]);
void   override_span_del_theta_ctrl_(int *nspn, double span_del_theta_ctrl[ ]);
void   override_span_del_m_(         int *nspn, double span_del_m[          ]);
void   override_span_del_theta_(     int *nspn, double span_del_theta[      ]);
void   override_span_thk_ctrl_(      int *nspn, double span_thk_ctrl[       ]);
void   override_offsets_(                       double offsets[             ]);
void   override_hub_inf_offset_(                double hub_inf_offset[      ]);
void   override_tip_inf_offset_(                double tip_inf_offset[      ]);
void   override_naca_le_radius_(     int *nspn, double naca_le_radius[      ]);
void   override_naca_u_max_(         int *nspn, double naca_u_max[          ]);
void   override_naca_t_max_(         int *nspn, double naca_t_max[          ]);
void   override_naca_t_te_(          int *nspn, double naca_t_te[           ]);

/*static int    EG_fitBspline(ego context,
                            int npnt, int bitflag, double xyz[],
                            int ncp, ego *ecurve, double *rms);*/
/*static int    fit1dCloud(int m, int ordered, double XYZcloud[],
                         int n, double cp[], double *normf);*/
//static int    eval1dBspline(double T, int n, double cp[], double XYZ[],
//                            /*@null@*/double dXYZdT[], /*@null@*/double dXYZdP[]);
//static int    cubicBsplineBases(int ncp, double t, double N[], double dN[]);
//static int    solveSparse(double SAv[], int SAi[], double b[], double x[],
//                          int itol, double *errmax, int *iter);
//static double L2norm(double f[], int n);

#ifdef GRAFIC
static int    plotCurve(int npnt, double xyz[], ego ecurve);
#endif


/*
 ************************************************************************
 *                                                                      *
 *   udpExecute - execute the primitive                                 *
 *                                                                      *
 ************************************************************************
 */

int
udpExecute(ego  context,                /* (in)  EGADS context */
           ego  *ebody,                 /* (out) Body pointer */
           int  *nMesh,                 /* (out) number of associated meshes */
           char *string[])              /* (out) error message */
{
    int     status = EGADS_SUCCESS;

#define NPNT 1000
    int     nsec, isec, npnt, /*ipnt,ipnt2, */nn, /*periodic, */*senses/*[4]*/, sense[8],oclass,mtype,nchild,/*nchilds,*/sizes[2];
    double  /*xyz[3*NPNT], rms, xyzNode[18],*/data[6],/*data2[12],*/node0[3],node1[3],/*node2[3],node3[3],node4[3],node5[3],*/xyz2[3*NPNT],/*xyz3[3*NPNT],*/angle,bladespace,nblades/*,data3[4]*/;
    double  trange[2] /*,trange1[2], trange2[2]*/;
    double  /*xsave, ysave, zsave, */xmin, ymin, zmin, xmax, ymax, zmax;
    char    filename[300], nextline[257], casename[257];
    FILE    *fp,*hub,/**casing,*/*meanline;
    ego     ecurve[20], enodes[5], eedges[4], /*eloop,*/eloops[20], efaces[20],/***/body,/*bodies,*/ebody2[10],/**ebody3,*/emodel,*echilds2,/*source,*echilds,*/eref,/*meansurface,transform,*/*children;

#ifdef DEBUG
    printf("udpExecute(context=%llx)\n", (long long)context);
    printf("ncp(     0) = %d\n", NCP(     0));
    printf("filename(0) = %s\n", FILENAME(0));
    printf("auxname( 0) = %s\n", AUXNAME( 0));
    printf("average( 0) = %s\n", AVERAGE( 0));
#endif

    /* default return values */
    *ebody  = NULL;
    *nMesh  = 0;
    *string = NULL;

    /* check arguments */

    /* cache copy of arguments for future use */
    status = cacheUdp();
    if (status < 0) {
        printf(" udpExecute: problem caching arguments\n");
    }

#ifdef DEBUG
    printf("ncp(     %d) = %d\n", numUdp, NCP(     numUdp));
    printf("filename(%d) = %s\n", numUdp, FILENAME(numUdp));
    printf("auxname( %d) = %s\n", numUdp, AUXNAME( numUdp));
    printf("average( %d) = %s\n", numUdp, AVERAGE( numUdp));
#endif

    /* open the input file to extract the casename and number of sections */
    fp = fopen(FILENAME(numUdp), "r");
    if (fp == NULL) {
        printf("could not open \"%s\"\n", FILENAME(numUdp));
        status = -9999;
    }

    /* read the file line by line */
    (void) fgets(nextline, 257, fp);
    (void) fgets(nextline, 257, fp);
    sscanf(nextline, "%s", casename);

    (void) fgets(nextline, 257, fp);
    (void) fgets(nextline, 257, fp);

    (void) fgets(nextline, 257, fp);
    (void) fgets(nextline, 257, fp);
    sscanf(nextline, "%lf", &nblades);
    printf("Number of Blades in Row: %lf\n", nblades);

    (void) fgets(nextline, 257, fp);
    (void) fgets(nextline, 257, fp);

    (void) fgets(nextline, 257, fp);
    (void) fgets(nextline, 257, fp);
    sscanf(nextline, "%d", &nsec);

    fclose(fp);

    /* try running Tblade */
    printf("filename = %s\n", FILENAME(numUdp));
    printf("auxname  = %s\n", AUXNAME(numUdp));
    bgb3d_sub_(FILENAME(numUdp), AUXNAME(numUdp), ARG_2(numUdp), "", "",
               strlen(FILENAME(numUdp)), strlen(AUXNAME(numUdp)), (int)strlen(ARG_2(numUdp)), strlen(""), strlen(""));
    /* Read Hub File */        
    sprintf(filename, "hub.%s.sldcrv", casename);
    printf("reading: %s\n", filename);
    
    hub = fopen(filename, "r");
    fp  = hub;      //Might need to remove
    if (fp == NULL) {
        printf("could not open \"%s\"\n", filename);
        status = -9999;
    }          

    /* Read Meanline Files and create curves */
    for (isec = 1; isec <= 1; isec++) {
    
        sprintf(filename, "meanline.sec%d.%s.dat",isec,casename);
        printf("reading: %s\n", filename);
        meanline = fopen(filename, "r");
        if (fp == NULL) {
            printf("could not open \"%s\"\n", filename);
            status = -9999;
        }
        
        /*Filling xyz2 array with points in meanline file*/
        npnt = 0;
        while (1) {
            nn = fscanf(meanline, "%lf %lf %lf\n", &(xyz2[3*npnt]), &(xyz2[3*npnt+1]), &(xyz2[3*npnt+2]));  
            if (nn < 3) {
                break;
            }else if (npnt >= NPNT) {
                printf("recompile with larger NPNT\n");
                status = -9999;
            } else {
                npnt++;
            }
        }
        
        /*Finding xyz positions of the first and last points*/
        xmax=xyz2[3*(npnt-1)];
        ymax=xyz2[3*(npnt-1)+1];
        zmax=xyz2[3*(npnt-1)+2];
        xmin=xyz2[0];
        ymin=xyz2[1];
        zmin=xyz2[2];
        
        /*Creating a node at the first and last points*/        
        node0[0]=xmin;//xyz2[0];
        node0[1]=ymin;//xyz2[1];
        node0[2]=zmin;//xyz2[2];
    
        node1[0]=xmax;//xyz2[3*(npnt-1)];
        node1[1]=ymax;//xyz2[3*(npnt-1)+1];
        node1[2]=zmax;//xyz2[3*(npnt-1)+2];
    
        status = EG_makeTopology(context, NULL, NODE, 0, node0, 0, NULL, NULL, &(enodes[0]));
        #ifdef DEBUG
            printf("EG_makeTopology(node0) -> status=%d\n", status);
        #endif

        status = EG_makeTopology(context, NULL, NODE, 0, node1, 0, NULL, NULL, &(enodes[1]));
        #ifdef DEBUG
            printf("EG_makeTopology(node1) -> status=%d\n", status);
        #endif
    
        
        /*Fit B-Spline to Points*/
        sizes[0]=npnt;
        sizes[1]=0; 
        status=EG_approximate(context, 2, 1.0e-6,sizes, xyz2,&(ecurve[0]));
        #ifdef DEBUG
        printf("EG_approximate -> status=%d\n", status);
        #endif
        
        /*Get Ranges for Edge*/
        status = EG_invEvaluate(ecurve[0], node0, &(trange[0]), data);
        #ifdef DEBUG    
        printf("EG_invEvaluate -> status=%d\n", status);
        #endif
        status = EG_invEvaluate(ecurve[0], node1, &(trange[1]), data);  
        #ifdef DEBUG
        printf("EG_invEvaluate -> status=%d\n", status);
        #endif
        
        /*Make the Meanline an Edge*/
        status = EG_makeTopology(context, ecurve[0], EDGE, TWONODE, trange, 2, &(enodes[0]), NULL, &(eedges[0]));
        #ifdef DEBUG        
        printf("EG_makeTopology(edge) -> status=%d\n", status); 
        #endif

        /* make Loop from this Edge */
        sense[0] = SFORWARD;
        sense[1] = SFORWARD;
        status = EG_makeTopology(context, NULL, LOOP, OPEN, NULL, 1, eedges, sense, &(eloops[0]));
        #ifdef DEBUG        
        printf("EG_makeTopology(loop) -> status=%d\n", status); 
        #endif

        fclose(meanline);   
    }
    
    /*Making the Nodes for the Line along the X-axis-the .0001 is because ESP 
    runs into an error when I use 0 */
    node0[0]=xmin;
    node0[1]=0;
    node0[2]=.0001;

    node1[0]=xmax;
    node1[1]=0;
    node1[2]=.0001;
    
    status = EG_makeTopology(context, NULL, NODE, 0, node0, 0, NULL, NULL, &(enodes[0]));
    #ifdef DEBUG
        printf("EG_makeTopology(node0) -> status=%d\n", status);
    #endif
    
    status = EG_makeTopology(context, NULL, NODE, 0, node1, 0, NULL, NULL, &(enodes[1]));
    #ifdef DEBUG
        printf("EG_makeTopology(node1) -> status=%d\n", status);
    #endif
    
    /*Making the Straight curve along the x-axis*/
    data[0] = node0[0];
    data[1] = node0[1];
    data[2] = node0[2];
    data[3] = node1[0] - node0[0];
    data[4] = node1[1] - node0[1];
    data[5] = node1[2] - node0[2];

    status = EG_makeGeometry(context, CURVE, LINE, NULL, NULL, data, &(ecurve[0]));
    #ifdef DEBUG
        printf("EG_makeGeometry(curve) -> status=%d\n", status);
    #endif

    /*Finding the range between the nodes*/ 
    status = EG_invEvaluate(ecurve[0], node0, &(trange[0]), data);
    #ifdef DEBUG
        printf("invEvaluate -> status=%d\n", status);
    #endif
    status = EG_invEvaluate(ecurve[0], node1, &(trange[1]), data);  
    #ifdef DEBUG
        printf("EG_invEvaluate -> status=%d\n", status);
    #endif
    
    /*Make the Edge*/
    status = EG_makeTopology(context, ecurve[0], EDGE, TWONODE, trange, 2, &(enodes[0]), NULL, &(eedges[0]));   
    #ifdef DEBUG
        printf("EG_makeTopology(edge) -> status=%d\n", status);
    #endif

    /* make Loop from this Edge */
    sense[0] = SFORWARD;
    sense[1] = SFORWARD;
    status = EG_makeTopology(context, NULL, LOOP, OPEN, NULL, 1, eedges, sense, &(eloops[1]));
    #ifdef DEBUG
        printf("EG_makeTopology(loop) -> status=%d\n", status);
    #endif

    /*Blend the meanline edge with the edge along the x-axis*/
    status=EG_blend(2,eloops,NULL,NULL,&body);
    #ifdef DEBUG
        printf("EG_blend -> status=%d\n", status);
    #endif
    
    /*Get the from the sheet body so that it can be revolved*/
    status=EG_getTopology(body, &eref,&oclass,&mtype,data,&nchild,&children,&senses);
    #ifdef DEBUG
        printf("EG_getTopology -> status=%d\n", status);
    #endif
    status=EG_getTopology(children[0], &eref,&oclass,&mtype,data,&nchild,&children,&senses);
    #ifdef DEBUG
        printf("EG_getTopology -> status=%d\n", status);
    #endif
    
    /*Create another instance of the face*/
    status=EG_copyObject(children[0], NULL,&(efaces[0]));
    #ifdef DEBUG
        printf("EG_copyObject -> status=%d\n", status);
    #endif
    
    /*Determine the spacing between blades*/
    bladespace=360/nblades;
    angle=bladespace/2;
    #ifdef DEBUG
    printf("Bladespace = %lf\n", bladespace);
    printf("nblades = %lf\n", nblades);
    #endif
    
    /*Revolve Face around x-axis to Get half of Solid Body*/
    data[0]=-1;
    data[1]=0;
    data[2]=0;
    data[3]=1;
    data[4]=0;
    data[5]=0;
    
    
    status=EG_rotate(children[0],angle,data, &(ebody2[1]));
    #ifdef DEBUG
        printf("EG_rotate -> status=%d\n", status);
    #endif
    
    /*Rotate the other face instance the other way to get other half*/
    data[0]=100;
    data[1]=0;
    data[2]=0;
    data[3]=-100;
    data[4]=0;
    data[5]=0;
    status=EG_rotate(efaces[0],angle,data, &(ebody2[0]));
    #ifdef DEBUG
        printf("EG_rotate -> status=%d\n", status);
    #endif
    
    /*Fuse the two halfs together*/
    status=EG_solidBoolean(ebody2[0],ebody2[1],FUSION,&emodel);
    #ifdef DEBUG
        printf("EG_solidBoolean -> status=%d\n", status);
    #endif
    
    /*Extract the one body from the model created*/
    status=EG_getTopology(emodel, &eref,&oclass,&mtype,data,&nchild,&echilds2,&senses);

    #ifdef DEBUG
        printf("EG_getTopology -> status=%d\n", status);
    #endif
    status=EG_copyObject(echilds2[0], NULL,ebody);
    #ifdef DEBUG
        printf("EG_copyObject-> status=%d\n", status);
    #endif

    /*Remembder the body*/
    udps[numUdp].ebody = *ebody;
/*
 *************************************************************************/

/*cleanup:
    if (status != EGADS_SUCCESS) {
        *string = udpErrorStr(status);
    }*/ 
    
    return status;
}

/*
 ************************************************************************
 *                                                                      *
 *   udpSensitivity - return sensitivity derivatives for the "real" argument *
 *                                                                      *
 ************************************************************************
 */

int
udpSensitivity(ego    ebody,            /* (in)  Body pointer */
   /*@unused@*/int    npnt,             /* (in)  number of points */
   /*@unused@*/int    entType,          /* (in)  OCSM entity type */
   /*@unused@*/int    entIndex,         /* (in)  OCSM entity index (bias-1) */
   /*@unused@*/double uvs[],            /* (in)  parametric coordinates for evaluation */
   /*@unused@*/double vels[])           /* (out) velocities */
{
    int    iudp, judp;

#ifdef DEBUG
    printf("udpSensitivity(ebody=%llx, npnt=%d, entType=%d, entIndex=%d, uvs=%f %f)\n",
           (long long)ebody, npnt, entType, entIndex, uvs[0], uvs[1]);
#endif

    /* check that ebody matches one of the ebodys */
    iudp = 0;
    for (judp = 1; judp <= numUdp; judp++) {
        if (ebody == udps[judp].ebody) {
            iudp = judp;
            break;
        }
    }
    if (iudp <= 0) {
        return EGADS_NOTMODEL;
    }

    /* this routine is not written yet */
    return EGADS_NOLOAD;
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_chord_ctrl - callback from Tblade3 to change         *
 *                              span_chord_ctrl array                   *
 *                                                                      *
 ************************************************************************
 */

void override_span_chord_ctrl_(int *nspn, double span_chord_ctrl[])
{
    int    ispn, narg=5;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_chord_ctrl\n");
        for (ispn=0; ispn < *nspn; ispn++) {
            span_chord_ctrl[ispn] = SPAN_CHORD_CTRL(numUdp,ispn);
            printf("     span_chord_ctrl(%2d) = %12.5f\n", ispn+1, span_chord_ctrl[ispn]);
        }
    } else {
        printf(" ==> not overriding span_chord_ctrl (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_thk_c_ctrl - callback from Tblade3 to change         *
 *                              span_thk_c_ctrl array                   *
 *                                                                      *
 ************************************************************************
 */

void override_span_thk_c_ctrl_(int *nspn, double span_thk_c_ctrl[])
{
    int    ispn, narg=6;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_thk_c_ctrl\n");
        for (ispn=0; ispn < *nspn; ispn++) {
            span_thk_c_ctrl[ispn] = SPAN_THK_C_CTRL(numUdp,ispn);
            printf("     span_thk_c_ctrl(%2d) = %12.5f\n", ispn+1, span_thk_c_ctrl[ispn]);
        }
    } else {
        printf(" ==> not overriding span_thk_c_ctrl (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_in_beta_ctrl - callback from Tblade3 to change       *
 *                                span_in_beta_ctrl array               *
 *                                                                      *
 ************************************************************************
 */

void override_span_in_beta_ctrl_(int *nspn, double span_in_beta_ctrl[])
{
    int    ispn, narg=7;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_in_beta_ctrl\n");
        for (ispn=0; ispn < *nspn; ispn++) {
            span_in_beta_ctrl[ispn] = SPAN_IN_BETA_CTRL(numUdp,ispn);
            printf("     span_in_beta_ctrl(%2d) = %12.5f\n", ispn+1, span_in_beta_ctrl[ispn]);
        }
    } else {
        printf(" ==> not overriding span_in_beta_ctrl (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_out_beta_ctrl - callback from Tblade3 to change      *
 *                                 span_out_beta_ctrl array             *
 *                                                                      *
 ************************************************************************
 */

void override_span_out_beta_ctrl_(int *nspn, double span_out_beta_ctrl[])
{
    int   ispn, narg=8;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_out_beta_ctrl\n");
        for (ispn=0; ispn < *nspn; ispn++) {
            span_out_beta_ctrl[ispn] = SPAN_OUT_BETA_CTRL(numUdp,ispn);
            printf("     span_out_beta_ctrl(%2d) = %12.5f\n", ispn+1, span_out_beta_ctrl[ispn]);
        }
    } else {
        printf(" ==> not overriding span_out_beta_ctrl (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_chord - callback from Tblade3 to change              *
 *                         span_chord array                             *
 *                                                                      *
 ************************************************************************
 */

void override_span_chord_(int *nspn, double span_chord[])
{
    int    ispn, narg=9;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_chord\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            span_chord[ispn] = SPAN_CHORD(numUdp,ispn);
            printf("     span_chord(%2d) = %12.5f\n", ispn+1, span_chord[ispn]);
        }
    } else {
        printf(" ==> not overriding span_chord (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_thk_c - callback from Tblade3 to change              *
 *                         span_thk_c array                             *
 *                                                                      *
 ************************************************************************
 */

void override_span_thk_c_(int *nspn, double span_thk_c[])
{
    int    ispn, narg=10;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_thk_c\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            span_thk_c[ispn] = SPAN_THK_C(numUdp,ispn);
            printf("     span_thk_c(%2d) = %12.5f\n", ispn+1, span_thk_c[ispn]);
        }
    } else {
        printf(" ==> not overriding span_thk_c (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_u_max - callback from Tblade3 to change              *
 *                         span_u_max array                             *
 *                                                                      *
 ************************************************************************
 */

void override_span_u_max_(int *nspn, double span_u_max[])
{
    int    ispn, narg=11;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overrdiing span_u_max\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            span_u_max[ispn] = SPAN_U_MAX(numUdp,ispn);
            printf("     span_u_max(%2d) = %12.5f\n", ispn+1, span_u_max[ispn]);
            }
        } else {
            printf(" ==> not overriding span_u_max (nspn=%d but size=%d)\n",
                  *nspn, udps[numUdp].arg[narg].size);
            }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_in_beta - callback from Tblade3 to change            *
 *                           span_in_beta array                         *
 *                                                                      *
 ************************************************************************
 */

void override_span_in_beta_(int *nspn, double span_in_beta[])
{
    int    ispn, narg=12;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_in_beta\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            span_in_beta[ispn] = SPAN_IN_BETA(numUdp,ispn);
            printf("     span_in_beta(%2d) = %12.5f\n", ispn+1, span_in_beta[ispn]);
        }
    } else {
        printf(" ==> not overriding span_in_beta (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_out_beta - callback from Tblade3 to change           *
 *                            span_out_beta array                       *
 *                                                                      *
 ************************************************************************
 */

void override_span_out_beta_(int *nspn, double span_out_beta[])
{
    int    ispn, narg=13;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_out_beta\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            span_out_beta[ispn] = SPAN_OUT_BETA(numUdp,ispn);
            printf("     span_out_beta(%2d) = %12.5f\n", ispn+1, span_out_beta[ispn]);
        }
    } else {
        printf(" ==> not overriding span_out_beta (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_u2 - callback from Tblade3 to change u2 array             *
 *                                                                      *
 ************************************************************************
 */

void override_u2_(int *nspn, double u2[])
{
    int    ispn, narg=14;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u2\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u2[ispn] = U2(numUdp,ispn);
            printf("    u2(%2d) = %12.5f\n", ispn+1, u2[ispn]);
        }
    } else {
        printf(" ==> not overriding u2 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_u3 - callback from Tblade3 to change u3 array             *
 *                                                                      *
 ************************************************************************
 */

void override_u3_(int *nspn, double u3[])
{
    int    ispn, narg=15;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u3\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u3[ispn] = U3(numUdp,ispn);
            printf("    u3(%2d) = %12.5f\n", ispn+1, u3[ispn]);
        }
    } else {
        printf(" ==> not overriding u3 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_u4 - callback from Tblade3 to change u4 array             *
 *                                                                      *
 ************************************************************************
 */

void override_u4_(int *nspn, double u4[])
{
    int    ispn, narg=16;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u4\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u4[ispn] = U4(numUdp,ispn);
            printf("    u4(%2d) = %12.5f\n", ispn+1, u4[ispn]);
        }
    } else {
        printf(" ==> not overriding u4 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_u5 - callback from Tblade3 to change u5 array             *
 *                                                                      *
 ************************************************************************
 */

void override_u5_(int *nspn, double u5[])
{
    int    ispn, narg=17;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u5\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u5[ispn] = U5(numUdp,ispn);
            printf("    u5(%2d) = %12.5f\n", ispn+1, u5[ispn]);
        }
    } else {
        printf(" ==> not overriding u5 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_u6 - callback from Tblade3 to change u6 array             *
 *                                                                      *
 ************************************************************************
 */

void override_u6_(int *nspn, double u6[])
{
    int    ispn, narg=18;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u6\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u6[ispn] = U6(numUdp,ispn);
            printf("    u6(%2d) = %12.5f\n", ispn+1, u6[ispn]);
        }
    } else {
        printf(" ==> not overriding u6 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_cur1 - callback from Tblade3 to change cur1 array         *
 *                                                                      *
 ************************************************************************
 */

void override_cur1_(int *nspn, double cur1[])
{
    int    ispn, narg=19;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur1\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur1[ispn] = CUR1(numUdp,ispn);
            printf("     cur1(%2d) = %12.5f\n", ispn+1, cur1[ispn]);
        }
    } else {
        printf(" ==> not overriding cur1 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_cur2 - callback from Tblade3 to change cur2 array         *
 *                                                                      *
 ************************************************************************
 */

void override_cur2_(int *nspn, double cur2[])
{
    int    ispn, narg=20;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur2\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur2[ispn] = CUR2(numUdp,ispn);
            printf("     cur2(%2d) = %12.5f\n", ispn+1, cur2[ispn]);
        }
    } else {
        printf(" ==> not overriding cur2 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_cur3 - callback from Tblade3 to change cur3 array         *
 *                                                                      *
 ************************************************************************
 */

void override_cur3_(int *nspn, double cur3[])
{
    int    ispn, narg=21;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur3\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur3[ispn] = CUR3(numUdp,ispn);
            printf("     cur3(%2d) = %12.5f\n", ispn+1, cur3[ispn]);
        }
    } else {
        printf(" ==> not overriding cur3 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_cur4 - callback from Tblade3 to change cur4 array         *
 *                                                                      *
 ************************************************************************
 */

void override_cur4_(int *nspn, double cur4[])
{
    int    ispn, narg=22;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur4\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur4[ispn] = CUR4(numUdp,ispn);
            printf("     cur4(%2d) = %12.5f\n", ispn+1, cur4[ispn]);
        }
    } else {
        printf(" ==> not overriding cur4 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_cur5 - callback from Tblade3 to change cur5 array         *
 *                                                                      *
 ************************************************************************
 */

void override_cur5_(int *nspn, double cur5[])
{
    int    ispn, narg=23;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur5\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur5[ispn] = CUR5(numUdp,ispn);
            printf("     cur5(%2d) = %12.5f\n", ispn+1, cur5[ispn]);
        }
    } else {
        printf(" ==> not overriding cur5 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_cur6 - callback from Tblade3 to change cur6 array         *
 *                                                                      *
 ************************************************************************
 */

void override_cur6_(int *nspn, double cur6[])
{
    int    ispn, narg=24;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur6\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur6[ispn] = CUR6(numUdp,ispn);
            printf("     cur6(%2d) = %12.5f\n", ispn+1, cur6[ispn]);
        }
    } else {
        printf(" ==> not overriding cur6 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_cur7 - callback from Tblade3 to change cur7 array         *
 *                                                                      *
 ************************************************************************
 */

void override_cur7_(int *nspn, double cur7[])
{
    int    ispn, narg=25;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur7\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur7[ispn] = CUR7(numUdp,ispn);
            printf("     cur7(%2d) = %12.5f\n", ispn+1, cur7[ispn]);
        }
    } else {
        printf(" ==> not overriding cur7 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_curv_ctrl - callback from Tblade3 to change          *
 *                             span_curv_ctrl array                     *
 *                                                                      *
 ************************************************************************
 */

void override_span_curv_ctrl_(int *nspn, double span_curv_ctrl[])
{
    int    ispn, narg=26;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_curv_ctrl\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            span_curv_ctrl[ispn] = SPAN_CURV_CTRL(numUdp,ispn);
            printf("     span_curv_ctrl(%2d) = %12.5f\n", ispn+1, span_curv_ctrl[ispn]);
        }
    } else {
        printf(" ==> not overriding span_curv_ctrl (nspn=%d but size=%d)\n",
              *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_del_m_ctrl - callback from Tblade3 to change         *
 *                              span_del_m_ctrl array                   *
 *                                                                      *
 ************************************************************************
 */

void override_span_del_m_ctrl_(int *nspn, double span_del_m_ctrl[])
{
    int    ispn, narg=27;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_del_m_ctrl\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            span_del_m_ctrl[ispn] = SPAN_DEL_M_CTRL(numUdp,ispn);
            printf("     span_del_m_ctrl(%2d) = %12.5f\n", ispn+1, span_del_m_ctrl[ispn]);
        }
    } else {
        printf(" ==> not overriding span_del_m_ctrl (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_del_theta_ctrl - callback from Tblade3 to change     *
 *                                  span_del_theta_ctrl array           *
 *                                                                      *
 ************************************************************************
 */

void override_span_del_theta_ctrl_(int *nspn, double span_del_theta_ctrl[])
{
    int    ispn, narg=28;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_del_theta_ctrl\n");
        for (ispn=0; ispn < *nspn; ispn++) {
            span_del_theta_ctrl[ispn] = SPAN_DEL_THETA_CTRL(numUdp,ispn);
            printf("     span_del_theta_ctrl(%2d) = %12.5f\n",  ispn+1, span_del_theta_ctrl[ispn]);
        }
    } else {
        printf(" ==> not overriding span_del_theta_ctrl (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_del_m - callback from Tblade3 to change              *
 *                         span_del_m array                             *
 *                                                                      *
 ************************************************************************
 */

void override_span_del_m_(int *nspn, double span_del_m[])
{
    int    ispn, narg=29;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_del_m\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            span_del_m[ispn] = SPAN_DEL_M(numUdp,ispn);
            printf("     span_del_m(%2d) = %12.5f\n", ispn+1, span_del_m[ispn]);
        }
    } else {
        printf(" ==> not overriding span_del_m (nspn=%d but size=%d)\n",
              *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_span_del_theta - callback from Tblade3 to change          *
 *                             span_del_theta array                     *
 *                                                                      *
 ************************************************************************
 */

void override_span_del_theta_(int *nspn, double span_del_theta[])
{
    int    ispn, narg=30;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_del_theta\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            span_del_theta[ispn] = SPAN_DEL_THETA(numUdp,ispn);
            printf("     span_del_theta(%2d) = %12.5f\n", ispn+1, span_del_theta[ispn]);
        }
    } else {
        printf(" ==> not overriding span_del_theta (nspn=%d but size=%d)\n",
              *nspn, udps[numUdp].arg[narg].size);
    }
}

/*
 ************************************************************************
 *                                                                      *
 *   override_span_thk_ctrl - callback from Tblade3 to change           *
 *                            span_thk_ctrl array                       *
 *                                                                      *
 ************************************************************************
 */

void override_span_thk_ctrl_(int *nspn, double span_thk_ctrl[])
{
    int    ispn, narg=31;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding span_thk_ctrl\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            span_thk_ctrl[ispn] = SPAN_THK_CTRL(numUdp,ispn);
            printf("     span_thk_ctrl(%2d) = %12.5f\n", ispn+1, span_thk_ctrl[ispn]);
        }
    } else {
        printf(" ==> not overriding span_thk_ctrl (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_offsets - callback from Tblade3 to change                 *
 *                      offsets array                                   *
 *                                                                      *
 ************************************************************************
 */

void override_offsets_(double offsets[])
{
    int    ioffset, narg=32;

    if (udps[numUdp].arg[narg].size == 2) {
        printf(" ==> overriding offsets\n");
        for (ioffset = 0; ioffset < 2; ioffset++) {
            offsets[ioffset] = OFFSETS(numUdp,ioffset);
            printf("     offsets(%2d) = %12.5f\n", ioffset+1, offsets[ioffset]);
        }
    } else {
        printf(" ==> not overriding offsets (noffset=2 but size=%d)\n",
               udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_hub_inf_offset - callback from Tblade3 to change          *
 *                             hub_inf_offset                           *
 *                                                                      *
 ************************************************************************
 */

void override_hub_inf_offset_(double hub_inf_offset[])
{
    int    ioffset, narg=33;

    if (udps[numUdp].arg[narg].size == 1) {
        printf(" ==> overriding hub_inf_offset\n");
        for (ioffset = 0; ioffset < 1; ioffset++) {
            hub_inf_offset[ioffset] = HUB_INF_OFFSET(numUdp,ioffset);
            printf("     hub_inf_offset(%2d) = %12.5f\n", ioffset+1, hub_inf_offset[ioffset]);
        }
    } else {
        printf(" ==> not overriding hub_inf_offset (noffset=1 but size=%d)\n",
               udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_tip_inf_offset - callback from Tblade3 to change          *
 *                             tip_inf_offset                           *
 *                                                                      *
 ************************************************************************
 */

void override_tip_inf_offset_(double tip_inf_offset[])
{
    int    ioffset, narg=34;

    if (udps[numUdp].arg[narg].size == 1) {
        printf(" ==> overriding tip_inf_offset\n");
        for (ioffset = 0; ioffset < 1; ioffset++) {
            tip_inf_offset[ioffset] = TIP_INF_OFFSET(numUdp,ioffset);
            printf("     tip_inf_offset(%2d) = %12.5f\n", ioffset+1, tip_inf_offset[ioffset]);
        }
    } else {
        printf(" ==> not overriding tip_inf_offset (noffset=1 but size=%d)\n",
               udps[numUdp].arg[narg].size);
    }
}

/*
 ************************************************************************
 *                                                                      *
 *   override_naca_le_radius - callback from Tblade3 to change          *
 *                             naca_le_radius array                     *
 *                                                                      *
 ************************************************************************
 */

void override_naca_le_radius_(int *nspn, double naca_le_radius[])
{
    int    ispn, narg=35;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding naca_le_radius\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            naca_le_radius[ispn] = NACA_LE_RADIUS(numUdp,ispn);
            printf("     naca_le_radius(%2d) = %12.5f\n", ispn+1, naca_le_radius[ispn]);
        }
    } else {
        printf(" ==> not overriding naca_le_radius (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_naca_u_max - callback from Tblade3 to change              *
 *                         naca_u_max array                             *
 *                                                                      *
 ************************************************************************
 */

void override_naca_u_max_(int *nspn, double naca_u_max[])
{
    int    ispn, narg=36;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding naca_u_max\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            naca_u_max[ispn] = NACA_U_MAX(numUdp,ispn);
            printf("     naca_u_max(%2d) = %12.5f\n", ispn+1, naca_u_max[ispn]);
        }
    } else {
        printf(" ==> not overriding naca_u_max (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_naca_t_max - callback from Tblade3 to change              *
 *                         naca_t_max array                             *
 *                                                                      *
 ************************************************************************
 */

void override_naca_t_max_(int *nspn, double naca_t_max[])
{
    int    ispn, narg=37;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding naca_t_max\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            naca_t_max[ispn] = NACA_T_MAX(numUdp,ispn);
            printf("     naca_t_max(%2d) = %12.5f\n", ispn+1, naca_t_max[ispn]);
        }
    } else {
        printf(" ==> not overriding naca_t_max (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_naca_t_te - callback from Tblade3 to change               *
 *                        naca_t_te array                               *
 *                                                                      *
 ************************************************************************
 */

void override_naca_t_te_(int *nspn, double naca_t_te[])
{
    int    ispn, narg=38;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding naca_t_te\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            naca_t_te[ispn] = NACA_T_TE(numUdp,ispn);
            printf("     naca_t_te(%2d) = %12.5f\n", ispn+1, naca_t_te[ispn]);
        }
    } else {
        printf(" ==> not overriding naca_t_te (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}

/*
 ************************************************************************
 *                                                                      *
 *   EG_fitBspline - fit a degree=3 Bspline curve to a set of points    *
 *                                                                      *
 ************************************************************************
 */

/*static int
EG_fitBspline(ego    context,  */           /* (in)  EGADS context */
              /*int    npnt,   */           /* (in)  number of points */
              /*int    bitflag,*/           /* (in)  1=ordered, 2=periodic */
              /*double xyz[],  */           /* (in)  array of points (xyzxyz...) */
              /*int    ncp,    */           /* (in)  number of control points */
              /*ego    *ecurve,*/           /* (out) Bspline curve (degree=3) */
              /*double *rms)   */           /* (out) RMS distance of points from curve */
/*{
    int    status = EGADS_SUCCESS;

    int    nknot, ndata, idata, j, header[4];
    double *cpdata=NULL;*/

    /* --------------------------------------------------------------- */

    /* default returns */
    /**ecurve = NULL;
    *rms    = 0;*/

    /* check the inputs */
    /*if (context == NULL) {
        status = EGADS_NULLOBJ;
        goto cleanup;
    } else if (npnt < 2) {
        status = EGADS_NODATA;
        goto cleanup;
    } else if (xyz == NULL) {
        status = EGADS_NODATA;
        goto cleanup;
    } else if (ncp < 3) {
        status = EGADS_NODATA;
        goto cleanup;
    }*/

    /* set up arrays needed to define Bspline */
    /*nknot = ncp + 4;
    ndata = nknot + 3 * ncp;

    header[0] = 0;            // bitflag
    header[1] = 3;            // degree
    header[2] = ncp;          // number of control points
    header[3] = nknot;        // number of knots

    cpdata = (double *) malloc(ndata*sizeof(double));
    if (cpdata == NULL) {
        status = EGADS_MALLOC;
        goto cleanup;
    }

    ndata = 0;*/

    /* knot vector */
    /*cpdata[ndata++] = 0;
    cpdata[ndata++] = 0;
    cpdata[ndata++] = 0;
    cpdata[ndata++] = 0;

    for (j = 1; j < ncp-3; j++) {
        cpdata[ndata++] = j;
    }

    cpdata[ndata++] = ncp - 3;
    cpdata[ndata++] = ncp - 3;
    cpdata[ndata++] = ncp - 3;
    cpdata[ndata++] = ncp - 3;*/

    /* control points at the two ends */
    /*idata = ndata;
    cpdata[idata  ] = xyz[0];
    cpdata[idata+1] = xyz[1];
    cpdata[idata+2] = xyz[2];

    idata = ndata + 3 * (ncp-1);
    cpdata[idata  ] = xyz[3*npnt-3];
    cpdata[idata+1] = xyz[3*npnt-2];
    cpdata[idata+2] = xyz[3*npnt-1];*/

    /* perform the fitting (which updates the interior control points) */
    /*status = fit1dCloud(npnt, bitflag, xyz,
                        ncp,  &(cpdata[ndata]), rms);
    if (status < EGADS_SUCCESS) goto cleanup;*/

    /* make the geometry */
    /*status = EG_makeGeometry(context, CURVE, BSPLINE, NULL,
                             header, cpdata, ecurve);

cleanup:
    if (cpdata != NULL) free(cpdata);

    return status;
}*/


/*
 ************************************************************************
 *                                                                      *
 *   fit1dCloud - find spline that best-fits the cloud of points        *
 *                                                                      *
 ************************************************************************
 */

/*static int
fit1dCloud(int    m,           */           /* (in)  number of points in cloud */
           /*int    bitflag,   */           /* (in)  1=ordered, 2=periodic */
           /*double XYZcloud[],*/           /* (in)  array  of points in cloud */
           /*int    n,         */           /* (in)  number of control points */
           /*double cp[],      */           /* (in)  array  of control points (first and last are set) */
           /*                  */           /* (out) array  of control points (all set) */
           /*double *normf)    */           /* (out) RMS of distances between cloud and fit */
/*{
    int    status = EGADS_SUCCESS;*/      /* (out)  return status */

    /*int    ordered=0, periodic=0, np, nvar, ivar, jvar, nobj, iobj, i, j, k, next;
    int    niter, iter, count, itol, maxiter;
    double xmin, xmax, ymin, ymax, zmin, zmax, scale, xcent, ycent, zcent;
    double frac, toler, lambda, normdelta, normfnew, delta0, delta1, delta2;
    double XYZ[3], dXYZdT[3], omega, dmax, dist1, dist2, dist;
    double xx, xb, xe, yy, yb, ye, zz, zb, ze, tt, dd, dmin;

    int    *MMi=NULL;
    double *MMd=NULL, errmax;
    double *XYZcopy=NULL, *dXYZdP=NULL, *cpnew=NULL;
    double *beta=NULL, *delta=NULL, *betanew=NULL;
    double *f=NULL,  *fnew=NULL;
    double *aa=NULL, *bb=NULL, *cc=NULL, *rhs=NULL;

    ROUTINE(fit1dCloud);*/

    /* --------------------------------------------------------------- */

//#ifdef DEBUG
//    printf("enter fit1dCloud(m=%d, ordered=%d, n=%d)\n", m, ordered, n);
//#endif

//    assert(m > 1);                      // needed to avoid clang warning
//    assert(n > 2);                      // needed to avoid clang warning

    /* default return */
//    *normf  = 1e-12;

    /* extract ordered and periodic flags */
//    if (bitflag == 1 || bitflag == 3) ordered  = 1;
//    if (bitflag == 2 || bitflag == 3) periodic = 1;

    /* number of design variables and objectives */
//    np   = 3 * n - 6;
//    nvar = m + np;
//    nobj = 3 * m;
//
//    /* if m < n, then assume that the linear spline is the best fit */
//    if (m < n) {
//        for (j = 1; j < n-1; j++) {
//            frac = (double)(j) / (double)(n-1);
//
//            cp[3*j  ] = (1-frac) * cp[0] + frac * cp[3*n-3];
//            cp[3*j+1] = (1-frac) * cp[1] + frac * cp[3*n-2];
//            cp[3*j+2] = (1-frac) * cp[2] + frac * cp[3*n-1];
//        }
//
//#ifdef DEBUG
//        printf("making linear fit because not enough points in cloud\n");
//#endif
//        goto cleanup;
//    }
//
//    /* allocate all temporary arrays */
//    XYZcopy = (double *) malloc(3*m*sizeof(double));
//    dXYZdP  = (double *) malloc(  n*sizeof(double));
//    cpnew   = (double *) malloc(3*n*sizeof(double));
//
//    beta    = (double *) malloc(nvar*sizeof(double));
//    delta   = (double *) malloc(nvar*sizeof(double));
//    betanew = (double *) malloc(nvar*sizeof(double));
//
//    f       = (double *) malloc(nobj*sizeof(double));
//    fnew    = (double *) malloc(nobj*sizeof(double));
//
//    aa      = (double *) malloc(m    *sizeof(double));
//    bb      = (double *) malloc(m *np*sizeof(double));
//    cc      = (double *) malloc(np*np*sizeof(double));
//    rhs     = (double *) malloc(nvar *sizeof(double));
//
//    if (XYZcopy == NULL ||dXYZdP == NULL || cpnew == NULL ||
//        beta    == NULL || delta == NULL || betanew == NULL ||
//        f       == NULL || fnew  == NULL ||
//        aa      == NULL || bb    == NULL || cc == NULL || rhs == NULL) {
//        status = EGADS_MALLOC;
//        goto cleanup;
//    }
//
//#define AA(I)       aa[(I)]
//#define BB(I,J)     bb[(J)+np*(I)]
//#define CC(I,J)     cc[(J)+np*(I)]
//
//    /* transform inputs so that they are centered at origin and
//       unit length */
//    xmin = XYZcloud[0];
//    xmax = XYZcloud[0];
//    ymin = XYZcloud[1];
//    ymax = XYZcloud[1];
//    zmin = XYZcloud[2];
//    zmax = XYZcloud[2];
//
//    for (k = 1; k < m; k++) {
//        if (XYZcloud[3*k  ] < xmin) xmin = XYZcloud[3*k  ];
//        if (XYZcloud[3*k  ] > xmax) xmax = XYZcloud[3*k  ];
//        if (XYZcloud[3*k+1] < ymin) ymin = XYZcloud[3*k+1];
//        if (XYZcloud[3*k+1] > ymax) ymax = XYZcloud[3*k+1];
//        if (XYZcloud[3*k+2] < zmin) zmin = XYZcloud[3*k+2];
//        if (XYZcloud[3*k+2] > zmax) zmax = XYZcloud[3*k+2];
//    }
//
//    scale = 1.0 / MAX(MAX(xmax-xmin, ymax-ymin), zmax-zmin);
//    xcent = scale * (xmin + xmax) / 2;
//    ycent = scale * (ymin + ymax) / 2;
//    zcent = scale * (zmin + zmax) / 2;
//
//    for (k = 0; k < m; k++) {
//        XYZcopy[3*k  ] = scale * (XYZcloud[3*k  ] - xcent);
//        XYZcopy[3*k+1] = scale * (XYZcloud[3*k+1] - ycent);
//        XYZcopy[3*k+2] = scale * (XYZcloud[3*k+2] - zcent);
//    }
//    for (j = 0; j < n; j++) {
//        cp[3*j  ] = scale * (cp[3*j  ] - xcent);
//        cp[3*j+1] = scale * (cp[3*j+1] - ycent);
//        cp[3*j+2] = scale * (cp[3*j+2] - zcent);
//    }
//
//    /* set up the initial values for the interior control
//       points and the initial values of "t" */
//
//    /* XYZcopy is ordered */
//    if (ordered == 1) {
//
//        /* set the initial control point locations by picking up evenly
//           spaced points (based upon point number) from the cloud */
//        for (j = 1; j < n-1; j++) {
//            i = (j * (m-1)) / (n-1);
//
//            cp[3*j  ] = XYZcopy[3*i  ];
//            cp[3*j+1] = XYZcopy[3*i+1];
//            cp[3*j+2] = XYZcopy[3*i+2];
//        }
//
//        /* for each point in the cloud, assign the value of "t"
//           (which is stored in the first m betas) based upon it
//           local pseudo-arc-length */
//        beta[0] = 0;
//        for (k = 1; k < m; k++) {
//            beta[k] = beta[k-1] + sqrt(SQR(XYZcopy[3*k  ]-XYZcopy[3*k-3])
//                                      +SQR(XYZcopy[3*k+1]-XYZcopy[3*k-2])
//                                      +SQR(XYZcopy[3*k+2]-XYZcopy[3*k-1]));
//        }
//
//        for (k = 0; k < m; k++) {
//            beta[k] = (n-3) * beta[k] / beta[m-1];
//        }
//
//    /* XYZcopy is unordered */
//    } else {
//
//        /* set the "center" control point to coincide with the point
//           in the cloud that is furthest away from the first and
//           last control points */
//        dmax = 0;
//        for (k = 1; k < m-1; k++) {
//            dist1 = pow(XYZcopy[3*k  ]-cp[0], 2)
//                  + pow(XYZcopy[3*k+1]-cp[1], 2)
//                  + pow(XYZcopy[3*k+2]-cp[2], 2);
//            dist2 = pow(XYZcopy[3*k  ]-cp[3*n-3], 2)
//                  + pow(XYZcopy[3*k+1]-cp[3*n-2], 2)
//                  + pow(XYZcopy[3*k+2]-cp[3*n-1], 2);
//            dist  = MIN(dist1, dist2);
//
//            if (dist > dmax) {
//                dmax = dist;
//                cp[3*(n/2)  ] = XYZcopy[3*k  ];
//                cp[3*(n/2)+1] = XYZcopy[3*k+1];
//                cp[3*(n/2)+2] = XYZcopy[3*k+2];
//            }
//        }
//
//        /* fill in the other control points */
//        for (j = 1; j < (n/2); j++) {
//            frac = (double)(j) / (double)(n/2);
//
//            cp[3*j  ] = (1-frac) * cp[0] + frac * cp[3*(n/2)  ];
//            cp[3*j+1] = (1-frac) * cp[1] + frac * cp[3*(n/2)+1];
//            cp[3*j+2] = (1-frac) * cp[2] + frac * cp[3*(n/2)+2];
//        }
//
//        for (j = (n/2)+1; j < n; j++) {
//            frac = (double)(j-(n/2)) / (double)(n-1-(n/2));
//
//            cp[3*j  ] = (1-frac) * cp[3*(n/2)  ] + frac * cp[3*n-3];
//            cp[3*j+1] = (1-frac) * cp[3*(n/2)+1] + frac * cp[3*n-2];
//            cp[3*j+2] = (1-frac) * cp[3*(n/2)+2] + frac * cp[3*n-1];
//        }
//
//        /* for each point in the cloud, assign the value of "t"
//           (which is stored in the first m betas) as the closest
//           point to the control polygon */
//        for (k = 0; k < m; k++) {
//            xx = XYZcopy[3*k  ];
//            yy = XYZcopy[3*k+1];
//            zz = XYZcopy[3*k+2];
//
//            dmin = HUGEQ;
//            for (j = 1; j < n; j++) {
//                xb = cp[3*j-3];   yb = cp[3*j-2];   zb = cp[3*j-1];
//                xe = cp[3*j  ];   ye = cp[3*j+1];   ze = cp[3*j+2];
//
//                tt = ((xe-xb) * (xx-xb) + (ye-yb) * (yy-yb) + (ze-zb) * (zz-zb))
//                   / ((xe-xb) * (xe-xb) + (ye-yb) * (ye-yb) + (ze-zb) * (ze-zb));
//                tt = MIN(MAX(0, tt), 1);
//
//                dd = pow((1-tt) * xb + tt * xe - xx, 2)
//                   + pow((1-tt) * yb + tt * ye - yy, 2)
//                   + pow((1-tt) * zb + tt * ze - zz, 2);
//
//                if (dd < dmin) {
//                    dmin    = dd;
//                    beta[k] = ((j-1) + tt) * (double)(n - 3) / (double)(n - 1);
//                }
//            }
//        }
//    }
//
//#ifdef DEBUG
//    printf("Initialization\n");
//    for (j = 0; j < n; j++) {
//        printf("%3d: %12.6f %12.6f %12.6f\n", j, cp[3*j], cp[3*j+1], cp[3*j+2]);
//    }
//    for (k = 0; k < m; k++) {
//        printf("%3d: %12.6f\n", k, beta[k]);
//    }
//#endif
//
//    /* set the relaxation parameter for control points */
//    omega = 0.25;
//
//    /* insert the interior control points into the design variables */
//    next = m;
//    for (j = 1; j < n-1; j++) {
//        beta[next++] = cp[3*j  ];
//        beta[next++] = cp[3*j+1];
//        beta[next++] = cp[3*j+2];
//    }
//
//    /* compute the initial objective function */
//    for (k = 0; k < m; k++) {
//        status = eval1dBspline(beta[k], n, cp, XYZ, NULL, NULL);
//        CHECK_STATUS(eval1dBspline);
//
//        f[3*k  ] = XYZcopy[3*k  ] - XYZ[0];
//        f[3*k+1] = XYZcopy[3*k+1] - XYZ[1];
//        f[3*k+2] = XYZcopy[3*k+2] - XYZ[2];
//    }
//    *normf = L2norm(f, nobj) / m;
//#ifdef DEBUG
//    printf("initial   norm(f)=%11.4e\n", *normf);
//#endif
//
//    /* initialize the Levenberg-Marquardt algorithm */
//    niter  = 501;
//    toler  = 1.0e-6;
//    lambda = 1;
//
//    /* LM iterations */
//    for (iter = 0; iter < niter; iter++) {
//
//        /* initialize [AA  BB]
//                      [      ] =  transpose(J) * J + lambda * diag(transpose(J) * J)
//                      [BB' CC]
//
//           and        rhs  = -transpose(J) * f
//        */
//        for (jvar = 0; jvar < np; jvar++) {
//            for (ivar = 0; ivar < np; ivar++) {
//                CC(ivar,jvar) = 0;
//            }
//            CC(jvar,jvar) = 1e-6;
//        }
//
//        for (jvar = 0; jvar < nvar; jvar++) {
//            rhs[jvar] = 0;
//        }
//
//        /* accumulate AA, BB, CC, and rhs by looping over points in cloud */
//        for (k = 0; k < m; k++) {
//            status = eval1dBspline(beta[k], n, cp, XYZ, dXYZdT, dXYZdP);
//            CHECK_STATUS(eval1dBspline);
//
//            AA(k) = dXYZdT[0] * dXYZdT[0] + dXYZdT[1] * dXYZdT[1] + dXYZdT[2] * dXYZdT[2];
//
//            for (ivar = 1; ivar < n-1; ivar++) {
//                BB(k, 3*ivar-3) = dXYZdT[0] * dXYZdP[ivar];
//                BB(k, 3*ivar-2) = dXYZdT[1] * dXYZdP[ivar];
//                BB(k, 3*ivar-1) = dXYZdT[2] * dXYZdP[ivar];
//
//                for (jvar = 1; jvar < n-1; jvar++) {
//#ifndef __clang_analyzer__
//                    CC(3*ivar-3, 3*jvar-3) += dXYZdP[ivar] * dXYZdP[jvar];
//                    CC(3*ivar-2, 3*jvar-2) += dXYZdP[ivar] * dXYZdP[jvar];
//                    CC(3*ivar-1, 3*jvar-1) += dXYZdP[ivar] * dXYZdP[jvar];
//#endif
//                }
//            }
//
//            rhs[k] = dXYZdT[0] * f[3*k] + dXYZdT[1] * f[3*k+1] + dXYZdT[2] * f[3*k+2];
//
//            for (ivar = 1; ivar < n-1; ivar++) {
//#ifndef __clang_analyzer__
//                rhs[m+3*ivar-3] += dXYZdP[ivar] * f[3*k  ];
//                rhs[m+3*ivar-2] += dXYZdP[ivar] * f[3*k+1];
//                rhs[m+3*ivar-1] += dXYZdP[ivar] * f[3*k+2];
//#endif
//            }
//        }
//
//        /* set up sparse-matrix arrays */
//        count = m + 2 * m * np + np * np + 1;
//
//        MMd = (double *) malloc(count*sizeof(double));
//        MMi = (int    *) malloc(count*sizeof(int   ));
//
//        if (MMd == NULL || MMi == NULL) {
//            status = EGADS_MALLOC;
//            goto cleanup;
//        }
//
//        /* store diagonal values (multiplied by (1+lambda)) */
//        for (k = 0; k < m; k++) {
//            MMd[k] = AA(k) * (1 + lambda);
//        }
//        for (ivar = 0; ivar < np; ivar++) {
//            MMd[m+ivar] = CC(ivar,ivar) * (1 + lambda);
//        }
//
//        /* set up off-diagonal elements, including indices */
//        MMi[0] = nvar + 1;
//        count  = nvar;
//
//        /* BB to the right of AA */
//        for (k = 0; k < m; k++) {
//            for (jvar = 0; jvar < np; jvar++) {
//                count++;
//                MMd[count] = BB(k,jvar);
//                MMi[count] = m + jvar;
//            }
//            MMi[k+1] = count + 1;
//        }
//
//        for (ivar = 0; ivar < np; ivar++) {
//            /* transpose(BB) below A */
//            for (k = 0; k < m; k++) {
//                count++;
//                MMd[count] = BB(k,ivar);
//                MMi[count] = k;
//            }
//
//            /* CC in bottom-right corner */
//            for (jvar = 0; jvar < np; jvar++) {
//                if (ivar != jvar) {
//                    count++;
//                    MMd[count] = CC(ivar,jvar);
//                    MMi[count] = m + jvar;
//                }
//            }
//            MMi[m+ivar+1] = count + 1;
//        }
//
//        /* arbitrary value (not used) */
//        MMd[nvar] = 0;
//
//        /* set up for sparse matrix solve (via biconjugate gradient technique) */
//        itol    = 1;
//        errmax  = 1.0e-12;
//        maxiter = 2 * nvar;
//        for (ivar = 0; ivar < nvar; ivar++) {
//            delta[ivar] = 0;
//        }
//
//        status = solveSparse(MMd, MMi, rhs, delta, itol, &errmax, &maxiter);
//        CHECK_STATUS(solveSparse);
//
//        FREE(MMd);
//        FREE(MMi);
//
//        /* check for convergence on delta (which corresponds to a small
//           change in beta) */
//        normdelta = L2norm(delta, nvar);
//
//        if (normdelta < toler) {
//#ifdef DEBUG
//            printf("converged with norm(delta)=%11.4e\n", normdelta);
//#endif
//            break;
//        }
//
//        /* find the temporary new beta */
//        for (ivar = 0; ivar < nvar; ivar++) {
//
//            /* beta associated with Tcloud */
//            if (ivar < m) {
//                betanew[ivar] = beta[ivar] + delta[ivar];
//
//                if (betanew[ivar] < 0  ) betanew[ivar] = 0;
//                if (betanew[ivar] > n-3) betanew[ivar] = n-3;
//
//                /* beta associated with control points */
//            } else {
//                betanew[ivar] = beta[ivar] + omega * delta[ivar];
//            }
//        }
//
//        /* gradually increase omega */
//        omega = MIN(1.01*omega, 1.0);
//
//        /* extract the temporary control points from betanew */
//        next = m;
//        for (j = 0; j < n; j++) {
//            if (j == 0 || j == n-1) {
//                cpnew[3*j  ] = cp[3*j  ];
//                cpnew[3*j+1] = cp[3*j+1];
//                cpnew[3*j+2] = cp[3*j+2];
//            } else {
//                cpnew[3*j  ] = betanew[next++];
//                cpnew[3*j+1] = betanew[next++];
//                cpnew[3*j+2] = betanew[next++];
//            }
//        }
//
//        /* apply periodicity condition by making sure first and last
//           intervals are the same */
//        if (periodic == 1) {
//            delta0 = (2*cpnew[0] - cpnew[3] - cpnew[3*n-6]) / 2;
//            delta1 = (2*cpnew[1] - cpnew[4] - cpnew[3*n-5]) / 2;
//            delta2 = (2*cpnew[2] - cpnew[5] - cpnew[3*n-4]) / 2;
//
//            cpnew[    3] += delta0;
//            cpnew[    4] += delta1;
//            cpnew[    5] += delta2;
//
//            cpnew[3*n-6] += delta0;
//            cpnew[3*n-5] += delta1;
//            cpnew[3*n-4] += delta2;
//        }
//
//        /* compute the objective function based upon the new beta */
//        for (k = 0; k < m; k++) {
//            status = eval1dBspline(betanew[k], n, cp, XYZ, NULL, NULL);
//            CHECK_STATUS(eval1dBspline);
//
//            fnew[3*k  ] = XYZcopy[3*k  ] - XYZ[0];
//            fnew[3*k+1] = XYZcopy[3*k+1] - XYZ[1];
//            fnew[3*k+2] = XYZcopy[3*k+2] - XYZ[2];
//        }
//        normfnew = L2norm(fnew, nobj) / m;
//#ifdef DEBUG
//        if (iter%10 == 0) {
//            printf("iter=%4d: norm(delta)=%11.4e, norm(f)=%11.4e  ",
//                   iter, normdelta, normfnew);
//        }
//#endif
//
//        /* if this was a better step, accept it and decrease
//           lambda (making it more Newton-like) */
//        if (normfnew < *normf) {
//            lambda /= 2;
//#ifdef DEBUG
//            if (iter%10 == 0) {
//                printf("ACCEPTED,  lambda=%11.4e, omega=%10.5f\n", lambda, omega);
//            }
//#endif
//
//            /* save new design variables, control points, and
//               objective function */
//            for (ivar = 0; ivar < nvar; ivar++) {
//                beta[ivar] = betanew[ivar];
//            }
//            for (j = 0; j < n; j++) {
//                cp[3*j  ] = cpnew[3*j  ];
//                cp[3*j+1] = cpnew[3*j+1];
//                cp[3*j+2] = cpnew[3*j+2];
//            }
//            for (iobj = 0; iobj < nobj; iobj++) {
//                f[iobj] = fnew[iobj];
//            }
//            *normf = normfnew;
//
//        /* otherwise do not take the step and increase lambda (making it
//           more steepest-descent-like) */
//        } else {
//            lambda *= 2;
//#ifdef DEBUG
//            if (iter %10 == 0) {
//                printf("rejected,  lambda=%11.4e, omega=%10.5f\n", lambda, omega);
//            }
//#endif
//        }
//
//        /* check for convergence (based upon a small value of
//           objective function) */
//        if (*normf < toler) {
//#ifdef DEBUG
//            printf("converged with norm(f)=%11.4e\n", *normf);
//#endif
//            break;
//        }
//    }
//
//    /* transform control points back to their original scale */
//    for (j = 0; j < n; j++) {
//        cp[3*j  ] = xcent + cp[3*j  ] / scale;
//        cp[3*j+1] = ycent + cp[3*j+1] / scale;
//        cp[3*j+2] = zcent + cp[3*j+2] / scale;
//    }
//
//    *normf /= scale;
//
//#ifdef DEBUG
//    printf("final control points\n");
//    for (j = 0; j < n; j++) {
//        printf("%3d: %12.6f %12.6f %12.6f\n", j, cp[3*j], cp[3*j+1], cp[3*j+2]);
//    }
//    printf("*normf: %12.4e\n", *normf);
//#endif
//
//cleanup:
//    if (XYZcopy != NULL) free(XYZcopy);
//    if (dXYZdP  != NULL) free(dXYZdP );
//    if (cpnew   != NULL) free(cpnew  );
//
//    if (beta    != NULL) free(beta   );
//    if (delta   != NULL) free(delta  );
//    if (betanew != NULL) free(betanew);
//
//    if (f       != NULL) free(f      );
//    if (fnew    != NULL) free(fnew   );
//
//    if (aa      != NULL) free(aa     );
//    if (bb      != NULL) free(bb     );
//    if (cc      != NULL) free(cc     );
//    if (rhs     != NULL) free(rhs    );
//
//    if (MMi     != NULL) free(MMi    );
//    if (MMd     != NULL) free(MMd    );
//
//#undef AA
//#undef BB
//#undef CC
//
//    return status;
//}


/*
 ************************************************************************
 *                                                                      *
 *   eval1dBspline - evaluate cubic Bspline and its derivatives         *
 *                                                                      *
 ************************************************************************
 */

//static int
//eval1dBspline(double T,                 /* (in)  independent variable */
//              int    n,                 /* (in)  number of control points */
//              double cp[],              /* (in)  array  of control points */
//              double XYZ[],             /* (out) dependent variables */
//    /*@null@*/double dXYZdT[],          /* (out) derivative wrt T (or NULL) */
//    /*@null@*/double dXYZdP[])          /* (out) derivative wrt P (or NULL) */
//{
//    int    status = EGADS_SUCCESS;      /* (out) return status */
//
//    int    i, span;
//    double N[4], dN[4];
//
//    ROUTINE(eval1dBspline);
//
//    /* --------------------------------------------------------------- */
//
//    assert (n > 3);
//
//    XYZ[0] = 0;
//    XYZ[1] = 0;
//    XYZ[2] = 0;
//
//    /* set up the Bspline bases */
//    status = cubicBsplineBases(n, T, N, dN);
//    CHECK_STATUS(cubicBsplineBases);
//
//    span = MIN(floor(T), n-4);
//
//    /* find the dependent variable */
//    for (i = 0; i < 4; i++) {
//        XYZ[0] += N[i] * cp[3*(i+span)  ];
//        XYZ[1] += N[i] * cp[3*(i+span)+1];
//        XYZ[2] += N[i] * cp[3*(i+span)+2];
//    }
//
//    /* find the deriviative wrt T */
//    if (dXYZdT != NULL) {
//        dXYZdT[0] = 0;
//        dXYZdT[1] = 0;
//        dXYZdT[2] = 0;
//
//        for (i = 0; i < 4; i++) {
//            dXYZdT[0] += dN[i] * cp[3*(i+span)  ];
//            dXYZdT[1] += dN[i] * cp[3*(i+span)+1];
//            dXYZdT[2] += dN[i] * cp[3*(i+span)+2];
//        }
//    }
//
//    /* find the derivative wrt P */
//    if (dXYZdP != NULL) {
//        for (i = 0; i < n; i++) {
//            dXYZdP[i] = 0;
//        }
//        for (i = 0; i < 4; i++) {
//            dXYZdP[i+span] += N[i];
//        }
//    }
//
//cleanup:
//    return status;
//}


/*
 ************************************************************************
 *                                                                      *
 *   cubicBsplineBases - basis function values for cubic Bspline        *
 *                                                                      *
 ************************************************************************
 */

//static int
//cubicBsplineBases(int    ncp,           /* (in)  number of control points */
//                  double T,             /* (in)  independent variable (0<=T<=(ncp-3) */
//                  double N[],           /* (out) bases */
//                  double dN[])          /* (out) d(bases)/d(T) */
//{
//    int       status = EGADS_SUCCESS;   /* (out) return status */
//
//    int      i, r, span;
//    double   saved, dsaved, num, dnum, den, dden, temp, dtemp;
//    double   left[4], dleft[4], rite[4], drite[4];
//
//    ROUTINE(cubicBsplineBases);
//
//    /* --------------------------------------------------------------- */
//
//    span = MIN(floor(T)+3, ncp-1);
//
//    N[ 0] = 1.0;
//    dN[0] = 0;
//
//    for (i = 1; i <= 3; i++) {
//        left[ i] = T - MAX(0, span-2-i);
//        dleft[i] = 1;
//
//        rite[ i] = MIN(ncp-3,span-3+i) - T;
//        drite[i] =                     - 1;
//
//        saved  = 0;
//        dsaved = 0;
//
//        for (r = 0; r < i; r++) {
//            num   = N[ r];
//            dnum  = dN[r];
//
//            den   = rite[ r+1] + left[ i-r];
//            dden  = drite[r+1] + dleft[i-r];
//
//            temp  = num / den;
//            dtemp = (dnum * den - dden * num) / den / den;
//
//            N[ r] = saved  + rite[ r+1] * temp;
//            dN[r] = dsaved + drite[r+1] * temp + rite[r+1] * dtemp;
//
//            saved  = left[ i-r] * temp;
//            dsaved = dleft[i-r] * temp + left[i-r] * dtemp;
//        }
//
//        N[ i] = saved;
//        dN[i] = dsaved;
//    }
//
////cleanup:
//    return status;
//}


/*
 ************************************************************************
 *                                                                      *
 *   solveSparse - solve: A * x = b  using biconjugate gradient method  *
 *                                                                      *
 ************************************************************************
 */

//static int
//solveSparse(double SAv[],               /* (in)  sparse array values */
//            int    SAi[],               /* (in)  sparse array indices */
//            double b[],                 /* (in)  rhs vector */
//            double x[],                 /* (in)  guessed result vector */
//                                        /* (out) result vector */
//            int    itol,                /* (in)  stopping criterion */
//            double *errmax,             /* (in)  convergence tolerance */
//                                        /* (out) estimated error at convergence */
//            int    *iter)               /* (in)  maximum number of iterations */
//                                        /* (out) number of iterations taken */
//{
//    int    status = EGADS_SUCCESS;      /* (out) return status */
//
//    int    n, i, j, k, itmax;
//    double tol, ak, akden, bk, bknum, bkden, bnorm, dxnorm, xnorm, znorm_old, znorm=0;
//    double *p=NULL, *pp=NULL, *r=NULL, *rr=NULL, *z=NULL, *zz=NULL;
//
//    ROUTINE(solveSparse);
//
//    /* --------------------------------------------------------------- */
//
//    tol   = *errmax;
//    itmax = *iter;
//
//    n = SAi[0] - 1;
//
//    p  = (double *) malloc(n*sizeof(double));
//    pp = (double *) malloc(n*sizeof(double));
//    r  = (double *) malloc(n*sizeof(double));
//    rr = (double *) malloc(n*sizeof(double));
//    z  = (double *) malloc(n*sizeof(double));
//    zz = (double *) malloc(n*sizeof(double));
//
//    if (p == NULL || pp == NULL ||
//        r == NULL || rr == NULL ||
//        z == NULL || zz == NULL   ) {
//        status = EGADS_MALLOC;
//        goto cleanup;
//    }
//
//    /* make sure none of the diagonals are very small */
//    for (i = 0; i < n; i++) {
//        if (fabs(SAv[i]) < 1.0e-14) {
//            printf(" solveSparse: cannot solve since SAv[%d]=%11.4e\n", i, SAv[i]);
//            status = EGADS_DEGEN;
//            goto cleanup;
//        }
//    }
//
//    /* calculate initial residual */
//    *iter = 0;
//
//    /* r = A * x  */
//    for (i = 0; i < n; i++) {
//        r[i] = SAv[i] * x[i];
//
//        for (k = SAi[i]; k < SAi[i+1]; k++) {
//            r[i] += SAv[k] * x[SAi[k]];
//        }
//    }
//
//    for (j = 0; j < n; j++) {
//        r[ j] = b[j] - r[j];
//        rr[j] = r[j];
//    }
//
//    if (itol == 1) {
//        bnorm = L2norm(b, n);
//
//        for (j = 0; j < n; j++) {
//            z[j] = r[j] / SAv[j];
//        }
//    } else if (itol == 2) {
//        for (j = 0; j < n; j++) {
//            z[j] = b[j] / SAv[j];
//        }
//
//        bnorm = L2norm(z, n);
//
//        for (j = 0; j < n; j++) {
//            z[j] = r[j] / SAv[j];
//        }
//    } else {
//        for (j = 0; j < n; j++) {
//            z[j] = b[j] / SAv[j];
//        }
//
//        bnorm = L2norm(z, n);
//
//        for (j = 0; j < n; j++) {
//            z[j] = r[j] / SAv[j];
//        }
//
//        znorm = L2norm(z, n);
//    }
//
//    /* main iteration loop */
//    for (*iter = 0; *iter < itmax; (*iter)++) {
//
//        for (j = 0; j < n; j++) {
//            zz[j] = rr[j] / SAv[j];
//        }
//
//        /* calculate coefficient bk and direction vectors p and pp */
//        bknum = 0;
//        for (j = 0; j < n; j++) {
//            bknum += z[j] * rr[j];
//        }
//
//        if (*iter == 0) {
//            for (j = 0; j < n; j++) {
//                p[ j] = z[ j];
//                pp[j] = zz[j];
//            }
//        } else {
//            bk = bknum / bkden;
//
//            for (j = 0; j < n; j++) {
//                p[ j] = bk * p[ j] + z[ j];
//                pp[j] = bk * pp[j] + zz[j];
//            }
//        }
//
//        /* calculate coefficient ak, new iterate x, and new residuals r and rr */
//        bkden = bknum;
//
//        /* z = A * p  */
//        for (i = 0; i < n; i++) {
//            z[i] = SAv[i] * p[i];
//
//            for (k = SAi[i]; k < SAi[i+1]; k++) {
//                z[i] += SAv[k] * p[SAi[k]];
//            }
//        }
//
//        akden = 0;
//        for (j = 0; j < n; j++) {
//            akden += z[j] * pp[j];
//        }
//
//        ak = bknum / akden;
//
//        /* zz = transpose(A) * pp  */
//        for (i = 0; i < n; i++) {
//            zz[i] = SAv[i] * pp[i];
//        }
//
//        for (i = 0; i < n; i++) {
//            for (k = SAi[i]; k < SAi[i+1]; k++) {
//                j = SAi[k];
//                zz[j] += SAv[k] * pp[i];
//            }
//        }
//
//        for (j = 0; j < n; j++) {
//            x[ j] += ak * p[ j];
//            r[ j] -= ak * z[ j];
//            rr[j] -= ak * zz[j];
//        }
//
//        /* solve Abar * z = r */
//        for (j = 0; j < n; j++) {
//            z[j] = r[j] / SAv[j];
//        }
//
//        /* compute and check stopping criterion */
//        if (itol == 1) {
//            *errmax = L2norm(r, n) / bnorm;
//        } else if (itol == 2) {
//            *errmax = L2norm(z, n) / bnorm;
//        } else {
//            znorm_old = znorm;
//            znorm = L2norm(z, n);
//            if (fabs(znorm_old-znorm) > (1.0e-14)*znorm) {
//                dxnorm = fabs(ak) * L2norm(p, n);
//                *errmax  = znorm / fabs(znorm_old-znorm) * dxnorm;
//            } else {
//                *errmax = znorm / bnorm;
//                continue;
//            }
//
//            xnorm = L2norm(x, n);
//            if (*errmax <= xnorm/2) {
//                *errmax /= xnorm;
//            } else {
//                *errmax = znorm / bnorm;
//                continue;
//            }
//        }
//
//        /* exit if converged */
//        if (*errmax <= tol) break;
//    }
//
//cleanup:
//    if (p  != NULL) free(p );
//    if (pp != NULL) free(pp);
//    if (r  != NULL) free(r );
//    if (rr != NULL) free(rr);
//    if (z  != NULL) free(z );
//    if (zz != NULL) free(zz);
//
//    return status;
//}


/*
 ************************************************************************
 *                                                                      *
 *   L2norm - L2-norm of vector                                         *
 *                                                                      *
 ************************************************************************
 */

//static double
//L2norm(double f[],                      /* (in)  vector */
//       int    n)                        /* (in)  length of vector */
//{
//    double L2norm;                      /* (out) L2-norm */
//
//    int    i;
//
//    ROUTINE(L2norm);
//
//    /* --------------------------------------------------------------- */
//
//    /* L2-norm */
//    L2norm = 0;
//
//    for (i = 0; i < n; i++) {
//        L2norm += f[i] * f[i];
//    }
//
//    L2norm = sqrt(L2norm);
//
////cleanup:
//    return L2norm;
//}


/*
 ************************************************************************
 *                                                                      *
 *   plotCurve - plot geometry, control polygon, and fit                *
 *                                                                      *
 ************************************************************************
 */

#ifdef GRAFIC
static int
plotCurve(int    npnt,                  /* (in)  number of points in cloud */
          double xyz[],                 /* (in)  points in cloud */
          ego    ecurve)                /* (in)  Curve */
{
    int    status = EGADS_SUCCESS;

    int    io_kbd=5, io_scr=6, nline=0, nplot=0;
    int    indgr=1+2+4+16+64, periodic, oclass, mtype, *tempIlist, icp, ipnt;
    int    ilin[3], isym[3], nper[3];
    int    ncp=NCP(numUdp);
    float  *xplot=NULL, *yplot=NULL, *zplot=NULL;
    double xmin, xmax, ymin, ymax, zmin, zmax, trange[4], frac, tt, data[18];
    double *tempRlist;
    ego    eref;

    /* --------------------------------------------------------------- */

    xplot = (float*) malloc((npnt+1000+ncp)*sizeof(float));
    yplot = (float*) malloc((npnt+1000+ncp)*sizeof(float));
    zplot = (float*) malloc((npnt+1000+ncp)*sizeof(float));

    if (xplot == NULL || yplot == NULL || zplot == NULL) {
        status = EGADS_MALLOC;
        goto cleanup;
    }

    xmin = xyz[0];
    xmax = xyz[0];
    ymin = xyz[1];
    ymax = xyz[1];
    zmin = xyz[2];
    zmax = xyz[2];

    /* build plot arrays for data points */
    for (ipnt = 0; ipnt < npnt; ipnt++) {
        xplot[nplot] = xyz[3*ipnt  ];
        yplot[nplot] = xyz[3*ipnt+1];
        zplot[nplot] = xyz[3*ipnt+2];
        nplot++;

        if (xyz[3*ipnt  ] < xmin) xmin = xyz[3*ipnt  ];
        if (xyz[3*ipnt  ] > xmax) xmax = xyz[3*ipnt  ];
        if (xyz[3*ipnt+1] < ymin) ymin = xyz[3*ipnt+1];
        if (xyz[3*ipnt+1] > ymax) ymax = xyz[3*ipnt+1];
        if (xyz[3*ipnt+2] < zmin) zmin = xyz[3*ipnt+2];
        if (xyz[3*ipnt+2] > zmax) zmax = xyz[3*ipnt+2];
    }

    ilin[nline] = -GR_DASHED;
    isym[nline] = +GR_CIRCLE;
    nper[nline] = npnt;
    nline++;

    /* build plot arrays for fit */
    status = EG_getRange(ecurve, trange, &periodic);
    if (status < 0) goto cleanup;

    for (ipnt = 0; ipnt < 1000; ipnt++) {
        frac = (double)(ipnt) / (double)(1000-1);
        tt   = (1-frac) * trange[0] + frac * trange[1];

        status = EG_evaluate(ecurve, &tt, data);
        if (status < 0) goto cleanup;

        xplot[nplot] = data[0];
        yplot[nplot] = data[1];
        zplot[nplot] = data[2];
        nplot++;
    }

    ilin[nline] = +GR_SOLID;
    isym[nline] = -GR_PLUS;
    nper[nline] = 1000;
    nline++;

    /* build plot arrays for control points */
    status = EG_getGeometry(ecurve, &oclass, &mtype, &eref, &tempIlist, &tempRlist);
    if (status <  0      ) goto cleanup;
    if (oclass != CURVE  ) goto cleanup;
    if (mtype  != BSPLINE) goto cleanup;

    for (icp = 0; icp < ncp; icp++) {
        xplot[nplot] = tempRlist[tempIlist[3]+3*icp  ];
        yplot[nplot] = tempRlist[tempIlist[3]+3*icp+1];
        zplot[nplot] = tempRlist[tempIlist[3]+3*icp+2];
        nplot++;
    }

    ilin[nline] = +GR_DOTTED;
    isym[nline] = +GR_SQUARE;
    nper[nline] = ncp;
    nline++;

    /* generate plot */
    if        (xmin == xmax) {
        grinit_(&io_kbd, &io_scr, "udpFitcurve", strlen("udpFitcurve"));

        grline_(ilin, isym, &nline,                "~y~z~O=inputs, --=fit, ...=cp",
                &indgr, yplot, zplot, nper, strlen("~y~z~O=inputs, --=fit, ...=cp"));

    } else if (ymin == ymax) {
        grinit_(&io_kbd, &io_scr, "udpFitcurve", strlen("udpFitcurve"));

        grline_(ilin, isym, &nline,                "~z~x~O=inputs, --=fit, ...=cp",
                &indgr, zplot, xplot, nper, strlen("~z~x~O=inputs, --=fit, ...=cp"));

    } else {
        grinit_(&io_kbd, &io_scr, "udpFitcurve", strlen("udpFitcurve"));

        grline_(ilin, isym, &nline,                "~x~y~O=inputs, --=fit, ...=cp",
                &indgr, xplot, yplot, nper, strlen("~x~y~O=inputs, --=fit, ...=cp"));
    }

cleanup:
    if (xplot != NULL) free(xplot);
    if (yplot != NULL) free(yplot);
    if (zplot != NULL) free(zplot);

    return status;
}
#endif
