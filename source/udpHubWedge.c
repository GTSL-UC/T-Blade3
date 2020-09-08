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
#define HUB_INF_OFFSET(     IUDP,I) ((double *) (udps[IUDP].arg[33].val))[I]
#define TIP_INF_OFFSET(     IUDP,I) ((double *) (udps[IUDP].arg[34].val))[I]
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
#include <sys/stat.h>
#include <stdio.h>
#include <assert.h>

#ifdef WIN32
   #define SLASH '\\'
#else
   #include <unistd.h>
   #include <libgen.h>
   #define SLASH '/'
#endif

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
void   override_naca_le_radius_(     int *nspn, double naca_le_radius[      ]);
void   override_naca_u_max_(         int *nspn, double naca_u_max[          ]);
void   override_naca_t_max_(         int *nspn, double naca_t_max[          ]);
void   override_naca_t_te_(          int *nspn, double naca_t_te[           ]);


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
    int     nsec, isec, npnt, nn, *senses, sense[8], oclass, mtype, nchild, sizes[2], i, ichar;
    double  data[6], node0[3], node1[3], node2[3], node3[3], xyz2[3*NPNT], angle, nblades;
    double  trange[2], xmin, ymin, zmin, xmax, ymax, zmax, matrix[12];
    char    filename[257], auxname[257], *auxptr, nextline[257], casename[257];
    FILE    *fp, *fpSrc, *fpTgt, *fp_hub, *fp_meanline;
    ego     ecurve, enodes[4], eedge, eloops[2], eref, *echilds, eface, etemp[2], exform;

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

    /* make the Tblade3_temp temporary directory */
    status = mkdir("Tblade3_temp", 0777);
    if (status < 0) {
        printf(" udpExecute: could not \"mkdir Tblade3_temp\".  If it already exists, files may be overwritten.\n");
    }

    /* copy the input files to the Tblade3_temp directory */
    snprintf(filename, 256, "Tblade3_temp%c%s", SLASH, basename(FILENAME(0)));
#ifdef DEBUG
    printf("FILENAME(0)=%s\n", FILENAME(0));
    printf("fileame    =%s\n", filename   );
#endif

    fpSrc = fopen(FILENAME(0), "r");
    fpTgt = fopen(filename,    "w");

    if (fpSrc != NULL && fpTgt != NULL) {
        while (1) {
            ichar = fgetc(fpSrc);
            if (ichar != EOF) {
                fputc(ichar, fpTgt);
            } else {
                break;
            }
        }
    } else {
        printf(" udpExecute: could not copy \"%s\"\n", FILENAME(0));
        status = EGADS_NOTFOUND;
        goto cleanup;
    }

    if (fpSrc != NULL) {
        fclose(fpSrc);
        fpSrc = NULL;
    }
    if (fpTgt != NULL) {
        fclose(fpTgt);
        fpTgt = NULL;
    }

    auxptr = strstr(AUXNAME(0), "spancontrolinputs");
    if (auxptr == NULL) {
        auxptr = strstr(AUXNAME(0), "controlinputs");
        if (auxptr == NULL) {
            printf(" udpExecute: \"%s\" is not a valid auxname\n", AUXNAME(0));
            status = EGADS_NOTFOUND;
            goto cleanup;
        }
    }

    snprintf(auxname, 256, "Tblade3_temp%c%s", SLASH, auxptr);
#ifdef DEBUG
    printf("AUXNAME(0)=%s\n", AUXNAME(0));
    printf("auxname   =%s\n", auxname   );
#endif

    fpSrc = fopen(AUXNAME(0), "r");
    fpTgt = fopen(auxname,    "w");

    if (fpSrc != NULL && fpTgt != NULL) {
        while (1) {
            ichar = fgetc(fpSrc);
            if (ichar != EOF) {
                fputc(ichar, fpTgt);
            } else {
                break;
            }
        }
    } else {
        printf(" udpExecute: could not copy \"%s\"\n", AUXNAME(0));
        status = EGADS_NOTFOUND;
        goto cleanup;
    }

    if (fpSrc != NULL) {
        fclose(fpSrc);
        fpSrc = NULL;
    }
    if (fpTgt != NULL) {
        fclose(fpTgt);
        fpTgt = NULL;
    }

    /* change working directory to Tblade3_temp so that all temporary
       files are kept together */
    status = chdir("Tblade3_temp");
    if (status < 0) {
        printf(" udpExecute: could not change to \"Tblade3_temp\"\n");
        status = EGADS_EMPTY;
        return status; 
    }

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
    snprintf(filename, 256, "%s", basename(FILENAME(numUdp)));

    fp = fopen(filename, "r");
    if (fp == NULL) {
        printf("could not open \"%s\"\n", filename);
        status = -9999;
        goto cleanup;
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
    bgb3d_sub_(filename, AUXNAME(numUdp), ARG_2(numUdp), "", "",
               strlen(filename), strlen(AUXNAME(numUdp)), (int)strlen(ARG_2(numUdp)), strlen(""), strlen(""));

    /* read hub file */
    sprintf(filename, "hub.%s.sldcrv", casename);
    printf("reading: %s\n", filename);

    fp_hub = fopen(filename, "r");
    fp  = fp_hub;      //Might need to remove
    if (fp == NULL) {
        printf("could not open \"%s\"\n", filename);
        status = -9999;
        goto cleanup;
    }

    /* read meanline files and create curves */
    for (isec = 1; isec <= 1; isec++) {

        sprintf(filename, "meanline.sec%d.%s.dat", isec, casename);
        printf("reading: %s\n", filename);
        fp_meanline = fopen(filename, "r");
        if (fp == NULL) {
            printf("could not open \"%s\"\n", filename);
            status = -9999;
            goto cleanup;
        }

        /* fill xyz2 array with points in meanline file */
        npnt = 0;
        while (1) {
            nn = fscanf(fp_meanline, "%lf %lf %lf\n", &(xyz2[3*npnt]), &(xyz2[3*npnt+1]), &(xyz2[3*npnt+2]));
            if (nn < 3) {
                break;
            }else if (npnt >= NPNT) {
                printf("recompile with larger NPNT\n");
                status = -9999;
                goto cleanup;
            } else {
                npnt++;
            }
        }

        fclose(fp_meanline);
    }

    /* bump coordinates out a little to ensure UNION
    for (i = 0; i < npnt; i++) {
        xyz2[3*i+1] *= 1.002;
        xyz2[3*i+2] *= 1.002;
    } */

    /* find xyz positions of the first and last points */
    xmin = xyz2[0];
    ymin = xyz2[1];
    zmin = xyz2[2];

    xmax = xyz2[3*(npnt-1)  ];
    ymax = xyz2[3*(npnt-1)+1];
    zmax = xyz2[3*(npnt-1)+2];

    /* create a node at the first and last points */
    node0[0] = xmin;
    node0[1] = ymin;
    node0[2] = zmin;

    status = EG_makeTopology(context, NULL, NODE, 0, node0, 0, NULL, NULL, &(enodes[0]));
#ifdef DEBUG
    printf("EG_makeTopology(node0) -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    node1[0] = xmax;
    node1[1] = ymax;
    node1[2] = zmax;

    status = EG_makeTopology(context, NULL, NODE, 0, node1, 0, NULL, NULL, &(enodes[1]));
#ifdef DEBUG
    printf("EG_makeTopology(node1) -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    /* fit B-Spline to points */
    sizes[0]=npnt;
    sizes[1]=0;
    status=EG_approximate(context, 2, 1.0e-6, sizes, xyz2, &ecurve);
#ifdef DEBUG
    printf("EG_approximate -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    /* get ranges for Edge */
    status = EG_invEvaluate(ecurve, node0, &(trange[0]), data);
#ifdef DEBUG
    printf("EG_invEvaluate -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    status = EG_invEvaluate(ecurve, node1, &(trange[1]), data);
#ifdef DEBUG
    printf("EG_invEvaluate -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    /* make the Edge from the meanline */
    etemp[0] = enodes[0];
    etemp[1] = enodes[1];
    status = EG_makeTopology(context, ecurve, EDGE, TWONODE, trange, 2, etemp, NULL, &eedge);
#ifdef DEBUG
    printf("EG_makeTopology(edge0) -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    sense[0] = SFORWARD;
    status = EG_makeTopology(context, NULL, LOOP, OPEN, NULL, 1, &eedge, sense, &(eloops[0]));
#ifdef DEBUG
    printf("EG_makeTopology(loop0) -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    /* make an Edge along the X-axis */
    node2[0] = xmin;
    node2[1] = 0;
    node2[2] = 0.000;    // avoid uncaught exception in EG_rotate

    status = EG_makeTopology(context, NULL, NODE, 0, node2, 0, NULL, NULL, &(enodes[2]));
#ifdef DEBUG
    printf("EG_makeTopology(node2) -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    node3[0] = xmax;
    node3[1] = 0;
    node3[2] = 0.000;    // avoid uncaught exception in EG_rotate

    status = EG_makeTopology(context, NULL, NODE, 0, node3, 0, NULL, NULL, &(enodes[3]));
#ifdef DEBUG
    printf("EG_makeTopology(node3) -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    data[0] = node2[0];
    data[1] = node2[1];
    data[2] = node2[2];
    data[3] = node3[0] - node2[0];
    data[4] = node3[1] - node2[1];
    data[5] = node3[2] - node2[2];

    status = EG_makeGeometry(context, CURVE, LINE, NULL, NULL, data, &ecurve);
#ifdef DEBUG
    printf("EG_makeGeometry -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    /* find the range between the Nodes */
    status = EG_invEvaluate(ecurve, node2, &(trange[0]), data);
#ifdef DEBUG
    printf("EG_invEvaluate -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    status = EG_invEvaluate(ecurve, node3, &(trange[1]), data);
#ifdef DEBUG
    printf("EG_invEvaluate -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    /* make the Edge */
    etemp[0] = enodes[2];
    etemp[1] = enodes[3];
    status = EG_makeTopology(context, ecurve, EDGE, TWONODE, trange, 2, etemp, NULL, &eedge);
#ifdef DEBUG
    printf("EG_makeTopology(edge2) -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    sense[0] = SFORWARD;
    status = EG_makeTopology(context, NULL, LOOP, OPEN, NULL, 1, &eedge, sense, &(eloops[1]));
#ifdef DEBUG
    printf("EG_makeTopology(loop1) -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    /* make a face between the Loops */
    status = EG_ruled(2, eloops, &(etemp[0]));
#ifdef DEBUG
    printf("EG_ruled -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    status = EG_getTopology(etemp[0], &eref, &oclass, &mtype, data, &nchild, &echilds, &senses);
#ifdef DEBUG
    printf("EG_getTopology -> status=%d, nchild=%d\n", status, nchild);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    /* rotate the Face one-half of the blade spacing */
    angle = PI / nblades;
    
    matrix[ 0] = 1;   matrix[ 1] = 0;             matrix[ 2] = 0;             matrix[ 3] = 0;
    matrix[ 4] = 0;   matrix[ 5] = +cos(angle);   matrix[ 6] = +sin(angle);   matrix[ 7] = 0;
    matrix[ 8] = 0;   matrix[ 9] = -sin(angle);   matrix[10] = +cos(angle);   matrix[11] = 0;

    status = EG_makeTransform(context, matrix, &exform);
#ifdef DEBUG
    printf("EG_makeTransform -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    status = EG_copyObject(echilds[0], exform, &eface);
#ifdef DEBUG
    printf("EG_copyObject -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    status = EG_deleteObject(exform);
    if (status != EGADS_SUCCESS) goto cleanup;

    /* revolve this Face about the x-axis */
    data[0] = node2[0];
    data[1] = node2[1];
    data[2] = node2[2];
    data[3] = node3[0];
    data[4] = node3[1];
    data[5] = node3[2];

    angle = 360 / nblades;

    status = EG_rotate(eface, angle, data, ebody);
#ifdef DEBUG
    printf("EG_rotate -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    /* remember the body */
    udps[numUdp].ebody = *ebody;

cleanup:
    /* change working directory back */
    chdir("./..");

    if (status != EGADS_SUCCESS) {
        *string = udpErrorStr(status);
    }

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
 *   override_span_chord_ctrl - callback from T-Blade3 to change        *
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
            span_chord_ctrl[ispn] = SPAN_CHORD_CTRL(numUdp, ispn);
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
 *   override_span_thk_c_ctrl - callback from T-Blade3 to change        *
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
            span_thk_c_ctrl[ispn] = SPAN_THK_C_CTRL(numUdp, ispn);
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
 *   override_span_in_beta_ctrl - callback from T-Blade3 to change      *
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
            span_in_beta_ctrl[ispn] = SPAN_IN_BETA_CTRL(numUdp, ispn);
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
 *   override_span_out_beta_ctrl - callback from T-Blade3 to change     *
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
            span_out_beta_ctrl[ispn] = SPAN_OUT_BETA_CTRL(numUdp, ispn);
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
 *   override_span_chord - callback from T-Blade3 to change             *
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
            span_chord[ispn] = SPAN_CHORD(numUdp, ispn);
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
 *   override_span_thk_c - callback from T-Blade3 to change             *
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
            span_thk_c[ispn] = SPAN_THK_C(numUdp, ispn);
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
 *   override_span_u_max - callback from T-Blade3 to change             *
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
            span_u_max[ispn] = SPAN_U_MAX(numUdp, ispn);
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
 *   override_span_in_beta - callback from T-Blade3 to change           *
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
            span_in_beta[ispn] = SPAN_IN_BETA(numUdp, ispn);
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
 *   override_span_out_beta - callback from T-Blade3 to change          *
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
            span_out_beta[ispn] = SPAN_OUT_BETA(numUdp, ispn);
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
 *   override_u2 - callback from T-Blade3 to change u2 array            *
 *                                                                      *
 ************************************************************************
 */

void override_u2_(int *nspn, double u2[])
{
    int    ispn, narg=14;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u2\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u2[ispn] = U2(numUdp, ispn);
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
 *   override_u3 - callback from T-Blade3 to change u3 array            *
 *                                                                      *
 ************************************************************************
 */

void override_u3_(int *nspn, double u3[])
{
    int    ispn, narg=15;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u3\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u3[ispn] = U3(numUdp, ispn);
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
 *   override_u4 - callback from T-Blade3 to change u4 array            *
 *                                                                      *
 ************************************************************************
 */

void override_u4_(int *nspn, double u4[])
{
    int    ispn, narg=16;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u4\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u4[ispn] = U4(numUdp, ispn);
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
 *   override_u5 - callback from T-Blade3 to change u5 array            *
 *                                                                      *
 ************************************************************************
 */

void override_u5_(int *nspn, double u5[])
{
    int    ispn, narg=17;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u5\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u5[ispn] = U5(numUdp, ispn);
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
 *   override_u6 - callback from T-Blade3 to change u6 array            *
 *                                                                      *
 ************************************************************************
 */

void override_u6_(int *nspn, double u6[])
{
    int    ispn, narg=18;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u6\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u6[ispn] = U6(numUdp, ispn);
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
 *   override_cur1 - callback from T-Blade3 to change cur1 array        *
 *                                                                      *
 ************************************************************************
 */

void override_cur1_(int *nspn, double cur1[])
{
    int    ispn, narg=19;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur1\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur1[ispn] = CUR1(numUdp, ispn);
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
 *   override_cur2 - callback from T-Blade3 to change cur2 array        *
 *                                                                      *
 ************************************************************************
 */

void override_cur2_(int *nspn, double cur2[])
{
    int    ispn, narg=20;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur2\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur2[ispn] = CUR2(numUdp, ispn);
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
 *   override_cur3 - callback from T-Blade3 to change cur3 array        *
 *                                                                      *
 ************************************************************************
 */

void override_cur3_(int *nspn, double cur3[])
{
    int    ispn, narg=21;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur3\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur3[ispn] = CUR3(numUdp, ispn);
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
 *   override_cur4 - callback from T-Blade3 to change cur4 array        *
 *                                                                      *
 ************************************************************************
 */

void override_cur4_(int *nspn, double cur4[])
{
    int    ispn, narg=22;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur4\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur4[ispn] = CUR4(numUdp, ispn);
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
 *   override_cur5 - callback from T-Blade3 to change cur5 array        *
 *                                                                      *
 ************************************************************************
 */

void override_cur5_(int *nspn, double cur5[])
{
    int    ispn, narg=23;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur5\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur5[ispn] = CUR5(numUdp, ispn);
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
 *   override_cur6 - callback from T-Blade3 to change cur6 array        *
 *                                                                      *
 ************************************************************************
 */

void override_cur6_(int *nspn, double cur6[])
{
    int    ispn, narg=24;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur6\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur6[ispn] = CUR6(numUdp, ispn);
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
 *   override_cur7 - callback from T-Blade3 to change cur7 array        *
 *                                                                      *
 ************************************************************************
 */

void override_cur7_(int *nspn, double cur7[])
{
    int    ispn, narg=25;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding cur7\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            cur7[ispn] = CUR7(numUdp, ispn);
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
 *   override_span_curv_ctrl - callback from T-Blade3 to change         *
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
            span_curv_ctrl[ispn] = SPAN_CURV_CTRL(numUdp, ispn);
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
 *   override_span_del_m_ctrl - callback from T-Blade3 to change        *
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
            span_del_m_ctrl[ispn] = SPAN_DEL_M_CTRL(numUdp, ispn);
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
 *   override_span_del_theta_ctrl - callback from T-Blade3 to change    *
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
            span_del_theta_ctrl[ispn] = SPAN_DEL_THETA_CTRL(numUdp, ispn);
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
 *   override_span_del_m - callback from T-Blade3 to change             *
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
            span_del_m[ispn] = SPAN_DEL_M(numUdp, ispn);
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
 *   override_span_del_theta - callback from T-Blade3 to change         *
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
            span_del_theta[ispn] = SPAN_DEL_THETA(numUdp, ispn);
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
 *   override_span_thk_ctrl - callback from T-Blade3 to change          *
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
            span_thk_ctrl[ispn] = SPAN_THK_CTRL(numUdp, ispn);
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
 *   override_offsets - callback from T-Blade3 to change                *
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
            offsets[ioffset] = OFFSETS(numUdp, ioffset);
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
 *   override_hub_inf_offset - callback from T-Blade3 to change         *
 *                             hub_inf_offset                           *
 *                                                                      *
 ************************************************************************
 */

void override_hub_inf_offset_(double hub_inf_offset[])
{
    int    narg = 33;

    if (udps[numUdp].arg[narg].size == 1){
        printf(" ==> overriding hub_inf_offset\n");
        hub_inf_offset[0] = HUB_INF_OFFSET(numUdp,0);
        printf("     hub_inf_offset(%2d) = %12.5f\n", 1, hub_inf_offset[0]);
    } else {
        printf(" ==> not overriding hub_inf_offset (noffset=1 but size=%d)\n",
               udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_tip_inf_offset - callback from T-Blade3 to change         *
 *                             tip_inf_offset                           *
 *                                                                      *
 ************************************************************************
 */

void override_tip_inf_offset_(double tip_inf_offset[])
{
    int    narg = 34;

    if (udps[numUdp].arg[narg].size == 1){
        printf(" ==> overriding tip_inf_offset\n");
        tip_inf_offset[0] = TIP_INF_OFFSET(numUdp,0);
        printf("     tip_inf_offset(%2d) = %12.5f\n", 1, tip_inf_offset[0]);
    } else {
        printf(" ==> not overriding tip_inf_offset (noffset=1 but size=%d)\n",
               udps[numUdp].arg[narg].size);
    }
}

/*
 ************************************************************************
 *                                                                      *
 *   override_naca_le_radius - callback from T-Blade3 to change         *
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
            naca_le_radius[ispn] = NACA_LE_RADIUS(numUdp, ispn);
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
 *   override_naca_u_max - callback from T-Blade3 to change             *
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
            naca_u_max[ispn] = NACA_U_MAX(numUdp, ispn);
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
 *   override_naca_t_max - callback from T-Blade3 to change             *
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
            naca_t_max[ispn] = NACA_T_MAX(numUdp, ispn);
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
 *   override_naca_t_te - callback from T-Blade3 to change              *
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
            naca_t_te[ispn] = NACA_T_TE(numUdp, ispn);
            printf("     naca_t_te(%2d) = %12.5f\n", ispn+1, naca_t_te[ispn]);
        }
    } else {
        printf(" ==> not overriding naca_t_te (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}
