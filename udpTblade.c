 /*
 ************************************************************************
 *                                                                      *
 * udpTblade -- udp file to generate a turbomachinery blade             *
 *                                                                      *
 *            Written by John Dannenhoffer @ Syracuse University        *
 *            Patterned after code written by Bob Haimes  @ MIT         *
 *                                                                      *
 ************************************************************************
 */

/*
 * Copyright (C) 2011/2018  John F. Dannenhoffer, III (Syracuse University)
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

#define NUMUDPARGS 40
#include "udpUtilities.h"

/* shorthands for accessing argument values and velocities */
#define NCP(           IUDP)   ((int    *) (udps[IUDP].arg[ 0].val))[0]
#define FILENAME(      IUDP)   ((char   *) (udps[IUDP].arg[ 1].val))
#define AUXNAME(       IUDP)   ((char   *) (udps[IUDP].arg[ 2].val))
/*#define CHORD(         IUDP,I) ((double *) (udps[IUDP].arg[ 3].val))[I]
#define THK_C(         IUDP,I) ((double *) (udps[IUDP].arg[ 4].val))[I]
#define INCI(          IUDP,I) ((double *) (udps[IUDP].arg[ 5].val))[I]
#define DEVN(          IUDP,I) ((double *) (udps[IUDP].arg[ 6].val))[I]*/
#define CUR1(          IUDP,I) ((double *) (udps[IUDP].arg[ 3].val))[I]
#define CUR2(          IUDP,I) ((double *) (udps[IUDP].arg[ 4].val))[I]
#define CUR3(          IUDP,I) ((double *) (udps[IUDP].arg[ 5].val))[I]
#define CUR4(          IUDP,I) ((double *) (udps[IUDP].arg[ 6].val))[I]
#define CUR5(          IUDP,I) ((double *) (udps[IUDP].arg[ 7].val))[I]
#define CUR6(          IUDP,I) ((double *) (udps[IUDP].arg[ 8].val))[I]
#define CUR7(          IUDP,I) ((double *) (udps[IUDP].arg[ 9].val))[I]
/*#define IN_BETA(       IUDP,I) ((double *) (udps[IUDP].arg[13].val))[I]
#define OUT_BETA(      IUDP,I) ((double *) (udps[IUDP].arg[14].val))[I]*/
#define U2(            IUDP,I) ((double *) (udps[IUDP].arg[10].val))[I]
#define U3(            IUDP,I) ((double *) (udps[IUDP].arg[11].val))[I]
#define U4(            IUDP,I) ((double *) (udps[IUDP].arg[12].val))[I]
#define U5(            IUDP,I) ((double *) (udps[IUDP].arg[13].val))[I]
#define U6(            IUDP,I) ((double *) (udps[IUDP].arg[14].val))[I]
#define SPAN_DEL_M(    IUDP,I) ((double *) (udps[IUDP].arg[15].val))[I]
#define SPAN_DEL_THETA(IUDP,I) ((double *) (udps[IUDP].arg[16].val))[I]
#define SPAN_IN_BETA(  IUDP,I) ((double *) (udps[IUDP].arg[17].val))[I]
#define SPAN_OUT_BETA( IUDP,I) ((double *) (udps[IUDP].arg[18].val))[I]
#define SPAN_CHORD(    IUDP,I) ((double *) (udps[IUDP].arg[19].val))[I]
#define SPAN_THK_C(    IUDP,I) ((double *) (udps[IUDP].arg[20].val))[I]
#define SPAN_CURV_CTRL(IUDP,I) ((double *) (udps[IUDP].arg[21].val))[I]
#define SPAN_THK_CTRL( IUDP,I) ((double *) (udps[IUDP].arg[22].val))[I]
#define EXACT_U1(      IUDP,I) ((double *) (udps[IUDP].arg[23].val))[I]
#define EXACT_U2(      IUDP,I) ((double *) (udps[IUDP].arg[24].val))[I]
#define EXACT_U3(      IUDP,I) ((double *) (udps[IUDP].arg[25].val))[I]
#define EXACT_U4(      IUDP,I) ((double *) (udps[IUDP].arg[26].val))[I]
#define EXACT_U5(      IUDP,I) ((double *) (udps[IUDP].arg[27].val))[I]
#define EXACT_U6(      IUDP,I) ((double *) (udps[IUDP].arg[28].val))[I]
#define EXACT_U7(      IUDP,I) ((double *) (udps[IUDP].arg[29].val))[I]
#define EXACT_THK1(    IUDP,I) ((double *) (udps[IUDP].arg[30].val))[I]
#define EXACT_THK2(    IUDP,I) ((double *) (udps[IUDP].arg[31].val))[I]
#define EXACT_THK3(    IUDP,I) ((double *) (udps[IUDP].arg[32].val))[I]
#define EXACT_THK4(    IUDP,I) ((double *) (udps[IUDP].arg[33].val))[I]
#define EXACT_THK5(    IUDP,I) ((double *) (udps[IUDP].arg[34].val))[I]
#define EXACT_THK6(    IUDP,I) ((double *) (udps[IUDP].arg[35].val))[I]
#define EXACT_THK7(    IUDP,I) ((double *) (udps[IUDP].arg[36].val))[I]
#define EXACT_LETHK(   IUDP,I) ((double *) (udps[IUDP].arg[37].val))[I]
#define EXACT_TETHK(   IUDP,I) ((double *) (udps[IUDP].arg[38].val))[I] 
#define THK_FLAGS(     IUDP,I) ((int    *) (udps[IUDP].arg[39].val))[I]

/* data about possible arguments */
static char*  argNames[NUMUDPARGS] = {"ncp",            "filename",     "auxname",
                                      /*"chord",        "thk_c",        "inci",             "devn",*/
                                      "cur1",           "cur2",         "cur3",             "cur4",
                                      "cur5",           "cur6",         "cur7",             /*"in_beta",        "out_beta",*/
                                      "u2",             "u3",           "u4",               "u5",
                                      "u6",             "span_del_m",   "span_del_theta",   "span_in_beta", 
                                      "span_out_beta",  "span_chord",   "span_thk_c",       "span_curv_ctrl",   
                                      "span_thk_ctrl",  "exact_u1",     "exact_u2",         "exact_u3",         
                                      "exact_u4",       "exact_u5",     "exact_u6",         "exact_u7",
                                      "exact_thk1",     "exact_thk2",   "exact_thk3",       "exact_thk4",
                                      "exact_thk5",     "exact_thk6",   "exact_thk7",       "exact_lethk",      
                                      "exact_tethk",    "thk_flags",    };
static int    argTypes[NUMUDPARGS] = {ATTRINT,  ATTRSTRING, ATTRSTRING,
                                      /*ATTRREAL, ATTRREAL,   ATTRREAL,   ATTRREAL,*/
                                      ATTRREAL, ATTRREAL,   ATTRREAL,   ATTRREAL,
                                      ATTRREAL, ATTRREAL,   ATTRREAL,   /*ATTRREAL,   ATTRREAL,*/
                                      ATTRREAL, ATTRREAL,   ATTRREAL,   ATTRREAL,
                                      ATTRREAL, ATTRREAL,   ATTRREAL,   ATTRREAL, 
                                      ATTRREAL, ATTRREAL,   ATTRREAL,   ATTRREAL,    
                                      ATTRREAL, ATTRREAL,   ATTRREAL,   ATTRREAL,
                                      ATTRREAL, ATTRREAL,   ATTRREAL,   ATTRREAL,
                                      ATTRREAL, ATTRREAL,   ATTRREAL,   ATTRREAL,
                                      ATTRREAL, ATTRREAL,   ATTRREAL,   ATTRREAL,
                                      ATTRREAL, ATTRINT,    };
static int    argIdefs[NUMUDPARGS] = {33,       0,          0,
                                      /*0,        0,          0,          0,*/
                                      0,        0,          0,          0,
                                      0,        0,          0,          /*0,          0,*/
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,        
                                      0,        0,          0,          0,      
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,
                                      0,        0,          0,          0,
                                      0,        0,          };
static double argDdefs[NUMUDPARGS] = {33.,      0.,         0.,
                                      /*0.,       0.,         0.,         0.,*/
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         /*0.,         0.,*/
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         0.,       
                                      0.,       0.,         0.,         0.,     
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         0.,         0., 
                                      0.,       0.,         0.,         0.,
                                      0.,       0.,         };

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

#ifdef GRAFIC
   #include "grafic.h"
   #define DEBUG
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

#ifdef WIN32
   extern void   BGB3D_SUB (char fname[], char sname[], char arg2[], char arg3[], char arg4[],
                            int len_fname, int len_sname, int len_arg2, int len_arg3, int len_arg4);

   /*void   OVERRIDE_CHORD (        int *nspn, double chord[         ]);
   void   OVERRIDE_THK_C (        int *nspn, double thk_c[         ]);
   void   OVERRIDE_INCI (         int *nspn, double inci[          ]);
   void   OVERRIDE_DEVN (         int *nspn, double devn[          ]);*/
   void   OVERRIDE_CUR1 (          int *nspn, double cur1[          ]);
   void   OVERRIDE_CUR2 (          int *nspn, double cur2[          ]);
   void   OVERRIDE_CUR3 (          int *nspn, double cur3[          ]);
   void   OVERRIDE_CUR4 (          int *nspn, double cur4[          ]);
   void   OVERRIDE_CUR5 (          int *nspn, double cur5[          ]);
   void   OVERRIDE_CUR6 (          int *nspn, double cur6[          ]);
   void   OVERRIDE_CUR7 (          int *nspn, double cur7[          ]);
   /*void   OVERRIDE_IN_BETA(       int *nspn, double in_beta[       ]);
   void   OVERRIDE_OUT_BETA(      int *nspn, double out_beta[      ]);*/
   void   OVERRIDE_U2 (            int *nspn, double u2[            ]);
   void   OVERRIDE_U3 (            int *nspn, double u3[            ]);
   void   OVERRIDE_U4 (            int *nspn, double u4[            ]);
   void   OVERRIDE_U5 (            int *nspn, double u5[            ]);
   void   OVERRIDE_U6 (            int *nspn, double u6[            ]);
   void   OVERRIDE_SPAN_DEL_M (    int *nspn, double span_del_m[    ]);
   void   OVERRIDE_SPAN_DEL_THETA (int *nspn, double span_del_theta[]);
   void   OVERRIDE_SPAN_IN_BETA (  int *nspn, double span_in_beta[  ]);
   void   OVERRIDE_SPAN_OUT_BETA ( int *nspn, double span_out_beta[ ]);
   void   OVERRIDE_SPAN_CHORD (    int *nspn, double span_chord[    ]);
   void   OVERRIDE_SPAN_THK_C (    int *nspn, double span_thk_c[    ]);
   void   OVERRIDE_SPAN_CURV_CTRL (int *nspn, double span_curv_ctrl[]);
   void   OVERRIDE_SPAN_THK_CTRL ( int *nspn, double span_thk_ctrl[ ]);
   void   OVERRIDE_EXACT_U1 (      int *nspn, double exact_u1[      ]);
   void   OVERRIDE_EXACT_U2 (      int *nspn, double exact_u2[      ]);
   void   OVERRIDE_EXACT_U3 (      int *nspn, double exact_u3[      ]);
   void   OVERRIDE_EXACT_U4 (      int *nspn, double exact_u4[      ]);
   void   OVERRIDE_EXACT_U5 (      int *nspn, double exact_u5[      ]);
   void   OVERRIDE_EXACT_U6 (      int *nspn, double exact_u6[      ]);
   void   OVERRIDE_EXACT_U7 (      int *nspn, double exact_u7[      ]);
   void   OVERRIDE_EXACT_THK1 (    int *nspn, double exact_thk1[    ]);
   void   OVERRIDE_EXACT_THK2 (    int *nspn, double exact_thk2[    ]);
   void   OVERRIDE_EXACT_THK3 (    int *nspn, double exact_thk3[    ]);
   void   OVERRIDE_EXACT_THK4 (    int *nspn, double exact_thk4[    ]);
   void   OVERRIDE_EXACT_THK5 (    int *nspn, double exact_thk5[    ]);
   void   OVERRIDE_EXACT_THK6 (    int *nspn, double exact_thk6[    ]);
   void   OVERRIDE_EXACT_THK7 (    int *nspn, double exact_thk7[    ]);
   void   OVERRIDE_EXACT_LETHK (   int *nspn, double exact_lethk[   ]);
   void   OVERRIDE_EXACT_TETHK (   int *nspn, double exact_tethk[   ]);
   void   OVERRIDE_THK_FLAGS (                int    thk_flags[     ]);
#else
   extern void   bgb3d_sub_(char fname[], char sname[], char arg2[], char arg3[],   char arg4[],
                            int len_fname, int len_sname, int len_arg2, int len_arg3, int len_arg4);

   /*void   override_chord_(         int *nspn, double chord[         ]);
   void   override_thk_c_(         int *nspn, double thk_c[         ]);
   void   override_inci_(          int *nspn, double inci[          ]);
   void   override_devn_(          int *nspn, double devn[          ]);*/
   void   override_cur1_(          int *nspn, double cur1[          ]);
   void   override_cur2_(          int *nspn, double cur2[          ]);
   void   override_cur3_(          int *nspn, double cur3[          ]);
   void   override_cur4_(          int *nspn, double cur4[          ]);
   void   override_cur5_(          int *nspn, double cur5[          ]);
   void   override_cur6_(          int *nspn, double cur6[          ]);
   void   override_cur7_(          int *nspn, double cur7[          ]);
   /*void   override_in_beta_(       int *nspn, double in_beta[       ]);
   void   override_out_beta_(      int *nspn, double out_beta[      ]);*/
   void   override_u2_(            int *nspn, double u2[            ]);
   void   override_u3_(            int *nspn, double u3[            ]);
   void   override_u4_(            int *nspn, double u4[            ]);
   void   override_u5_(            int *nspn, double u5[            ]);
   void   override_u6_(            int *nspn, double u6[            ]);
   void   override_span_del_m_(    int *nspn, double span_del_m[    ]);
   void   override_span_del_theta_(int *nspn, double span_del_theta[]);
   void   override_span_in_beta_(  int *nspn, double span_in_beta[  ]);
   void   override_span_out_beta_( int *nspn, double span_out_beta[ ]);
   void   override_span_chord_(    int *nspn, double span_chord[    ]);
   void   override_span_thk_c_(    int *nspn, double span_thk_c[    ]);
   void   override_span_curv_ctrl_(int *nspn, double span_curv_ctrl[]);
   void   override_span_thk_ctrl_( int *nspn, double span_thk_ctrl[ ]);
   void   override_exact_u1_(      int *nspn, double exact_u1[      ]);
   void   override_exact_u2_(      int *nspn, double exact_u2[      ]);
   void   override_exact_u3_(      int *nspn, double exact_u3[      ]);
   void   override_exact_u4_(      int *nspn, double exact_u4[      ]);
   void   override_exact_u5_(      int *nspn, double exact_u5[      ]);
   void   override_exact_u6_(      int *nspn, double exact_u6[      ]);
   void   override_exact_u7_(      int *nspn, double exact_u7[      ]);
   void   override_exact_thk1_(    int *nspn, double exact_thk1[    ]);
   void   override_exact_thk2_(    int *nspn, double exact_thk2[    ]);
   void   override_exact_thk3_(    int *nspn, double exact_thk3[    ]);
   void   override_exact_thk4_(    int *nspn, double exact_thk4[    ]);
   void   override_exact_thk5_(    int *nspn, double exact_thk5[    ]);
   void   override_exact_thk6_(    int *nspn, double exact_thk6[    ]);
   void   override_exact_thk7_(    int *nspn, double exact_thk7[    ]);
   void   override_exact_lethk_(   int *nspn, double exact_lethk[   ]);
   void   override_exact_tethk_(   int *nspn, double exact_tethk[   ]);
   void   override_thk_flags_(                int    thk_flags[     ]);
#endif

static int    EG_fitBspline(ego context,
                            int npnt, int bitflag, double xyz[],
                            int ncp, ego *ecurve, double *rms);
static int    fit1dCloud(int m, int ordered, double XYZcloud[],
                         int n, double cp[], double *normf);
static int    eval1dBspline(double T, int n, double cp[], double XYZ[],
                            /*@null@*/double dXYZdT[], /*@null@*/double dXYZdP[]);
static int    cubicBsplineBases(int ncp, double t, double N[], double dN[]);
static int    solveSparse(double SAv[], int SAi[], double b[], double x[],
                          int itol, double *errmax, int *iter);
static double L2norm(double f[], int n);

#ifdef GRAFIC
static int    plotCurve(int npnt, double xyz[], ego ecurve);
#endif

#ifdef WIN32
// Returns filename portion of the given path
// Returns empty string if path is directory
const char *basename(const char *path)
{
  const char *filename = strrchr(path, '\\');
  if (filename == NULL)
    filename = path;
  else
    filename++;
  return filename;
}
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
    int     ichar, nsec, isec, npnt, ipnt, jpnt, nn, periodic, senses[4];
    int     oclass, mtype, nbody, *senses2;
    double  xyz[3*NPNT], rms, xyzNode[18], *xr0=NULL;
    double  trange[3], data[18], swap;
    char    filename[257], auxname[257], *auxptr, nextline[257], casename[257];
    FILE    *fp, *fpSrc=NULL, *fpTgt=NULL;
    ego     enodes[5], eedges[4], eloop, *ecurves=NULL;
    ego     ecurve, epcurve, esurface, esurf, eref, efaces[4], emodel, *ebodys;


#ifdef DEBUG
    printf("udpExecute(context=%llx)\n", (long long)context);
    printf("ncp(     0) = %d\n", NCP(     0));
    printf("filename(0) = %s\n", FILENAME(0));
    printf("auxname( 0) = %s\n", AUXNAME( 0));
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
        goto cleanup;
    }

#ifdef DEBUG
    printf("ncp(     %d) = %d\n", numUdp, NCP(     numUdp));
    printf("filename(%d) = %s\n", numUdp, FILENAME(numUdp));
    printf("auxname( %d) = %s\n", numUdp, AUXNAME( numUdp));
#endif

    /* open the input file to extract the casename and number of sections */
    snprintf(filename, 256, "%s", basename(FILENAME(numUdp)));

    fp = fopen(filename, "r");
    if (fp == NULL) {
        printf(" udpExecute: could not open \"%s\"\n", filename);
        status = EGADS_NOTFOUND;
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

    (void) fgets(nextline, 257, fp);
    (void) fgets(nextline, 257, fp);

    (void) fgets(nextline, 257, fp);
    (void) fgets(nextline, 257, fp);
    sscanf(nextline, "%d", &nsec);

    fclose(fp);

    /* try running Tblade */
#ifdef WIN32
    printf("filename=%s\n", filename);
    printf("strlen(filename)=%d\n", (int)strlen(filename));
    printf("auxname=%s\n", AUXNAME(numUdp));
    printf("strlen(auxname)=%d\n", (int)strlen(AUXNAME(numUdp)));
    BGB3D_SUB (filename, AUXNAME(numUdp), "", "", "",
               strlen(filename), strlen(AUXNAME(numUdp)), strlen(""), strlen(""), strlen(""));
#else
    bgb3d_sub_(filename, AUXNAME(numUdp), "", "", "",
               strlen(filename), strlen(AUXNAME(numUdp)), strlen(""), strlen(""), strlen(""));
#endif

    ecurves = (ego*) malloc(nsec*sizeof(ego));
    if (ecurves == NULL) {
        printf(" udpExecute: MALLOC(ecurves) error\n");
        status = EGADS_MALLOC;
        goto cleanup;
    }

    /* initialize (to eliminate scan-build warnings) */
    trange[0] = trange[1] = trange[2] = 0;

    /* process each of the section files */
    for (isec = 1; isec <= nsec; isec++) {

        /* read the input files */
        snprintf(filename, 256, "sec%d.%s.dat", isec, casename);
        printf("reading: %s\n", filename);

        fp = fopen(filename, "r");
        if (fp == NULL) {
            printf("could not open \"%s\"\n", filename);
            status = -9999;
            goto cleanup;
        }

        npnt = 0;
        while (1) {
            nn = fscanf(fp, "%lf %lf %lf\n", &(xyz[3*npnt]), &(xyz[3*npnt+1]), &(xyz[3*npnt+2]));
            if (nn < 3) {
                break;
            } else if (npnt >= NPNT) {
                printf("recompile with larger NPNT\n");
                status = -9999;
                goto cleanup;
            } else {
                npnt++;
            }
        }
        printf("section %2d has %d points\n", isec, npnt);

        assert(npnt > 0);
        xr0 = (double*) malloc(3*npnt*sizeof(double));
        if (xr0 == NULL) {
            status = EGADS_MALLOC;
            goto cleanup;
        }

        /* if the hub or tip, generate a fit for the body of revolution */
        if (isec == 1 || isec == nsec) {
            for (ipnt = 0; ipnt < npnt; ipnt++) {
                xr0[3*ipnt  ] =      xyz[3*ipnt  ];
                xr0[3*ipnt+1] = sqrt(xyz[3*ipnt+1]*xyz[3*ipnt+1]
                                    +xyz[3*ipnt+2]*xyz[3*ipnt+2]);
                xr0[3*ipnt+2] = 0;
            }

            /* order the points based upon x (using SLOW bubble sort) */
            for (ipnt = 0; ipnt < npnt-1; ipnt++) {
                for (jpnt = ipnt+1; jpnt < npnt; jpnt++) {
                    if (xr0[3*ipnt] > xr0[3*jpnt]) {
                        swap = xr0[3*ipnt  ]; xr0[3*ipnt  ] = xr0[3*jpnt  ]; xr0[3*jpnt  ] = swap;
                        swap = xr0[3*ipnt+1]; xr0[3*ipnt+1] = xr0[3*jpnt+1]; xr0[3*jpnt+1] = swap;
                        swap = xr0[3*ipnt+2]; xr0[3*ipnt+2] = xr0[3*jpnt+2]; xr0[3*jpnt+2] = swap;
                    }
                }
            }

            /* arbitrarily extend surface beyond xmin and xmax */
            if (npnt > 25) {
                xr0[3] =     xr0[0];
                xr0[4] =     xr0[1];
                xr0[5] =     xr0[2];
                xr0[0] = 2 * xr0[3] - xr0[30];
                xr0[1] = 2 * xr0[4] - xr0[31];
                xr0[2] = 2 * xr0[5] - xr0[32];

                xr0[3*npnt-6] =     xr0[3*npnt-3];
                xr0[3*npnt-5] =     xr0[3*npnt-2];
                xr0[3*npnt-4] =     xr0[3*npnt-1];
                xr0[3*npnt-3] = 2 * xr0[3*npnt-6] - xr0[3*npnt-30];
                xr0[3*npnt-2] = 2 * xr0[3*npnt-5] - xr0[3*npnt-29];
                xr0[3*npnt-1] = 2 * xr0[3*npnt-4] - xr0[3*npnt-28];
            }

            /* fit r(x) */
            status = EG_fitBspline(context, npnt, 1, xr0, 6, &ecurve, &rms);
#ifdef DEBUG
            printf("EG_fitBspline -> status=%d, rms=%f\n", status, rms);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;

#ifdef GRAFIC
            /* plot the fit */
            status = plotCurve(npnt, xr0, ecurve);
            if (status != EGADS_SUCCESS) goto cleanup;
#endif

            /* generate a body of revolution */
            data[0] = data[1] = data[2] = 0;
            data[3] = 1;
            data[4] = data[5] = 0;
            status = EG_makeGeometry(context, SURFACE, REVOLUTION, ecurve,
                                     NULL, data, &esurface);
#ifdef DEBUG
            printf("EG_makeGeometry -> status=%d\n", status);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;
        }
#ifdef GRAFIC
        /* compute and plot r(x) */
        if (isec == 1 || isec == nsec) {
            int   io_kbd=5, io_scr=6, indgr=1+4+16+64;
            int   nline=1, ilin[1], isym[1], nper[1];
            float xplot[250], rplot[250];

            for (ipnt = 0; ipnt < npnt; ipnt++) {
                xplot[ipnt] = xr0[3*ipnt  ];
                rplot[ipnt] = xr0[3*ipnt+1];
            }
            ilin[0] = -GR_SOLID;
            isym[0] =  GR_CIRCLE;
            nper[0] = npnt;

            grinit_(&io_kbd, &io_scr, "rad(x)", strlen("rad(x)"));
            grline_(ilin, isym, &nline, "~x~r~ ", &indgr,
                    xplot, rplot, nper, strlen("~x~r "));
        }
#endif

        /* generate a fit of the curve */
        status = EG_fitBspline(context, npnt, 3, xyz, NCP(numUdp), &(ecurves[isec-1]), &rms);
#ifdef DEBUG
        printf("EG_fitBspline -> status=%d, rms=%f\n", status, rms);
#endif
        if (status != EGADS_SUCCESS) goto cleanup;

#ifdef GRAFIC
        /* plot the fit */
        status = plotCurve(npnt, xyz, ecurves[isec-1]);
        if (status != EGADS_SUCCESS) goto cleanup;
#endif

        /* get the range of the Curves */
        status = EG_getRange(ecurves[isec-1], trange, &periodic);
        trange[2] =  trange[1];
        trange[1] = (trange[0] + trange[2]) / 2;

#ifdef DEBUG
        printf("EG_getRange -> status=%d\n", status);
        printf("trange =%f %f %f\n", trange[0], trange[1], trange[2]);
#endif
        if (status != EGADS_SUCCESS) goto cleanup;

        /* special processing for hub and tip */
        if (isec == 1 || isec == nsec) {

            /* create the two Nodes for this section */
            status = EG_evaluate(ecurves[isec-1], trange, xyzNode);
#ifdef DEBUG
            printf("EG_evaluate -> status=%d, xyzNode=%f %f %f\n",
                   status, xyzNode[0], xyzNode[1], xyzNode[2]);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;

            status = EG_makeTopology(context, NULL, NODE, 0,
                                     xyzNode, 0, NULL, NULL, &(enodes[0]));
#ifdef DEBUG
            printf("EG_makeTopology(node0) -> status=%d\n", status);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;

            status = EG_evaluate(ecurves[isec-1], &(trange[1]), xyzNode);
#ifdef DEBUG
            printf("EG_evaluate -> status=%d, xyzNode=%f %f %f\n",
                   status, xyzNode[0], xyzNode[1], xyzNode[2]);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;

            status = EG_makeTopology(context, NULL, NODE, 0,
                                     xyzNode, 0, NULL, NULL, &(enodes[1]));
#ifdef DEBUG
            printf("EG_makeTopology(node1) -> status=%d\n", status);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;

            enodes[2] = enodes[0];

            /* create the two Edges for this section */
            status = EG_makeTopology(context, ecurves[isec-1], EDGE, TWONODE,
                                     trange, 2, &(enodes[0]), NULL, &(eedges[0]));
#ifdef DEBUG
            printf("EG_makeTopology(edge0) -> status=%d\n", status);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;

            status = EG_makeTopology(context, ecurves[isec-1], EDGE, TWONODE,
                                     &(trange[1]), 2, &(enodes[1]), NULL, &(eedges[1]));
#ifdef DEBUG
            printf("EG_makeTopology(edge1) -> status=%d\n", status);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;

            /* create the Loop for this section */
            status = EG_otherCurve(esurface, ecurves[isec-1], 0, &epcurve);
#ifdef DEBUG
            printf("EG_otherCurve -> status=%d\n", status);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;

            eedges[2] = epcurve;
            eedges[3] = epcurve;

            senses[0] = SFORWARD;
            senses[1] = SFORWARD;
            status = EG_makeTopology(context, esurface, LOOP, CLOSED,
                                     NULL, 2, eedges, senses, &eloop);
#ifdef DEBUG
            printf("EG_makeTopology(loop) -> status=%d\n", status);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;

            if (isec == 1) {
                status = EG_makeTopology(context, esurface, FACE, SFORWARD,
                                         NULL, 1, &eloop, senses, &(efaces[2]));
            } else {
                status = EG_makeTopology(context, esurface, FACE, SFORWARD,
                                         NULL, 1, &eloop, senses, &(efaces[3]));
            }
#ifdef DEBUG
            printf("EG_makeTopology(face) -> status=%d\n", status);
#endif
            if (status != EGADS_SUCCESS) goto cleanup;
        }

        fclose(fp);

        free(xr0);
        xr0 = NULL;
    }

    /* create the blde surface */
    status = EG_skinning(context, nsec, ecurves, 3, &esurf);
#ifdef DEBUG
    printf("EG_skinning -> status=%d\n", status);

    printf("esurf:\n");
    printEgo(esurf);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;
        
    /* break the surface into 2 Faces */
    data[0] = trange[0];
    data[1] = trange[1];
    data[2] = 0;
    data[3] = 1;
    status = EG_makeFace(esurf, SFORWARD, data, &(efaces[0]));
#ifdef DEBUG
    printf("EG_makeFace -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    data[0] = trange[1];
    data[1] = trange[2];
    data[2] = 0;
    data[3] = 1;
    status = EG_makeFace(esurf, SFORWARD, data, &(efaces[1]));
#ifdef DEBUG
    printf("EG_makeFace -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    /* sew the Faces into a SolidBody */
    status = EG_sewFaces(4, efaces, 0, 0, &emodel);
    printf("EG_sewFaces -> status=%d\n", status);

    status = EG_getTopology(emodel, &eref, &oclass, &mtype,
                            data, &nbody, &ebodys, &senses2);
#ifdef DEBUG
    printf("EG_getTopology(model) -> status=%d\n", status);
#endif
    if (status != EGADS_SUCCESS) goto cleanup;

    *ebody = ebodys[0];

    /* set the output value(s) */

    /* remember this model (body) */
    udps[numUdp].ebody = *ebody;

#ifdef DEBUG
    printf("udpExecute -> *ebody=%llx\n", (long long)(*ebody));
#endif

cleanup:

    /* change working directory back */
    chdir("./..");

    if (ecurves != NULL) free(ecurves);

    if (xr0 != NULL) free(xr0);

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
 *   override_chord - callback from Tblade3 to change chord array       *
 *                                                                      *
 ************************************************************************
 */

/*#ifdef WIN32
void OVERRIDE_CHORD (int *nspn, double chord[])
#else
void override_chord_(int *nspn, double chord[])
#endif
{
    int    ispn, narg=3;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding chord\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            chord[ispn] = CHORD(numUdp,ispn);
            printf("     chord(%2d) = %12.5f\n", ispn+1, chord[ispn]);
        }
    } else {
        printf(" ==> not overriding chord (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}*/


/*
 ************************************************************************
 *                                                                      *
 *   override_thk_c - callback from Tblade3 to change thk_c array       *
 *                                                                      *
 ************************************************************************
 */

/*#ifdef WIN32
void OVERRIDE_THK_C (int *nspn, double thk_c[])
#else
void override_thk_c_(int *nspn, double thk_c[])
#endif
{
    int    ispn, narg=4;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding thk_c\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            thk_c[ispn] = THK_C(numUdp,ispn);
            printf("     thk_c(%2d) = %12.5f\n", ispn+1, thk_c[ispn]);
        }
    } else {
        printf(" ==> not overriding thk_c (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}*/


/*
 ************************************************************************
 *                                                                      *
 *   override_inci - callback from Tblade3 to change inci array         *
 *                                                                      *
 ************************************************************************
 */

/*#ifdef WIN32
void OVERRIDE_INCI (int *nspn, double inci[])
#else
void override_inci_(int *nspn, double inci[])
#endif
{
    int    ispn, narg=5;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding inci\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            inci[ispn] = INCI(numUdp,ispn);
            printf("     inci(%2d) = %12.5f\n", ispn+1, inci[ispn]);
        }
    } else {
        printf(" ==> not overriding inci (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}*/


/*
 ************************************************************************
 *                                                                      *
 *   override_devn - callback from Tblade3 to change devn array         *
 *                                                                      *
 ************************************************************************
 */

/*#ifdef WIN32
void OVERRIDE_DEVN (int *nspn, double devn[])
#else
void override_devn_(int *nspn, double devn[])
#endif
{
    int    ispn, narg=6;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding devn\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            devn[ispn] = DEVN(numUdp,ispn);
            printf("     devn(%2d) = %12.5f\n", ispn+1, devn[ispn]);
        }
    } else {
        printf(" ==> not overriding devn (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}*/


/*
 ************************************************************************
 *                                                                      *
 *   override_cur1 - callback from Tblade3 to change cur1 array         *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_CUR1 (int *nspn, double cur1[])
#else
void override_cur1_(int *nspn, double cur1[])
#endif
{
    int    ispn, narg=3;

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

#ifdef WIN32
void OVERRIDE_CUR2 (int *nspn, double cur2[])
#else
void override_cur2_(int *nspn, double cur2[])
#endif
{
    int    ispn, narg=4;

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

#ifdef WIN32
void OVERRIDE_CUR3 (int *nspn, double cur3[])
#else
void override_cur3_(int *nspn, double cur3[])
#endif
{
    int    ispn, narg=5;

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

#ifdef WIN32
void OVERRIDE_CUR4 (int *nspn, double cur4[])
#else
void override_cur4_(int *nspn, double cur4[])
#endif
{
    int    ispn, narg=6;

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

#ifdef WIN32
void OVERRIDE_CUR5 (int *nspn, double cur5[])
#else
void override_cur5_(int *nspn, double cur5[])
#endif
{
    int    ispn, narg=7;

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

#ifdef WIN32
void OVERRIDE_CUR6 (int *nspn, double cur6[])
#else
void override_cur6_(int *nspn, double cur6[])
#endif
{
    int    ispn, narg=8;

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

#ifdef WIN32
void OVERRIDE_CUR7 (int *nspn, double cur7[])
#else
void override_cur7_(int *nspn, double cur7[])
#endif
{
    int    ispn, narg=9;

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
 *   override_in_beta - callback from Tblade3 to change in_beta array   *
 *                                                                      *
 ************************************************************************
 */

/*#ifdef WIN32
void OVERRIDE_IN_BETA (int *nspn, double in_beta[])
#else
void override_in_beta_(int *nspn, double in_beta[])
#endif
{
    int    ispn, narg=13;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding in_beta\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            in_beta[ispn] = IN_BETA(numUdp,ispn);
            printf("     in_beta(%2d) = %12.5f\n", ispn+1, in_beta[ispn]);
        }
    } else {
        printf(" ==> not overriding in_beta (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}*/


/*
 ************************************************************************
 *                                                                      *
 *   override_out_beta - callback from Tblade3 to change out_beta array *
 *                                                                      *
 ************************************************************************
 */

/*#ifdef WIN32
void OVERRIDE_OUT_BETA (int *nspn, double out_beta[])
#else
void override_out_beta_(int *nspn, double out_beta[])
#endif
{
    int    ispn, narg=14;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding out_beta\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            out_beta[ispn] = OUT_BETA(numUdp,ispn);
            printf("     out_beta(%2d) = %12.5f\n", ispn+1, out_beta[ispn]);
        }
    } else {
        printf(" ==> not overriding out_beta (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}*/


/*
 ************************************************************************
 *                                                                      *
 *   override_u2 - callback from Tblade3 to change u2 array             *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_U2 (int *nspn, double u2[])
#else
void override_u2_(int *nspn, double u2[])
#endif
{
    int    ispn, narg=10;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u2\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u2[ispn] = U2(numUdp,ispn);
            printf("     u2(%2d) = %12.5f\n", ispn+1, u2[ispn]);
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

#ifdef WIN32
void OVERRIDE_U3 (int *nspn, double u3[])
#else
void override_u3_(int *nspn, double u3[])
#endif
{
    int    ispn, narg=11;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u3\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u3[ispn] = U3(numUdp,ispn);
            printf("     u3(%2d) = %12.5f\n", ispn+1, u3[ispn]);
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

#ifdef WIN32
void OVERRIDE_U4 (int *nspn, double u4[])
#else
void override_u4_(int *nspn, double u4[])
#endif
{
    int    ispn, narg=12;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u4\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u4[ispn] = U4(numUdp,ispn);
            printf("     u4(%2d) = %12.5f\n", ispn+1, u4[ispn]);
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

#ifdef WIN32
void OVERRIDE_U5 (int *nspn, double u5[])
#else
void override_u5_(int *nspn, double u5[])
#endif
{
    int    ispn, narg=13;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u5\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u5[ispn] = U5(numUdp,ispn);
            printf("     u5(%2d) = %12.5f\n", ispn+1, u5[ispn]);
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

#ifdef WIN32
void OVERRIDE_U6 (int *nspn, double u6[])
#else
void override_u6_(int *nspn, double u6[])
#endif
{
    int    ispn, narg=14;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding u6\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            u6[ispn] = U6(numUdp,ispn);
            printf("     u6(%2d) = %12.5f\n", ispn+1, u6[ispn]);
        }
    } else {
        printf(" ==> not overriding u6 (nspn=%d but size=%d)\n",
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

#ifdef WIN32
void OVERRIDE_SPAN_DEL_M (int *nspn, double span_del_m[])
#else
void override_span_del_m_(int *nspn, double span_del_m[])
#endif
{
    int    ispn, narg = 15;

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

#ifdef WIN32
void OVERRIDE_SPAN_DEL_THETA (int *nspn, double span_del_theta[])
#else
void override_span_del_theta_(int *nspn, double span_del_theta[])
#endif
{
    int    ispn, narg = 16;

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
 *   override_span_in_beta - callback from Tblade3 to change            *
 *                           span_in_beta array                         *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_SPAN_IN_BETA (int *nspn, double span_in_beta[])
#else
void override_span_in_beta_(int *nspn, double span_in_beta[])
#endif
{
    int    ispn, narg=17;

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

#ifdef WIN32
void OVERRIDE_SPAN_OUT_BETA (int *nspn, double span_out_beta[])
#else
void override_span_out_beta_(int *nspn, double span_out_beta[])
#endif
{
    int    ispn, narg=18;

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
 *   override_span_chord - callback from Tblade3 to change              *
 *                         span_chord array                             *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_SPAN_CHORD (int *nspn, double span_chord[])
#else
void override_span_chord_(int *nspn, double span_chord[])
#endif
{
    int    ispn, narg=19;

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

#ifdef WIN32
void OVERRIDE_SPAN_THK_C (int *nspn, double span_thk_c[])
#else
void override_span_thk_c_(int *nspn, double span_thk_c[])
#endif
{
    int    ispn, narg=20;

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
 *   override_span_curv_ctrl - callback from Tblade3 to change          *
 *                             span_curv_ctrl array                     *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_SPAN_CURV_CTRL (int *nspn, double span_curv_ctrl[])
#else
void override_span_curv_ctrl_(int *nspn, double span_curv_ctrl[])
#endif
{
    int    ispn, narg=21;

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
 *   override_span_thk_ctrl - callback from Tblade3 to change           *
 *                            span_thk_ctrl array                       *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_SPAN_THK_CTRL (int *nspn, double span_thk_ctrl[])
#else
void override_span_thk_ctrl_(int *nspn, double span_thk_ctrl[])
#endif
{
    int    ispn, narg=22;

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
 *   override_exact_u1 - callback from Tblade3 to change                *
 *                       exact_u1 array                                 *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_U1 (int *nspn, double exact_u1[])
#else
void override_exact_u1_(int *nspn, double exact_u1[])
#endif
{
    int    ispn, narg=23;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_u1\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_u1[ispn] = EXACT_U1(numUdp,ispn);
            printf("     exact_u1(%2d) = %12.5f\n", ispn+1, exact_u1[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_u1 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_u2 - callback from Tblade3 to change                *
 *                       exact_u2 array                                 *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_U2 (int *nspn, double exact_u2[])
#else
void override_exact_u2_(int *nspn, double exact_u2[])
#endif
{
    int    ispn, narg=24;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_u2\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_u2[ispn] = EXACT_U2(numUdp,ispn);
            printf("     exact_u2(%2d) = %12.5f\n", ispn+1, exact_u2[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_u2 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_u3 - callback from Tblade3 to change                *
 *                       exact_u3 array                                 *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_U3 (int *nspn, double exact_u3[])
#else
void override_exact_u3_(int *nspn, double exact_u3[])
#endif
{
    int    ispn, narg=25;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_u3\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_u3[ispn] = EXACT_U3(numUdp,ispn);
            printf("     exact_u3(%2d) = %12.5f\n", ispn+1, exact_u3[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_u3 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_u4 - callback from Tblade3 to change                *
 *                       exact_u4 array                                 *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_U4 (int *nspn, double exact_u4[])
#else
void override_exact_u4_(int *nspn, double exact_u4[])
#endif
{
    int    ispn, narg=26;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_u4\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_u4[ispn] = EXACT_U4(numUdp,ispn);
            printf("     exact_u4(%2d) = %12.5f\n", ispn+1, exact_u4[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_u4 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_u5 - callback from Tblade3 to change                *
 *                       exact_u5 array                                 *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_U5 (int *nspn, double exact_u5[])
#else
void override_exact_u5_(int *nspn, double exact_u5[])
#endif
{
    int    ispn, narg=27;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_u5\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_u5[ispn] = EXACT_U5(numUdp,ispn);
            printf("     exact_u5(%2d) = %12.5f\n", ispn+1, exact_u5[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_u5 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_u6 - callback from Tblade3 to change                *
 *                       exact_u6 array                                 *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_U6 (int *nspn, double exact_u6[])
#else
void override_exact_u6_(int *nspn, double exact_u6[])
#endif
{
    int    ispn, narg=28;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_u6\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_u6[ispn] = EXACT_U6(numUdp,ispn);
            printf("     exact_u6(%2d) = %12.5f\n", ispn+1, exact_u6[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_u6 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_u7 - callback from Tblade3 to change                *
 *                       exact_u7 array                                 *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_U7 (int *nspn, double exact_u7[])
#else
void override_exact_u7_(int *nspn, double exact_u7[])
#endif
{
    int    ispn, narg=29;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_u7\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_u7[ispn] = EXACT_U7(numUdp,ispn);
            printf("     exact_u7(%2d) = %12.5f\n", ispn+1, exact_u7[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_u7 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_thk1 - callback from Tblade3 to change              *
 *                         exact_thk1 array                             *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_THK1 (int *nspn, double exact_thk1[])
#else
void override_exact_thk1_(int *nspn, double exact_thk1[])
#endif
{
    int    ispn, narg=30;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_thk1\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_thk1[ispn] = EXACT_THK1(numUdp,ispn);
            printf("     exact_thk1(%2d) = %12.5f\n", ispn+1, exact_thk1[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_thk1 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_thk2 - callback from Tblade3 to change              *
 *                         exact_thk2 array                             *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_THK2 (int *nspn, double exact_thk2[])
#else
void override_exact_thk2_(int *nspn, double exact_thk2[])
#endif
{
    int    ispn, narg=31;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_thk2\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_thk2[ispn] = EXACT_THK2(numUdp,ispn);
            printf("     exact_thk2(%2d) = %12.5f\n", ispn+1, exact_thk2[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_thk2 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_thk3 - callback from Tblade3 to change              *
 *                         exact_thk3 array                             *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_THK3 (int *nspn, double exact_thk3[])
#else
void override_exact_thk3_(int *nspn, double exact_thk3[])
#endif
{
    int    ispn, narg=32;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_thk3\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_thk3[ispn] = EXACT_THK3(numUdp,ispn);
            printf("     exact_thk3(%2d) = %12.5f\n", ispn+1, exact_thk3[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_thk3 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_thk4 - callback from Tblade3 to change              *
 *                         exact_thk4 array                             *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_THK4 (int *nspn, double exact_thk4[])
#else
void override_exact_thk4_(int *nspn, double exact_thk4[])
#endif
{
    int    ispn, narg=33;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_thk4\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_thk4[ispn] = EXACT_THK4(numUdp,ispn);
            printf("     exact_thk4(%2d) = %12.5f\n", ispn+1, exact_thk4[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_thk4 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_thk5 - callback from Tblade3 to change              *
 *                         exact_thk5 array                             *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_THK5 (int *nspn, double exact_thk5[])
#else
void override_exact_thk5_(int *nspn, double exact_thk5[])
#endif
{
    int    ispn, narg=34;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_thk5\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_thk5[ispn] = EXACT_THK5(numUdp,ispn);
            printf("     exact_thk5(%2d) = %12.5f\n", ispn+1, exact_thk5[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_thk5 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_thk6 - callback from Tblade3 to change              *
 *                         exact_thk6 array                             *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_THK6 (int *nspn, double exact_thk6[])
#else
void override_exact_thk6_(int *nspn, double exact_thk6[])
#endif
{
    int    ispn, narg=35;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_thk6\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_thk6[ispn] = EXACT_THK6(numUdp,ispn);
            printf("     exact_thk6(%2d) = %12.5f\n", ispn+1, exact_thk6[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_thk6 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_thk7 - callback from Tblade3 to change              *
 *                         exact_thk7 array                             *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_THK7 (int *nspn, double exact_thk7[])
#else
void override_exact_thk7_(int *nspn, double exact_thk7[])
#endif
{
    int    ispn, narg=36;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_thk7\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_thk7[ispn] = EXACT_THK7(numUdp,ispn);
            printf("     exact_thk7(%2d) = %12.5f\n", ispn+1, exact_thk7[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_thk7 (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_lethk - callback from Tblade3 to change             *
 *                          exact_lethk array                           *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_LETHK (int *nspn, double exact_lethk[])
#else
void override_exact_lethk_(int *nspn, double exact_lethk[])
#endif
{
    int    ispn, narg=37;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_lethk\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_lethk[ispn] = EXACT_LETHK(numUdp,ispn);
            printf("     exact_lethk(%2d) = %12.5f\n", ispn+1, exact_lethk[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_lethk (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_exact_tethk - callback from Tblade3 to change             *
 *                          exact_tethk array                           *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_EXACT_TETHK (int *nspn, double exact_tethk[])
#else
void override_exact_tethk_(int *nspn, double exact_tethk[])
#endif
{
    int    ispn, narg=38;

    if (udps[numUdp].arg[narg].size == *nspn) {
        printf(" ==> overriding exact_tethk\n");
        for (ispn = 0; ispn < *nspn; ispn++) {
            exact_tethk[ispn] = EXACT_TETHK(numUdp,ispn);
            printf("     exact_tethk(%2d) = %12.5f\n", ispn+1, exact_tethk[ispn]);
        }
    } else {
        printf(" ==> not overriding exact_tethk (nspn=%d but size=%d)\n",
               *nspn, udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   override_thk_flags - callback from Tblade3 to change               *
 *                        thk_flags array                               *
 *                                                                      *
 ************************************************************************
 */

#ifdef WIN32
void OVERRIDE_THK_FLAGS (int thk_flags[])
#else
void override_thk_flags_(int thk_flags[])
#endif
{
    int    iflag, narg=39;

    if (udps[numUdp].arg[narg].size == 3) {
        printf(" ==> overriding thk_flags\n");
        for (iflag = 0; iflag < 3; iflag++) {
            thk_flags[iflag] = THK_FLAGS(numUdp,iflag);
            printf("     thk_flags(%2d) = %d\n", iflag+1, thk_flags[iflag]);
        }
    } else {
        printf(" ==> not overriding thk_flags (nflag=3 but size=%d)\n",
               udps[numUdp].arg[narg].size);
    }
}


/*
 ************************************************************************
 *                                                                      *
 *   EG_fitBspline - fit a degree=3 Bspline curve to a set of points    *
 *                                                                      *
 ************************************************************************
 */

static int
EG_fitBspline(ego    context,           /* (in)  EGADS context */
              int    npnt,              /* (in)  number of points */
              int    bitflag,           /* (in)  1=ordered, 2=periodic */
              double xyz[],             /* (in)  array of points (xyzxyz...) */
              int    ncp,               /* (in)  number of control points */
              ego    *ecurve,           /* (out) Bspline curve (degree=3) */
              double *rms)              /* (out) RMS distance of points from curve */
{
    int    status = EGADS_SUCCESS;

    int    nknot, ndata, idata, j, header[4];
    double *cpdata=NULL;

    /* --------------------------------------------------------------- */

    /* default returns */
    *ecurve = NULL;
    *rms    = 0;

    /* check the inputs */
    if (context == NULL) {
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
    }

    /* set up arrays needed to define Bspline */
    nknot = ncp + 4;
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

    ndata = 0;

    /* knot vector */
    cpdata[ndata++] = 0;
    cpdata[ndata++] = 0;
    cpdata[ndata++] = 0;
    cpdata[ndata++] = 0;

    for (j = 1; j < ncp-3; j++) {
        cpdata[ndata++] = j;
    }

    cpdata[ndata++] = ncp - 3;
    cpdata[ndata++] = ncp - 3;
    cpdata[ndata++] = ncp - 3;
    cpdata[ndata++] = ncp - 3;

    /* control points at the two ends */
    idata = ndata;
    cpdata[idata  ] = xyz[0];
    cpdata[idata+1] = xyz[1];
    cpdata[idata+2] = xyz[2];

    idata = ndata + 3 * (ncp-1);
    cpdata[idata  ] = xyz[3*npnt-3];
    cpdata[idata+1] = xyz[3*npnt-2];
    cpdata[idata+2] = xyz[3*npnt-1];

    /* perform the fitting (which updates the interior control points) */
    status = fit1dCloud(npnt, bitflag, xyz,
                        ncp,  &(cpdata[ndata]), rms);
    if (status < EGADS_SUCCESS) goto cleanup;

    /* make the geometry */
    status = EG_makeGeometry(context, CURVE, BSPLINE, NULL,
                             header, cpdata, ecurve);

cleanup:
    if (cpdata != NULL) free(cpdata);

    return status;
}


/*
 ************************************************************************
 *                                                                      *
 *   fit1dCloud - find spline that best-fits the cloud of points        *
 *                                                                      *
 ************************************************************************
 */

static int
fit1dCloud(int    m,                    /* (in)  number of points in cloud */
           int    bitflag,              /* (in)  1=ordered, 2=periodic */
           double XYZcloud[],           /* (in)  array  of points in cloud */
           int    n,                    /* (in)  number of control points */
           double cp[],                 /* (in)  array  of control points (first and last are set) */
                                        /* (out) array  of control points (all set) */
           double *normf)               /* (out) RMS of distances between cloud and fit */
{
    int    status = EGADS_SUCCESS;      /* (out)  return status */

    int    ordered=0, periodic=0, np, nvar, ivar, jvar, nobj, iobj, i, j, k, next;
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

    ROUTINE(fit1dCloud);

    /* --------------------------------------------------------------- */

#ifdef DEBUG
    printf("enter fit1dCloud(m=%d, bitflag=%d, n=%d)\n", m, bitflag, n);
#endif

    assert(m > 1);                      // needed to avoid clang warning
    assert(n > 2);                      // needed to avoid clang warning

    /* default return */
    *normf  = 1e-12;

    /* extract ordered and periodic flags */
    if (bitflag == 1 || bitflag == 3) ordered  = 1;
    if (bitflag == 2 || bitflag == 3) periodic = 1;

    /* number of design variables and objectives */
    np   = 3 * n - 6;
    nvar = m + np;
    nobj = 3 * m;

    /* if m < n, then assume that the linear spline is the best fit */
    if (m < n) {
        for (j = 1; j < n-1; j++) {
            frac = (double)(j) / (double)(n-1);

            cp[3*j  ] = (1-frac) * cp[0] + frac * cp[3*n-3];
            cp[3*j+1] = (1-frac) * cp[1] + frac * cp[3*n-2];
            cp[3*j+2] = (1-frac) * cp[2] + frac * cp[3*n-1];
        }

#ifdef DEBUG
        printf("making linear fit because not enough points in cloud\n");
#endif
        goto cleanup;
    }

    /* allocate all temporary arrays */
    XYZcopy = (double *) malloc(3*m*sizeof(double));
    dXYZdP  = (double *) malloc(  n*sizeof(double));
    cpnew   = (double *) malloc(3*n*sizeof(double));

    beta    = (double *) malloc(nvar*sizeof(double));
    delta   = (double *) malloc(nvar*sizeof(double));
    betanew = (double *) malloc(nvar*sizeof(double));

    f       = (double *) malloc(nobj*sizeof(double));
    fnew    = (double *) malloc(nobj*sizeof(double));

    aa      = (double *) malloc(m    *sizeof(double));
    bb      = (double *) malloc(m *np*sizeof(double));
    cc      = (double *) malloc(np*np*sizeof(double));
    rhs     = (double *) malloc(nvar *sizeof(double));

    if (XYZcopy == NULL ||dXYZdP == NULL || cpnew == NULL ||
        beta    == NULL || delta == NULL || betanew == NULL ||
        f       == NULL || fnew  == NULL ||
        aa      == NULL || bb    == NULL || cc == NULL || rhs == NULL) {
        status = EGADS_MALLOC;
        goto cleanup;
    }

#define AA(I)       aa[(I)]
#define BB(I,J)     bb[(J)+np*(I)]
#define CC(I,J)     cc[(J)+np*(I)]

    /* transform inputs so that they are centered at origin and
       unit length */
    xmin = XYZcloud[0];
    xmax = XYZcloud[0];
    ymin = XYZcloud[1];
    ymax = XYZcloud[1];
    zmin = XYZcloud[2];
    zmax = XYZcloud[2];

    for (k = 1; k < m; k++) {
        if (XYZcloud[3*k  ] < xmin) xmin = XYZcloud[3*k  ];
        if (XYZcloud[3*k  ] > xmax) xmax = XYZcloud[3*k  ];
        if (XYZcloud[3*k+1] < ymin) ymin = XYZcloud[3*k+1];
        if (XYZcloud[3*k+1] > ymax) ymax = XYZcloud[3*k+1];
        if (XYZcloud[3*k+2] < zmin) zmin = XYZcloud[3*k+2];
        if (XYZcloud[3*k+2] > zmax) zmax = XYZcloud[3*k+2];
    }

    scale = 1.0 / MAX(MAX(xmax-xmin, ymax-ymin), zmax-zmin);
    xcent = scale * (xmin + xmax) / 2;
    ycent = scale * (ymin + ymax) / 2;
    zcent = scale * (zmin + zmax) / 2;

    for (k = 0; k < m; k++) {
        XYZcopy[3*k  ] = scale * (XYZcloud[3*k  ] - xcent);
        XYZcopy[3*k+1] = scale * (XYZcloud[3*k+1] - ycent);
        XYZcopy[3*k+2] = scale * (XYZcloud[3*k+2] - zcent);
    }
    for (j = 0; j < n; j++) {
        cp[3*j  ] = scale * (cp[3*j  ] - xcent);
        cp[3*j+1] = scale * (cp[3*j+1] - ycent);
        cp[3*j+2] = scale * (cp[3*j+2] - zcent);
    }

    /* set up the initial values for the interior control
       points and the initial values of "t" */

    /* XYZcopy is ordered */
    if (ordered == 1) {

        /* set the initial control point locations by picking up evenly
           spaced points (based upon point number) from the cloud */
        for (j = 1; j < n-1; j++) {
            i = (j * (m-1)) / (n-1);

            cp[3*j  ] = XYZcopy[3*i  ];
            cp[3*j+1] = XYZcopy[3*i+1];
            cp[3*j+2] = XYZcopy[3*i+2];
        }

        /* for each point in the cloud, assign the value of "t"
           (which is stored in the first m betas) based upon it
           local pseudo-arc-length */
        beta[0] = 0;
        for (k = 1; k < m; k++) {
            beta[k] = beta[k-1] + sqrt(SQR(XYZcopy[3*k  ]-XYZcopy[3*k-3])
                                      +SQR(XYZcopy[3*k+1]-XYZcopy[3*k-2])
                                      +SQR(XYZcopy[3*k+2]-XYZcopy[3*k-1]));
        }

        for (k = 0; k < m; k++) {
            beta[k] = (n-3) * beta[k] / beta[m-1];
        }

    /* XYZcopy is unordered */
    } else {

        /* set the "center" control point to coincide with the point
           in the cloud that is furthest away from the first and
           last control points */
        dmax = 0;
        for (k = 1; k < m-1; k++) {
            dist1 = pow(XYZcopy[3*k  ]-cp[0], 2)
                  + pow(XYZcopy[3*k+1]-cp[1], 2)
                  + pow(XYZcopy[3*k+2]-cp[2], 2);
            dist2 = pow(XYZcopy[3*k  ]-cp[3*n-3], 2)
                  + pow(XYZcopy[3*k+1]-cp[3*n-2], 2)
                  + pow(XYZcopy[3*k+2]-cp[3*n-1], 2);
            dist  = MIN(dist1, dist2);

            if (dist > dmax) {
                dmax = dist;
                cp[3*(n/2)  ] = XYZcopy[3*k  ];
                cp[3*(n/2)+1] = XYZcopy[3*k+1];
                cp[3*(n/2)+2] = XYZcopy[3*k+2];
            }
        }

        /* fill in the other control points */
        for (j = 1; j < (n/2); j++) {
            frac = (double)(j) / (double)(n/2);

            cp[3*j  ] = (1-frac) * cp[0] + frac * cp[3*(n/2)  ];
            cp[3*j+1] = (1-frac) * cp[1] + frac * cp[3*(n/2)+1];
            cp[3*j+2] = (1-frac) * cp[2] + frac * cp[3*(n/2)+2];
        }

        for (j = (n/2)+1; j < n; j++) {
            frac = (double)(j-(n/2)) / (double)(n-1-(n/2));

            cp[3*j  ] = (1-frac) * cp[3*(n/2)  ] + frac * cp[3*n-3];
            cp[3*j+1] = (1-frac) * cp[3*(n/2)+1] + frac * cp[3*n-2];
            cp[3*j+2] = (1-frac) * cp[3*(n/2)+2] + frac * cp[3*n-1];
        }

        /* for each point in the cloud, assign the value of "t"
           (which is stored in the first m betas) as the closest
           point to the control polygon */
        for (k = 0; k < m; k++) {
            xx = XYZcopy[3*k  ];
            yy = XYZcopy[3*k+1];
            zz = XYZcopy[3*k+2];

            dmin = HUGEQ;
            for (j = 1; j < n; j++) {
                xb = cp[3*j-3];   yb = cp[3*j-2];   zb = cp[3*j-1];
                xe = cp[3*j  ];   ye = cp[3*j+1];   ze = cp[3*j+2];

                tt = ((xe-xb) * (xx-xb) + (ye-yb) * (yy-yb) + (ze-zb) * (zz-zb))
                   / ((xe-xb) * (xe-xb) + (ye-yb) * (ye-yb) + (ze-zb) * (ze-zb));
                tt = MIN(MAX(0, tt), 1);

                dd = pow((1-tt) * xb + tt * xe - xx, 2)
                   + pow((1-tt) * yb + tt * ye - yy, 2)
                   + pow((1-tt) * zb + tt * ze - zz, 2);

                if (dd < dmin) {
                    dmin    = dd;
                    beta[k] = ((j-1) + tt) * (double)(n - 3) / (double)(n - 1);
                }
            }
        }
    }

#ifdef DEBUG
    printf("Initialization\n");
    for (j = 0; j < n; j++) {
        printf("%3d: %12.6f %12.6f %12.6f\n", j, cp[3*j], cp[3*j+1], cp[3*j+2]);
    }
    for (k = 0; k < m; k++) {
        printf("%3d: %12.6f\n", k, beta[k]);
    }
#endif

    /* set the relaxation parameter for control points */
    omega = 0.25;

    /* insert the interior control points into the design variables */
    next = m;
    for (j = 1; j < n-1; j++) {
        beta[next++] = cp[3*j  ];
        beta[next++] = cp[3*j+1];
        beta[next++] = cp[3*j+2];
    }

    /* compute the initial objective function */
    for (k = 0; k < m; k++) {
        status = eval1dBspline(beta[k], n, cp, XYZ, NULL, NULL);
        CHECK_STATUS(eval1dBspline);

        f[3*k  ] = XYZcopy[3*k  ] - XYZ[0];
        f[3*k+1] = XYZcopy[3*k+1] - XYZ[1];
        f[3*k+2] = XYZcopy[3*k+2] - XYZ[2];
    }
    *normf = L2norm(f, nobj) / m;
#ifdef DEBUG
    printf("initial   norm(f)=%11.4e\n", *normf);
#endif

    /* initialize the Levenberg-Marquardt algorithm */
    niter  = 501;
    toler  = 1.0e-6;
    lambda = 1;

    /* LM iterations */
    for (iter = 0; iter < niter; iter++) {

        /* initialize [AA  BB]
                      [      ] =  transpose(J) * J + lambda * diag(transpose(J) * J)
                      [BB' CC]

           and        rhs  = -transpose(J) * f
        */
        for (jvar = 0; jvar < np; jvar++) {
            for (ivar = 0; ivar < np; ivar++) {
                CC(ivar,jvar) = 0;
            }
            CC(jvar,jvar) = 1e-6;
        }

        for (jvar = 0; jvar < nvar; jvar++) {
            rhs[jvar] = 0;
        }

        /* accumulate AA, BB, CC, and rhs by looping over points in cloud */
        for (k = 0; k < m; k++) {
            status = eval1dBspline(beta[k], n, cp, XYZ, dXYZdT, dXYZdP);
            CHECK_STATUS(eval1dBspline);

            AA(k) = dXYZdT[0] * dXYZdT[0] + dXYZdT[1] * dXYZdT[1] + dXYZdT[2] * dXYZdT[2];

            for (ivar = 1; ivar < n-1; ivar++) {
                BB(k, 3*ivar-3) = dXYZdT[0] * dXYZdP[ivar];
                BB(k, 3*ivar-2) = dXYZdT[1] * dXYZdP[ivar];
                BB(k, 3*ivar-1) = dXYZdT[2] * dXYZdP[ivar];

                for (jvar = 1; jvar < n-1; jvar++) {
#ifndef __clang_analyzer__
                    CC(3*ivar-3, 3*jvar-3) += dXYZdP[ivar] * dXYZdP[jvar];
                    CC(3*ivar-2, 3*jvar-2) += dXYZdP[ivar] * dXYZdP[jvar];
                    CC(3*ivar-1, 3*jvar-1) += dXYZdP[ivar] * dXYZdP[jvar];
#endif
                }
            }

            rhs[k] = dXYZdT[0] * f[3*k] + dXYZdT[1] * f[3*k+1] + dXYZdT[2] * f[3*k+2];

            for (ivar = 1; ivar < n-1; ivar++) {
#ifndef __clang_analyzer__
                rhs[m+3*ivar-3] += dXYZdP[ivar] * f[3*k  ];
                rhs[m+3*ivar-2] += dXYZdP[ivar] * f[3*k+1];
                rhs[m+3*ivar-1] += dXYZdP[ivar] * f[3*k+2];
#endif
            }
        }

        /* set up sparse-matrix arrays */
        count = m + 2 * m * np + np * np + 1;

        MMd = (double *) malloc(count*sizeof(double));
        MMi = (int    *) malloc(count*sizeof(int   ));

        if (MMd == NULL || MMi == NULL) {
            status = EGADS_MALLOC;
            goto cleanup;
        }

        /* store diagonal values (multiplied by (1+lambda)) */
        for (k = 0; k < m; k++) {
            MMd[k] = AA(k) * (1 + lambda);
        }
        for (ivar = 0; ivar < np; ivar++) {
            MMd[m+ivar] = CC(ivar,ivar) * (1 + lambda);
        }

        /* set up off-diagonal elements, including indices */
        MMi[0] = nvar + 1;
        count  = nvar;

        /* BB to the right of AA */
        for (k = 0; k < m; k++) {
            for (jvar = 0; jvar < np; jvar++) {
                count++;
                MMd[count] = BB(k,jvar);
                MMi[count] = m + jvar;
            }
            MMi[k+1] = count + 1;
        }

        for (ivar = 0; ivar < np; ivar++) {
            /* transpose(BB) below A */
            for (k = 0; k < m; k++) {
                count++;
                MMd[count] = BB(k,ivar);
                MMi[count] = k;
            }

            /* CC in bottom-right corner */
            for (jvar = 0; jvar < np; jvar++) {
                if (ivar != jvar) {
                    count++;
                    MMd[count] = CC(ivar,jvar);
                    MMi[count] = m + jvar;
                }
            }
            MMi[m+ivar+1] = count + 1;
        }

        /* arbitrary value (not used) */
        MMd[nvar] = 0;

        /* set up for sparse matrix solve (via biconjugate gradient technique) */
        itol    = 1;
        errmax  = 1.0e-12;
        maxiter = 2 * nvar;
        for (ivar = 0; ivar < nvar; ivar++) {
            delta[ivar] = 0;
        }

        status = solveSparse(MMd, MMi, rhs, delta, itol, &errmax, &maxiter);
        CHECK_STATUS(solveSparse);

        FREE(MMd);
        FREE(MMi);

        /* check for convergence on delta (which corresponds to a small
           change in beta) */
        normdelta = L2norm(delta, nvar);

        if (normdelta < toler) {
#ifdef DEBUG
            printf("converged with norm(delta)=%11.4e\n", normdelta);
#endif
            break;
        }

        /* find the temporary new beta */
        for (ivar = 0; ivar < nvar; ivar++) {

            /* beta associated with Tcloud */
            if (ivar < m) {
                betanew[ivar] = beta[ivar] + delta[ivar];

                if (betanew[ivar] < 0  ) betanew[ivar] = 0;
                if (betanew[ivar] > n-3) betanew[ivar] = n-3;

                /* beta associated with control points */
            } else {
                betanew[ivar] = beta[ivar] + omega * delta[ivar];
            }
        }

        /* gradually increase omega */
        omega = MIN(1.01*omega, 1.0);

        /* extract the temporary control points from betanew */
        next = m;
        for (j = 0; j < n; j++) {
            if (j == 0 || j == n-1) {
                cpnew[3*j  ] = cp[3*j  ];
                cpnew[3*j+1] = cp[3*j+1];
                cpnew[3*j+2] = cp[3*j+2];
            } else {
                cpnew[3*j  ] = betanew[next++];
                cpnew[3*j+1] = betanew[next++];
                cpnew[3*j+2] = betanew[next++];
            }
        }

        /* apply periodicity condition by making sure first and last
           intervals are the same */
        if (periodic == 1) {
            delta0 = (2*cpnew[0] - cpnew[3] - cpnew[3*n-6]) / 2;
            delta1 = (2*cpnew[1] - cpnew[4] - cpnew[3*n-5]) / 2;
            delta2 = (2*cpnew[2] - cpnew[5] - cpnew[3*n-4]) / 2;

            cpnew[    3] += delta0;
            cpnew[    4] += delta1;
            cpnew[    5] += delta2;

            cpnew[3*n-6] += delta0;
            cpnew[3*n-5] += delta1;
            cpnew[3*n-4] += delta2;
        }

        /* compute the objective function based upon the new beta */
        for (k = 0; k < m; k++) {
            status = eval1dBspline(betanew[k], n, cp, XYZ, NULL, NULL);
            CHECK_STATUS(eval1dBspline);

            fnew[3*k  ] = XYZcopy[3*k  ] - XYZ[0];
            fnew[3*k+1] = XYZcopy[3*k+1] - XYZ[1];
            fnew[3*k+2] = XYZcopy[3*k+2] - XYZ[2];
        }
        normfnew = L2norm(fnew, nobj) / m;
#ifdef DEBUG
        if (iter%10 == 0) {
            printf("iter=%4d: norm(delta)=%11.4e, norm(f)=%11.4e  ",
                   iter, normdelta, normfnew);
        }
#endif

        /* if this was a better step, accept it and decrease
           lambda (making it more Newton-like) */
        if (normfnew < *normf) {
            lambda /= 2;
#ifdef DEBUG
            if (iter%10 == 0) {
                printf("ACCEPTED,  lambda=%11.4e, omega=%10.5f\n", lambda, omega);
            }
#endif

            /* save new design variables, control points, and
               objective function */
            for (ivar = 0; ivar < nvar; ivar++) {
                beta[ivar] = betanew[ivar];
            }
            for (j = 0; j < n; j++) {
                cp[3*j  ] = cpnew[3*j  ];
                cp[3*j+1] = cpnew[3*j+1];
                cp[3*j+2] = cpnew[3*j+2];
            }
            for (iobj = 0; iobj < nobj; iobj++) {
                f[iobj] = fnew[iobj];
            }
            *normf = normfnew;

        /* otherwise do not take the step and increase lambda (making it
           more steepest-descent-like) */
        } else {
            lambda *= 2;
#ifdef DEBUG
            if (iter %10 == 0) {
                printf("rejected,  lambda=%11.4e, omega=%10.5f\n", lambda, omega);
            }
#endif
        }

        /* check for convergence (based upon a small value of
           objective function) */
        if (*normf < toler) {
#ifdef DEBUG
            printf("converged with norm(f)=%11.4e\n", *normf);
#endif
            break;
        }
    }

    /* transform control points back to their original scale */
    for (j = 0; j < n; j++) {
        cp[3*j  ] = xcent + cp[3*j  ] / scale;
        cp[3*j+1] = ycent + cp[3*j+1] / scale;
        cp[3*j+2] = zcent + cp[3*j+2] / scale;
    }

    *normf /= scale;

#ifdef DEBUG
    printf("final control points\n");
    for (j = 0; j < n; j++) {
        printf("%3d: %12.6f %12.6f %12.6f\n", j, cp[3*j], cp[3*j+1], cp[3*j+2]);
    }
    printf("*normf: %12.4e\n", *normf);
#endif

cleanup:
    if (XYZcopy != NULL) free(XYZcopy);
    if (dXYZdP  != NULL) free(dXYZdP );
    if (cpnew   != NULL) free(cpnew  );

    if (beta    != NULL) free(beta   );
    if (delta   != NULL) free(delta  );
    if (betanew != NULL) free(betanew);

    if (f       != NULL) free(f      );
    if (fnew    != NULL) free(fnew   );

    if (aa      != NULL) free(aa     );
    if (bb      != NULL) free(bb     );
    if (cc      != NULL) free(cc     );
    if (rhs     != NULL) free(rhs    );

    if (MMi     != NULL) free(MMi    );
    if (MMd     != NULL) free(MMd    );

#undef AA
#undef BB
#undef CC

    return status;
}


/*
 ************************************************************************
 *                                                                      *
 *   eval1dBspline - evaluate cubic Bspline and its derivatives         *
 *                                                                      *
 ************************************************************************
 */

static int
eval1dBspline(double T,                 /* (in)  independent variable */
              int    n,                 /* (in)  number of control points */
              double cp[],              /* (in)  array  of control points */
              double XYZ[],             /* (out) dependent variables */
    /*@null@*/double dXYZdT[],          /* (out) derivative wrt T (or NULL) */
    /*@null@*/double dXYZdP[])          /* (out) derivative wrt P (or NULL) */
{
    int    status = EGADS_SUCCESS;      /* (out) return status */

    int    i, span;
    double N[4], dN[4];

    ROUTINE(eval1dBspline);

    /* --------------------------------------------------------------- */

    assert (n > 3);

    XYZ[0] = 0;
    XYZ[1] = 0;
    XYZ[2] = 0;

    /* set up the Bspline bases */
    status = cubicBsplineBases(n, T, N, dN);
    CHECK_STATUS(cubicBsplineBases);

    span = MIN(floor(T), n-4);

    /* find the dependent variable */
    for (i = 0; i < 4; i++) {
        XYZ[0] += N[i] * cp[3*(i+span)  ];
        XYZ[1] += N[i] * cp[3*(i+span)+1];
        XYZ[2] += N[i] * cp[3*(i+span)+2];
    }

    /* find the deriviative wrt T */
    if (dXYZdT != NULL) {
        dXYZdT[0] = 0;
        dXYZdT[1] = 0;
        dXYZdT[2] = 0;

        for (i = 0; i < 4; i++) {
            dXYZdT[0] += dN[i] * cp[3*(i+span)  ];
            dXYZdT[1] += dN[i] * cp[3*(i+span)+1];
            dXYZdT[2] += dN[i] * cp[3*(i+span)+2];
        }
    }

    /* find the derivative wrt P */
    if (dXYZdP != NULL) {
        for (i = 0; i < n; i++) {
            dXYZdP[i] = 0;
        }
        for (i = 0; i < 4; i++) {
            dXYZdP[i+span] += N[i];
        }
    }

cleanup:
    return status;
}


/*
 ************************************************************************
 *                                                                      *
 *   cubicBsplineBases - basis function values for cubic Bspline        *
 *                                                                      *
 ************************************************************************
 */

static int
cubicBsplineBases(int    ncp,           /* (in)  number of control points */
                  double T,             /* (in)  independent variable (0<=T<=(ncp-3) */
                  double N[],           /* (out) bases */
                  double dN[])          /* (out) d(bases)/d(T) */
{
    int       status = EGADS_SUCCESS;   /* (out) return status */

    int      i, r, span;
    double   saved, dsaved, num, dnum, den, dden, temp, dtemp;
    double   left[4], dleft[4], rite[4], drite[4];

    ROUTINE(cubicBsplineBases);

    /* --------------------------------------------------------------- */

    span = MIN(floor(T)+3, ncp-1);

    N[ 0] = 1.0;
    dN[0] = 0;

    for (i = 1; i <= 3; i++) {
        left[ i] = T - MAX(0, span-2-i);
        dleft[i] = 1;

        rite[ i] = MIN(ncp-3,span-3+i) - T;
        drite[i] =                     - 1;

        saved  = 0;
        dsaved = 0;

        for (r = 0; r < i; r++) {
            num   = N[ r];
            dnum  = dN[r];

            den   = rite[ r+1] + left[ i-r];
            dden  = drite[r+1] + dleft[i-r];

            temp  = num / den;
            dtemp = (dnum * den - dden * num) / den / den;

            N[ r] = saved  + rite[ r+1] * temp;
            dN[r] = dsaved + drite[r+1] * temp + rite[r+1] * dtemp;

            saved  = left[ i-r] * temp;
            dsaved = dleft[i-r] * temp + left[i-r] * dtemp;
        }

        N[ i] = saved;
        dN[i] = dsaved;
    }

//cleanup:
    return status;
}


/*
 ************************************************************************
 *                                                                      *
 *   solveSparse - solve: A * x = b  using biconjugate gradient method  *
 *                                                                      *
 ************************************************************************
 */

static int
solveSparse(double SAv[],               /* (in)  sparse array values */
            int    SAi[],               /* (in)  sparse array indices */
            double b[],                 /* (in)  rhs vector */
            double x[],                 /* (in)  guessed result vector */
                                        /* (out) result vector */
            int    itol,                /* (in)  stopping criterion */
            double *errmax,             /* (in)  convergence tolerance */
                                        /* (out) estimated error at convergence */
            int    *iter)               /* (in)  maximum number of iterations */
                                        /* (out) number of iterations taken */
{
    int    status = EGADS_SUCCESS;      /* (out) return status */

    int    n, i, j, k, itmax;
    double tol, ak, akden, bk, bknum, bkden=1, bnorm, dxnorm, xnorm, znorm_old, znorm=0;
    double *p=NULL, *pp=NULL, *r=NULL, *rr=NULL, *z=NULL, *zz=NULL;

    ROUTINE(solveSparse);

    /* --------------------------------------------------------------- */

    tol   = *errmax;
    itmax = *iter;

    n = SAi[0] - 1;

    p  = (double *) malloc(n*sizeof(double));
    pp = (double *) malloc(n*sizeof(double));
    r  = (double *) malloc(n*sizeof(double));
    rr = (double *) malloc(n*sizeof(double));
    z  = (double *) malloc(n*sizeof(double));
    zz = (double *) malloc(n*sizeof(double));

    if (p == NULL || pp == NULL ||
        r == NULL || rr == NULL ||
        z == NULL || zz == NULL   ) {
        status = EGADS_MALLOC;
        goto cleanup;
    }

    /* make sure none of the diagonals are very small */
    for (i = 0; i < n; i++) {
        if (fabs(SAv[i]) < 1.0e-14) {
            printf(" solveSparse: cannot solve since SAv[%d]=%11.4e\n", i, SAv[i]);
            status = EGADS_DEGEN;
            goto cleanup;
        }
    }

    /* calculate initial residual */
    *iter = 0;

    /* r = A * x  */
    for (i = 0; i < n; i++) {
        r[i] = SAv[i] * x[i];

        for (k = SAi[i]; k < SAi[i+1]; k++) {
            r[i] += SAv[k] * x[SAi[k]];
        }
    }

    for (j = 0; j < n; j++) {
        r[ j] = b[j] - r[j];
        rr[j] = r[j];
    }

    if (itol == 1) {
        bnorm = L2norm(b, n);

        for (j = 0; j < n; j++) {
            z[j] = r[j] / SAv[j];
        }
    } else if (itol == 2) {
        for (j = 0; j < n; j++) {
            z[j] = b[j] / SAv[j];
        }

        bnorm = L2norm(z, n);

        for (j = 0; j < n; j++) {
            z[j] = r[j] / SAv[j];
        }
    } else {
        for (j = 0; j < n; j++) {
            z[j] = b[j] / SAv[j];
        }

        bnorm = L2norm(z, n);

        for (j = 0; j < n; j++) {
            z[j] = r[j] / SAv[j];
        }

        znorm = L2norm(z, n);
    }

    /* main iteration loop */
    for (*iter = 0; *iter < itmax; (*iter)++) {

        for (j = 0; j < n; j++) {
            zz[j] = rr[j] / SAv[j];
        }

        /* calculate coefficient bk and direction vectors p and pp */
        bknum = 0;
        for (j = 0; j < n; j++) {
            bknum += z[j] * rr[j];
        }

        if (*iter == 0) {
            for (j = 0; j < n; j++) {
                p[ j] = z[ j];
                pp[j] = zz[j];
            }
        } else {
            bk = bknum / bkden;

            for (j = 0; j < n; j++) {
                p[ j] = bk * p[ j] + z[ j];
                pp[j] = bk * pp[j] + zz[j];
            }
        }

        /* calculate coefficient ak, new iterate x, and new residuals r and rr */
        bkden = bknum;

        /* z = A * p  */
        for (i = 0; i < n; i++) {
            z[i] = SAv[i] * p[i];

            for (k = SAi[i]; k < SAi[i+1]; k++) {
                z[i] += SAv[k] * p[SAi[k]];
            }
        }

        akden = 0;
        for (j = 0; j < n; j++) {
            akden += z[j] * pp[j];
        }

        ak = bknum / akden;

        /* zz = transpose(A) * pp  */
        for (i = 0; i < n; i++) {
            zz[i] = SAv[i] * pp[i];
        }

        for (i = 0; i < n; i++) {
            for (k = SAi[i]; k < SAi[i+1]; k++) {
                j = SAi[k];
                zz[j] += SAv[k] * pp[i];
            }
        }

        for (j = 0; j < n; j++) {
            x[ j] += ak * p[ j];
            r[ j] -= ak * z[ j];
            rr[j] -= ak * zz[j];
        }

        /* solve Abar * z = r */
        for (j = 0; j < n; j++) {
            z[j] = r[j] / SAv[j];
        }

        /* compute and check stopping criterion */
        if (itol == 1) {
            *errmax = L2norm(r, n) / bnorm;
        } else if (itol == 2) {
            *errmax = L2norm(z, n) / bnorm;
        } else {
            znorm_old = znorm;
            znorm = L2norm(z, n);
            if (fabs(znorm_old-znorm) > (1.0e-14)*znorm) {
                dxnorm = fabs(ak) * L2norm(p, n);
                *errmax  = znorm / fabs(znorm_old-znorm) * dxnorm;
            } else {
                *errmax = znorm / bnorm;
                continue;
            }

            xnorm = L2norm(x, n);
            if (*errmax <= xnorm/2) {
                *errmax /= xnorm;
            } else {
                *errmax = znorm / bnorm;
                continue;
            }
        }

        /* exit if converged */
        if (*errmax <= tol) break;
    }

cleanup:
    if (p  != NULL) free(p );
    if (pp != NULL) free(pp);
    if (r  != NULL) free(r );
    if (rr != NULL) free(rr);
    if (z  != NULL) free(z );
    if (zz != NULL) free(zz);

    return status;
}


/*
 ************************************************************************
 *                                                                      *
 *   L2norm - L2-norm of vector                                         *
 *                                                                      *
 ************************************************************************
 */

static double
L2norm(double f[],                      /* (in)  vector */
       int    n)                        /* (in)  length of vector */
{
    double L2norm;                      /* (out) L2-norm */

    int    i;

    ROUTINE(L2norm);

    /* --------------------------------------------------------------- */

    /* L2-norm */
    L2norm = 0;

    for (i = 0; i < n; i++) {
        L2norm += f[i] * f[i];
    }

    L2norm = sqrt(L2norm);

//cleanup:
    return L2norm;
}


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
    int    ilin[3], isym[3], nper[3], ncp;
    float  *xplot=NULL, *yplot=NULL, *zplot=NULL;
    double xmin, xmax, ymin, ymax, zmin, zmax, trange[4], frac, tt, data[18];
    double *tempRlist;
    ego    eref;

    /* --------------------------------------------------------------- */

    xplot = (float*) malloc((npnt+2000)*sizeof(float));
    yplot = (float*) malloc((npnt+2000)*sizeof(float));
    zplot = (float*) malloc((npnt+2000)*sizeof(float));

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
    if (status != EGADS_SUCCESS) goto cleanup;

    for (ipnt = 0; ipnt < 1000; ipnt++) {
        frac = (double)(ipnt) / (double)(1000-1);
        tt   = (1-frac) * trange[0] + frac * trange[1];

        status = EG_evaluate(ecurve, &tt, data);
        if (status != EGADS_SUCCESS) goto cleanup;

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
    if (status != EGADS_SUCCESS) goto cleanup;
    if (oclass != CURVE        ) goto cleanup;
    if (mtype  != BSPLINE      ) goto cleanup;

    ncp = tempIlist[2];

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
