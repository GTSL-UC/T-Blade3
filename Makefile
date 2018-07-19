.SUFFIXES:
.SUFFIXES: .f .o .f90
#
LIBDIR = lib
UNAME := $(shell uname)

DEFINE = 
FCOMP  = gfortran
FOPTS  = -fdefault-real-8 -g -fbounds-check -fbacktrace -O2 -Wline-truncation -cpp -DSTAND_ALONE
F90OPTS = -ffree-form -ffree-line-length-none -fPIC
#FOPTS = -fPIC -g

OBJS =  globvar.o spline.o readinput.o funcNsubs.o 3dbgb.o bladegen.o b3d2sec.o bladestack.o bspline3.o lesting.o \
        cubicspline.o lespline.o bsplinecam.o splinethick.o gauss_jordan.o airfoiltypes.o bladegrid2D.o ellipgrid.o \
		spanwise_variation.o spanwise_output.o poly_solve_bisect.o quartic_poly_solve.o thk_ctr_gen_driver.o \
		thk_ctrl_gen_der.o thk_ctrl_gen_spl.o \

#MYLIBS = $(HOME)/$(LIBDIR)/dtnurbsPIC_i.a

XLIBS  = -L/usr/X11R6/lib64 -lX11 -lpthread
GLIBS  = -L/usr/X11R6/lib64 -lGLU -lGL -lX11 -lXext -lpthread

#all: $(OBJS)
all: 3dbgb tblade3 techop

ifeq ($(UNAME),Darwin)
  3dbgb:$(OBJS)
	$(FCOMP)  -g $(OBJS) -o 3dbgb 
  tblade3:$(OBJS)
	$(FCOMP)  -g $(OBJS) -o tblade3  
  techop:techop.o
	$(FCOMP) -g techop.o -o techop    
else
  3dbgb:$(OBJS)
	$(FCOMP)  -g $(OBJS) -o 3dbgb 
  tblade3:$(OBJS)
	$(FCOMP)  -g $(OBJS) -o tblade3  
  techop:techop.o
	$(FCOMP) -g techop.o -o techop    
#  3dbgb:$(OBJS)
#	$(FCOMP)  -g -static $(OBJS) -o 3dbgb
#  tblade3:$(OBJS)
#	$(FCOMP)  -g -static $(OBJS) -o tblade3    
#  techop:techop.o
#	$(FCOMP) -g -static techop.o -o techop    
endif

.f.o:; $(FCOMP) -c -o $@ $(FOPTS) $*.f
.f90.o:; $(FCOMP) -c -o $@ $(FOPTS) $(F90OPTS) $*.f90


clean:

	-rm -f 3dbgb tblade3 techop techop.o $(OBJS) *.mod *.x *.exe
