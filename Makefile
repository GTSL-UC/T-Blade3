.PHONY: all clean redo
.SUFFIXES:
.SUFFIXES: .f .o .f90
#
#LIBDIR = lib
#UNAME := $(shell uname)
ifeq ($(OS),Windows_NT)
    detected_OS := Windows
else
    detected_OS := $(shell uname -s)
endif

DEFINE = 
FCOMP  = gfortran
FOPTS  = -fdefault-real-8 -g -fbounds-check -fbacktrace -O2 -Wline-truncation -Wall -Wno-unused-dummy-argument -cpp -DSTAND_ALONE
F90OPTS = -ffree-form -ffree-line-length-none -fPIC
ifdef MEMCHECK
FOPTS += -fsanitize=address -fno-omit-frame-pointer
endif
ifdef UNDEFINED
FOPTS += -fsanitize=undefined -fno-omit-frame-pointer
endif
LDFLAGS = -shared
EXEC1 = bin/3dbgb
EXEC2 = bin/tblade3
EXEC3 = bin/techop
TARGET_LIB = lib/tblade3.so

OBJS =  globvar.o file_operations.o errors.o spline.o readinput.o funcNsubs.o 3dbgb.o bladegen.o bladestack.o bspline3.o lesting.o \
        cubicspline.o lespline.o bsplinecam.o splinethick.o airfoiltypes.o spanwise_variation.o \
#OBJS =  globvar.o file_operations.o spline.o readinput.o funcNsubs.o 3dbgb.o bladegen.o b3d2sec.o bladestack.o bspline3.o lesting.o \
#        cubicspline.o lespline.o bsplinecam.o splinethick.o gauss_jordan.o airfoiltypes.o bladegrid2D.o ellipgrid.o \
#		spanwise_variation.o poly_solve_bisect.o quartic_poly_solve.o thk_ctr_gen_driver.o \
#		thk_ctrl_gen_der.o thk_ctrl_gen_spl.o \
#MYLIBS = $(HOME)/$(LIBDIR)/dtnurbsPIC_i.a

XLIBS  = -L/usr/X11R6/lib64 -lX11 -lpthread
GLIBS  = -L/usr/X11R6/lib64 -lGLU -lGL -lX11 -lXext -lpthread

#all: $(OBJS)
all: $(EXEC1) $(EXEC2) $(EXEC3) $(TARGET_LIB)

print-% : ; @echo $* = $($*)

ifeq ($(detected_OS),Windows)
  $(EXEC1):$(OBJS)
	$(FCOMP)  -g -static $(OBJS) -o $(EXEC1)
  $(EXEC2):$(OBJS)
	$(FCOMP)  -g -static $(OBJS) -o $(EXEC2)
  $(EXEC3):globvar.o file_operations.o errors.o funcNsubs.o spline.o techop.o
	$(FCOMP) -g -static globvar.o file_operations.o errors.o funcNsubs.o spline.o techop.o -o $(EXEC3)
else ifeq($(detected_OS),Linux)
  $(EXEC1):$(OBJS)
	$(FCOMP)  -g $(OBJS) -o $(EXEC1) 
  $(EXEC2):$(OBJS)
	$(FCOMP)  -g $(OBJS) -o $(EXEC2)
  $(EXEC3):globvar.o file_operations.o errors.o funcNsubs.o spline.o techop.o
	$(FCOMP) -g globvar.o file_operations.o errors.o funcNsubs.o spline.o techop.o -o $(EXEC3)
  $(TARGET_LIB):$(OBJS)
	$(FCOMP)  -shared -fPIC -o $@ $^
else
  $(EXEC1):$(OBJS)
	$(FCOMP)  -g $(OBJS) -o $(EXEC1) 
  $(EXEC2):$(OBJS)
	$(FCOMP)  -g $(OBJS) -o $(EXEC2)
  $(EXEC3):globvar.o file_operations.o errors.o funcNsubs.o spline.o techop.o
	$(FCOMP) -g globvar.o file_operations.o errors.o funcNsubs.o spline.o techop.o -o $(EXEC3)
endif

.f.o:; $(FCOMP) -c -o $@ $(FOPTS) $*.f
.f90.o:; $(FCOMP) -c -o $@ $(FOPTS) $(F90OPTS) $*.f90


clean:

	-rm -f $(EXEC1) $(EXEC2) $(EXEC3) techop.o $(TARGET_LIB) $(OBJS) *.mod *.x *.exe
    ifeq ($(detected_OS),Darwin)
	    -rm -r *.dSYM
    endif

redo:

	make clean
	clear
	make all
