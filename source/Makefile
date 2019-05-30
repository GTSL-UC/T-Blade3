.PHONY: code clean redo library clean_lib
.SUFFIXES:
.SUFFIXES: .f .o .f90

# Common executable variables
EXEC1 = 3dbgb
EXEC2 = tblade3
EXEC3 = techop

# Linux-only variables
ifeq ($(detected_OS),Linux)
    LDFLAGS = -shared -fPIC -o
    MKDIR_P := mkdir -p
    EXEC_DIR := ../bin
    LIB_DIR := ../lib
    TARGET_LIB = ../lib/libTBlade3.so
endif

# Mac-only variables
ifeq ($(detected_OS),Darwin)
    LDFLAGS = -dynamiclib -fPIC -o
    MKDIR_P := mkdir -p
    EXEC_DIR := ../bin
    LIB_DIR := ../lib
    TARGET_LIB = ../lib/tblade3.dylib
endif

OBJS =  globvar.o file_operations.o errors.o spline.o readinput.o funcNsubs.o 3dbgb.o bladegen.o bladestack.o bspline3.o lesting.o \
        cubicspline.o lespline.o bsplinecam.o splinethick.o airfoiltypes.o spanwise_variation.o \
#OBJS =  globvar.o file_operations.o spline.o readinput.o funcNsubs.o 3dbgb.o bladegen.o b3d2sec.o bladestack.o bspline3.o lesting.o \
#        cubicspline.o lespline.o bsplinecam.o splinethick.o gauss_jordan.o airfoiltypes.o bladegrid2D.o ellipgrid.o \
#		spanwise_variation.o poly_solve_bisect.o quartic_poly_solve.o thk_ctr_gen_driver.o \
#		thk_ctrl_gen_der.o thk_ctrl_gen_spl.o \
#MYLIBS = $(HOME)/$(LIBDIR)/dtnurbsPIC_i.a

# 'Make code' behavior
ifeq ($(detected_OS),Windows)
    code: $(EXEC1) $(EXEC2) $(EXEC3)
else
    code: $(EXEC_DIR) $(EXEC1) $(EXEC2) $(EXEC3) #$(TARGET_LIB)
    library: $(LIB_DIR) $(TARGET_LIB)
endif

# Compilation for Windows
ifeq ($(detected_OS),Windows)
  $(EXEC1):$(OBJS)
	$(FCOMP)  -g -static $(OBJS) -o ..\$(EXEC1)
  $(EXEC2):$(OBJS)
	$(FCOMP)  -g -static $(OBJS) -o ..\$(EXEC2)
  $(EXEC3):globvar.o file_operations.o errors.o funcNsubs.o spline.o techop.o
	$(FCOMP) -g -static globvar.o file_operations.o errors.o funcNsubs.o spline.o techop.o -o ..\$(EXEC3)
else # Compilation for Linux/MacOS
  $(EXEC_DIR):
	$(MKDIR_P) $(EXEC_DIR)
  $(EXEC1):$(OBJS)
	$(FCOMP)  -g $(OBJS) -o $(EXEC_DIR)/$(EXEC1) 
  $(EXEC2):$(OBJS)
	$(FCOMP)  -g $(OBJS) -o $(EXEC_DIR)/$(EXEC2)
  $(EXEC3):globvar.o file_operations.o errors.o funcNsubs.o spline.o techop.o
	$(FCOMP) -g globvar.o file_operations.o errors.o funcNsubs.o spline.o techop.o -o $(EXEC_DIR)/$(EXEC3)
  $(LIB_DIR):
	$(MKDIR_P) $(LIB_DIR)
  $(TARGET_LIB):$(OBJS)
	$(FCOMP)  $(LDFLAGS) $@ $^
endif

.f.o:; $(FCOMP) -c -o $@ $(FOPTS) $*.f
.f90.o:; $(FCOMP) -c -o $@ $(FOPTS) $(F90OPTS) $*.f90

# 'Make clean'
clean:

    ifeq ($(detected_OS),Windows)
		-rm -f $(EXEC1) $(EXEC2) $(EXEC3) techop.o $(OBJS) *.mod *.x *.exe
    else
		-rm -f $(EXEC_DIR)/$(EXEC1) $(EXEC_DIR)/$(EXEC2) $(EXEC_DIR)/$(EXEC3) techop.o $(OBJS) *.mod *.x *.exe
		-rm -r $(EXEC_DIR)
    endif

# 'Make clean_lib'
clean_lib:

    ifneq ($(detected_OS),Windows)
		-rm -f $(LIB_DIR)/$(TARGET_LIB)
		-rm -r $(LIB_DIR)
    endif

# 'Make redo'
redo:

	make clean
    ifneq ($(detected_OS),Windows)
		make clean_lib
		clear
		make code
		make library
    else
        make code
    endif
