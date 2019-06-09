.PHONY: code clean redo library clean_lib esp clean_esp
.DEFAULT_GOAL = code

# Rule to print variables from Makefile
print-% : ; @echo $* = $($*)

# Source code directory and test directory
SRC_DIR = $(shell pwd)/source
TEST_DIR = tests
VPATH = . $(SRC_DIR) $(TEST_DIR)

# OS detection
ifeq ($(OS),Windows_NT)
	detected_OS := Windows
else
	detected_OS := $(shell uname -s)
endif

# Only compile test suite if PFUNIT is found on the system and 
# we are not on Windows
ifdef PFUNIT
ifneq ($(detected_OS),Windows)
TESTING := yes
endif
endif

# Compilation flags
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

# Flags for tests
ifeq ($(TESTING),yes)
FFLAGS = -g -O0 -fbacktrace -fbounds-check -fcheck=mem -I$(PFUNIT)
FPPFLAGS = -DGNU -DBUILD_ROBUST
endif

# Include pFUnit files
ifeq ($(TESTING),yes)
include $(PFUNIT)/include/base.mk
endif

# Executable
ifeq ($(TESTING),yes)
EXE = $(TEST_DIR)/tests.x
endif

# Required libraries
XLIBS = -L/usr/X11R6/lib64 -lX11 -lpthread
GLIBS = -L/usr/X11R6/lib64 -lGLU -lGL -lX11 -lXext -lpthread

# Libraries for tests
ifeq ($(TESTING),yes)
LIBS_TEST = -L$(PFUNIT)/lib -lpfunit
endif

# Object files required for testing
OBJS_SRC = $(SRC_DIR)/globvar.o $(SRC_DIR)/file_operations.o $(SRC_DIR)/errors.o $(SRC_DIR)/spline.o $(SRC_DIR)/funcNsubs.o \
		   $(SRC_DIR)/bladegen.o $(SRC_DIR)/bladestack.o $(SRC_DIR)/bspline3.o $(SRC_DIR)/lesting.o $(SRC_DIR)/cubicspline.o $(SRC_DIR)/lespline.o \
		   $(SRC_DIR)/bsplinecam.o $(SRC_DIR)/splinethick.o $(SRC_DIR)/airfoiltypes.o $(SRC_DIR)/spanwise_variation.o \

# Rule for 'make tests'
ifeq ($(TESTING),yes)
tests: $(EXE)

	./$(EXE) -xml $(TEST_DIR)/tests.xml

TEMP:
	make -C $(TEST_DIR) tests
endif

# Build main T-Blade3 binaries
code:
	make -C $(SRC_DIR) code

# Build shared library (Linux/Darwin only)
library:
	make -C $(SRC_DIR) library

# Build ESP using UDPs
esp:
	make -C $(SRC_DIR) -f MakefileESP all

# Build tests
ifeq ($(TESTING),yes)
$(EXE): testSuites.inc $(OBJS_SRC) TEMP
	$(FCOMP) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -Itests $(PFUNIT)/include/driver.F90 $(TEST_DIR)/*.o $(OBJS_SRC) $(LIBS_TEST) $(FFLAGS) $(FPPFLAGS)
endif 

# Clean all object files, module files and binaries
clean:
	make -C $(SRC_DIR) clean
	make -C $(TEST_DIR) clean
	-rm -f $(EXE) tests/tests.xml


# Clean shared library files
clean_lib:
	make -C $(SRC_DIR) clean_lib

# Clean ESP files
clean_esp:
	make -C $(SRC_DIR) -f MakefileESP cleanall

# Clean all files and compile again
redo:
	make -C $(SRC_DIR) redo

# Export variables to sub-make process
export detected_OS
export DEFINE
export FCOMP
export FOPTS
export F90OPTS
export XLIBS
export GLIBS

# Only export if PFUNIT is found on system
ifdef PFUNIT
export FFLAGS
export FPPFLAGS
export LIBS_TEST
endif
