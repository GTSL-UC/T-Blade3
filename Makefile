.PHONY: all clean redo library clean_lib
.DEFAULT_GOAL: all

# Source code directory
SRC_DIR = $(shell pwd)/source

# OS detection
ifeq ($(OS),Windows_NT)
	detected_OS := Windows
else
	detected_OS := $(shell uname -s)
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

# Required libraries
XLIBS = -L/usr/X11R6/lib64 -lX11 -lpthread
GLIBS = -L/usr/X11R6/lib64 -lGLU -lGL -lX11 -lXext -lpthread

# Build main T-Blade3 binaries
all:
	make -C $(SRC_DIR) all

# Build shared library (Linux/Darwin only)
library:
	make -C $(SRC_DIR) library

# Clean all object files, module files and binaries
clean:
	make -C $(SRC_DIR) clean

# Clean shared library files
clean_lib:
	make -C $(SRC_DIR) clean_lib

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
