#.PHONY: clean tests

# Include pFUnit files
include $(PFUNIT)/include/base.mk

# Source code directory
SRC_DIR = $(shell pwd)/../source

# .pf files are treated as source files
SRCS = $(wildcard *.pf)
OBJS = $(SRCS:.pf=.o)

# Rule to make tests
tests: $(OBJS)
testSuites.inc: $(SRCS)

# Append to FFLAGS 
FFLAGS += -I$(SRC_DIR) -I$(PFUNIT)/mod

# Invoke parser script to generate .F90 files
%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@

%.o: %.F90
	$(FCOMP) -c $(FFLAGS) $(FPPFLAGS) $<

clean: local-E1-clean

local-E1-clean:
	$(RM) *$(OBJ_EXT) *.mod
