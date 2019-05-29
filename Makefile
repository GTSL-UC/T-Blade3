.PHONY: all clean redo library clean_lib
.DEFAULT_GOAL: all

# Source code directory
SRC_DIR = $(shell pwd)/source

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
