CC   = /d/casdev/gcc-mcdc/mingw-w64-i686-gcc/src/build-x86_64-w64-mingw32/gcc/cc1.exe
GCC = gcc
# BASEDIR := $(shell pwd)
LCOVDIR := ../lcov
GCOV    := ../mingw-w64-i686-gcc/src/build-x86_64-w64-mingw32/gcc/gcov.exe
LCOV    := $(LCOVDIR)/bin/lcov
GENHTML := $(LCOVDIR)/bin/genhtml

EXE  = test.exe

all: test.gcda.txt test.gcno.txt COV/index.html


$(EXE): test.o test_test.c
	$(GCC) --coverage -o $(@) $(^)
%.gcno: %.o
%.o: %.c	
	$(CC) -ftest-coverage -fprofile-arcs -da -fdump-tree-original-raw -fdump-tree-all $(^)
test.gcda: $(EXE)
	./$(<)

test.c.gcov: test.gcda
	$(GCOV) -a test.c; \
	cat $(@)

%.txt:	%
	od -tx4 -v -w4 -Anone  $(<) > $(@)

COV/index.html: test.c.gcov
	$(LCOV) --capture --directory . --rc lcov_branch_coverage=1 --output-file test.info
	$(GENHTML)  --branch-coverage -o COV test.info

clean:
	rm -rf $(EXE) *.o *.gc* *.info COV

