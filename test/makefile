FC = gfortran
FFLAGS = 
LIB_DIR = \
-L../src/

LIBS = -ldfftpack

test: test.o
	$(FC) -o test test.o $(LIB_DIR) $(LIBS)

test.o: test.f
	$(FC) -c test.f $(LIB_DIR) $(LIBS)