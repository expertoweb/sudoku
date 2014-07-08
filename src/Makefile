#  _________
# |   FILE  |_______________________________________
# |                 _________                       |
# | Makefile       |  AUTHOR |_______________________________________
# |   _________    |                                                 |
# |  | RESUME  |___| Rubén Muñoz <rmunoz@cbm.uam.es> CBM-SO (CSIC)   |
# |__|             |_________________________________________________|
#    | 19-07-2006:                                              |            
#    |                                                          | 
#    |   Type make to compile all examples.                     | 
#    |__________________________________________________________|

INCLUDEPATH=/home/rmunoz/software/mpich2-1.0.3/src/include/
LIBRARYPATH=/home/rmunoz/software/mpich2-1.0.3/lib
FC=xlf

all: hellompi sdkpar sdkser testpar master
hellompi: hellompi.f
	mpif77 -o hellompi -I$(INCLUDEPATH) -L$(LIBRARYPATH) hellompi.f

sdkpar: sdkpar.f90
	mpif90 -o sdkpar -I$(INCLUDEPATH) -L$(LIBRARYPATH) sdkpar.f90

master: master.f90
	$(FC) -o master master.f90

testpar: testpar.f90
	mpif90 -o testpar -I$(INCLUDEPATH) -L$(LIBRARYPATH) testpar.f90

sdkser: sdkser.f90
	$(FC) -o sdkser sdkser.f90

clean:
	rm -f hellompi sdkpar sdkser master testpar *.o *.mod
