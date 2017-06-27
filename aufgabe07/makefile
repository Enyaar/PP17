# Common definitions
CC = f95
MPIC = mpif90

# Number of Processors
NP = 5

# Compiler flags:
FLAGS =  -Wextra -O3
# flags for debugging:
#FLAGS =  -Wextra -Wall -g

main.x: main.f90 mod_initialize.o mod_finalize.o mod_jakobi.o 
	$(MPIC) -o main.x main.f90 mod_initialize.o mod_finalize.o mod_jakobi.o

mod_initialize.o: mod_initialize.f90
	$(CC) -c mod_initialize.f90

mod_finalize.o: mod_finalize.f90
	$(MPIC) -c mod_finalize.f90

mod_jakobi.o: mod_jakobi.f90 
	$(MPIC) -c mod_jakobi.f90 

run: main.x
	mpiexec -n $(NP) ./main.x

clean:
	rm *.mod *.o main.x
