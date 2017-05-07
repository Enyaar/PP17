#Compiler
#FC="gfortran"
FC="mpif90"
#Number of used Processors
KC=4

main.x: main.f90 
	$(FC) -o main.x main.f90

run: main.x
	mpiexec -n $(KC) ./main.x 

clean:
	rm  main.x *.mod *.o

