
main.x: main.f90 mod_initializeField.o utilities.o mod_lifecycle.o
	f95 -o main.x main.f90 mod_initializeField.o utilities.o mod_lifecycle.o


mod_initializeField.o: mod_initializeField.f90 utilities.o
	f95 -c mod_initializeField.f90

utilities.o: utilities.f90
	f95 -c utilities.f90

mod_lifecycle.o: mod_lifecycle.f90
	f95 -c mod_lifecycle.f90

run:main.x
	./main.x

clean: 
	rm *.mod *.o main.x
