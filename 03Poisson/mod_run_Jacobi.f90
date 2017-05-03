MODULE run
	IMPLICIT NONE
	CONTAINS

	subroutine calculate(matrix,checkExit)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		double precision, allocatable, dimension(:,:) :: temp !temporäre Kopie
		double precision :: star, corr, h !Hilfsvariable
		logical, intent(out) :: checkExit !Beendet Iteration in poisson.f90, falls = .true. am Ende der Subroutine
		integer :: i, j !Schleifenvariablen

		allocate(temp(ubound(matrix, 1), ubound(matrix, 2)))

		checkExit = .true.
		temp = matrix ! so müssen die Ränder nicht erneut belegt werden
		h = 1.0 / (ubound(matrix, 1) - lbound(matrix, 1))

		! Rechnet nicht an den Rändern
		do j = lbound(matrix, 2)+1, ubound(matrix, 2)-1
			do i = lbound(matrix, 1)+1, ubound(matrix, 1)-1
				! Stern
				star = -matrix(i,j+1)-matrix(i-1,j)+4*matrix(i,j)-matrix(i+1,j)-matrix(i,j-1)
				! Korrektur
				corr = -star/4
				if(corr >= 10e-6) then
					checkExit = .false.
				endif
				!Übertargen auf temporaere Matrix
				temp(i,j) = matrix(i,j) + corr 
			end do
		end do

		! Übertrag auf aktuelle Matrix auch wenn die Abbruchbedingung erfüllt ist,
		! da die "zu genaue" Matrix sowieso berechnet wurde 
		matrix = temp 

		deallocate(temp)
	end subroutine

END MODULE run
