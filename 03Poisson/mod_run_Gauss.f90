MODULE run
	IMPLICIT NONE
	CONTAINS
	
	subroutine calculate(matrix,checkExit) !more parameters as needed
		double precision, dimension(:,:), intent(inout) :: matrix
		double precision :: star, corr, h
		logical, intent(out) :: checkExit !Beendet Iteration in poisson.f90, falls = .true. am Ende der Subroutine
		integer :: i,j !Schleifenvariablen

		checkExit = .true.
		h = 1.0 / (ubound(matrix, 1) - lbound(matrix, 1))

		! Rechnet nicht an den Rändern
		do j = lbound(matrix, 2)+1, ubound(matrix, 2)-1
			do i = lbound(matrix, 1)+1, ubound(matrix, 1)-1
				! Stern
				star = (-matrix(i,j+1))-matrix(i-1,j)+4*matrix(i,j)-matrix(i+1,j)-matrix(i,j-1)
				! Korrektur
				corr = -star/4
				if (corr >= 10e-6) then
					checkExit = .false.
				end if
				! Übertrag auf aktuelle Matrix
				matrix(i,j) = matrix(i,j) + corr
			end do
		end do

	end subroutine calculate

END MODULE run
