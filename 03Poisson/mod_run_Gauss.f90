MODULE run
	IMPLICIT NONE
	CONTAINS
	
	subroutine calculate(matrix,checkExit) !more parameters as needed
		double precision, dimension(:,:), intent(inout) :: matrix

		double precision :: star, corr, h
		integer :: diff = 0, counter = 0
		logical, intent(out) :: checkExit
		integer :: i,j !Schleifenvariablen



		h = 1.0 / (ubound(matrix, 1) - lbound(matrix, 1))
		diff = 0
		counter = 0
		! Rechnet nicht an den Rändern
		do j = lbound(matrix, 2)+1, ubound(matrix, 2)-1
			do i = lbound(matrix, 1)+1, ubound(matrix, 1)-1
				! Stern
				star = -matrix(i,j+1)-matrix(i-1,j)+4*matrix(i,j)-matrix(i+1,j)-matrix(i,j-1)
				! Korrektur
				corr = matrix(i,j) * h * h - star/4
				if(corr <= 10e-6) then
					diff = diff +1
				endif
				counter = counter +1
				! Übertrag auf aktuelle Matrix
				matrix(i,j) = matrix(i,j) + corr
			end do
		end do

		!Wenn für jeden Wert in der Matrix die Abbruchbedingung erfüllt ist:
		if (diff == counter) then
			checkExit = .True. !beendet die Schleife in poission.f90
		endif






	end subroutine calculate

END MODULE run
