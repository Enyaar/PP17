MODULE run
	IMPLICIT NONE
	CONTAINS

	subroutine calculate(matrix,checkExit)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		double precision, allocatable, dimension(:,:) :: temp !temporäre Kopie
		double precision :: star, corr, h !Hilfsvariable
		integer :: diff = 0, counter = 0 !Zähler in Schleifen
		logical, intent(out) :: checkExit
		integer :: i, j !Schleifenvariablen

		allocate(temp(ubound(matrix, 1), ubound(matrix, 2)))
		temp = matrix ! so müssen die Ränder nicht erneut belegt werden
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
				!Übertargen auf temporaere Matrix
				temp(i,j) = matrix(i,j) + corr 
			end do
		end do

		!Wenn für jeden Wert in der Matrix die Abbruchbedingung erfüllt ist:
		if (diff == counter) then
			checkExit = .True.  !beendet die Schleife in poission.f90
		endif

		! Übertrag auf aktuelle Matrix auch wenn die Abbruchbedingung erfüllt ist,
		! da die "zu genaue" Matrix sowieso berechnet wurde 
		matrix = temp 

		deallocate(temp)
	end subroutine

END MODULE run
