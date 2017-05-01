MODULE initialize
	IMPLICIT NONE

	CONTAINS
	! subroutine createMatrix	Erstellt ein Feld mit den gewünschten Dimensionen
	! subroutine initializeMatrix	Belegt das Feld mit den Anfangswerten
	

	! Erstellt ein Feld mit den gewünschten Dimensionen
	subroutine createMatrix(matrix, matrixSize)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
        integer, intent(in):: matrixSize ! Kantenlänge der Matrix

		!Zuweisen der Matrix
		allocate(matrix(matrixSize, matrixSize))
	end subroutine createMatrix
	

	! Belegt das Feld mit den Anfangswerten
	subroutine initializeMatrix(matrix)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
        integer :: i, j ! Schleifenvariablen
        double precision :: stepSize ! Differenz zweier benachbarter Werte am Rand
		stepSize = 1.0 / (ubound(matrix, 1) - lbound(matrix, 1))
		
		!Belegen mit Nullen
		matrix = 0.

		!Belegen der Ränder
		i = 1
		do j = lbound(matrix, 1), ubound(matrix, 1)
			matrix(i,j) = 1 - ((j - 1) * stepSize) ! oben
			matrix(j,i) = 1 - ((j - 1) * stepSize) ! links
		end do
		j = ubound(matrix, 1)
		do i = ubound(matrix, 1), lbound(matrix, 1), -1
			matrix(i,j) = (i - 1) * stepSize ! rechts
			matrix(j,i) = (i - 1) * stepSize ! unten
		end do
		
	end subroutine initializeMatrix

END MODULE initialize
