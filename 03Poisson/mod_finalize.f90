MODULE finalize
	IMPLICIT NONE
	CONTAINS
	
	! subroutine outputMatrix	Reduktion und Ausgabe auf Konsole
	! subroutine downScaleMatrix	Kocht Matrix auf gewünschte Kantenlänge (width) ein. 
	! subroutine printMatrix	Ausdruck auf Konsole
	! subroutine freeMatrix 	Freigabe des Speichers
	

	! Kocht Matrix auf gewünschte Kantenlänge (width) ein.
	subroutine downScaleMatrix(matrix, output, width)
		double precision, allocatable, dimension(:,:), intent(in) :: matrix 
		double precision, allocatable, dimension(:,:), intent(out) :: output 
		integer, intent(in) :: width !Breite der ausgegebenen Matrix
		integer :: interlines ! Abstand zwischen ausgegebenen Zeilen
		integer :: i, j !Schleifenvariablen
		!
		allocate(output(width, width))
		interlines = (ubound(matrix,1) - lbound(matrix,1)) / (width -1)

		do i = 1, width
			do j = 1, width
				output(i,j) = matrix(1 + ((i -1) * interlines), &
								   & 1 + (j -1) * interlines)
			end do
		end do
		
	end subroutine downScaleMatrix
	
	! Ausdruck auf Konsole
	subroutine printMatrix(matrix)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		integer :: i, j !Schleifenvariablen
		
		do i = lbound(matrix, 1), ubound(matrix, 1)
			do j = lbound(matrix, 2), ubound(matrix, 2)
				! Typumwandlung zu Real um Nachkommastellen zu schneiden
				write (*,1, advance='no') real(matrix(i,j))
				! f10.8 heisst: 10 Stellen für die ganze Zahl, 8 für Nachkommastellen
				1	format(f12.10)
				write (*, '(A)', advance='no') "  "
			end do
			print *, ""
		end do

	end subroutine printMatrix
	
	
	! Reduktion und Ausgabe auf Konsole
	subroutine outputMatrix(matrix, width)
		double precision, dimension(:,:), allocatable, intent(in) :: matrix !matrix
		double precision, dimension(:,:), allocatable :: output !matrix
		integer, intent(in) :: width !Breite der ausgegebenen Matrix
		
		call downScaleMatrix(matrix, output, width)
		call printMatrix(output)
		
		deallocate(output)
	end subroutine 
	
	! Freigabe des Speichers
	subroutine freeMatrix(matrix)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
        
		deallocate(matrix)
	end subroutine freeMatrix

END MODULE finalize
