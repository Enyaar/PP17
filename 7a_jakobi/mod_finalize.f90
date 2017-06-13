MODULE finalize
!	use mpi
	IMPLICIT NONE
	CONTAINS
	
	! subroutine printNumber	Ausgabe einzelnes Double auf Konsole
	! subroutine printRow		Ausgabe einer Zeile auf Konsole
	
	
	! gibt einzelnen Wert auf Konsole aus
	subroutine printNumber(x)
		double precision, intent(in) :: x
		
		! Typumwandlung zu Real um Nachkommastellen zu schneiden
		write (*,1, advance='no') real(x)
		! f10.8 heisst: 10 Stellen für die ganze Zahl, 8 für Nachkommastellen
		1	format(f5.3)
		! Abstand
		write (*, '(A)', advance='no') "  "
	end subroutine printNumber
	
	
	! Prints one Row, only one Number every <interlines> rows
	subroutine printRow(matrix, row, interlines)
		double precision, dimension(:,:), allocatable, intent(in) :: matrix
		integer, intent(in) :: row ! Row to be printed
		integer, intent(in) :: interlines ! Size of gap between printed numbers
		integer :: i ! Schleifenvariable

		
		call printNumber(matrix(row,1))
		do i = 2, ubound(matrix, 2)
			if (mod(i, interlines) == 0) then
				call printNumber(matrix(row,i+1))
			endif
		end do
		print *, ""
	end subroutine printRow

END MODULE finalize
