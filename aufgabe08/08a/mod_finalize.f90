MODULE finalize
	use mpi
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

		
		call printNumber(matrix(1,row))
		do i = 2, ubound(matrix, 1)
			if (mod(i-1, interlines) == 0) then
				call printNumber(matrix(i,row))
			endif
		end do
		print *, ""
	end subroutine printRow

	subroutine printMatrix(matrix, interlines, offset, lineCount, mpi_rank, mpi_master, mpi_last, mpi_ierr)
		integer, intent(in) :: mpi_rank, mpi_master, mpi_last, mpi_ierr ! mpi integer
		integer :: status(MPI_STATUS_SIZE)
		double precision, dimension(:,:), allocatable, intent(in) :: matrix
		integer, intent(in) :: interlines, offset, lineCount ! Abstand zwischen Zeilen, die ausgegeben werden sollen und offset
		integer :: currentRow = 1 ! Laufender Zähler für Ausgabe über alle Prozesse


		if (mpi_rank == mpi_master) then
			! Prolog:
			print *, ""
			print *, "Fertige Matrix"
			! Dann alles andere (eventuell)
			do while (currentRow < (offset + lineCount))
				call printRow(matrix, currentRow, interlines)
				currentRow = currentRow + interlines
			end do
			!print*, matrix
			call mpi_send(currentRow, 1, mpi_integer, 1, 2017, mpi_comm_world, mpi_ierr)
		else 
			call mpi_recv(currentRow, 1, mpi_integer, (mpi_rank - 1), 2017, mpi_comm_world, status, mpi_ierr)
		
			do while (currentRow <= (offset + lineCount))
				call printRow(matrix, (currentRow - offset + 1), interlines)
				currentRow = currentRow + interlines
			end do
			!print*, matrix
			if (mpi_rank .ne. mpi_last) then
				call mpi_send(currentRow, 1, mpi_integer, (mpi_rank + 1), 2017, mpi_comm_world, mpi_ierr)
			endif
		endif
	end subroutine printMatrix

END MODULE finalize
