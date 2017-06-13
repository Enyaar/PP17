program poisson
	use mpi
	use initialize
	use run
	use finalize
	use comm
	!
	implicit none
	!
	! parameter:
	integer, parameter :: edgeLength = 17	! Kantenlänge der gesamten Matrix
	!Erlaubte outputSize-möglichkeiten (bei edgelength = 184): 3, 5, 9, 24, 47, 93, 185
	integer, parameter :: outputSize = 9	! Kantenlänge der ausgegebenen Matrix
	integer, parameter :: loopSize = 1	! Anzahl der Iterationen
	! mpi:
	integer :: mpi_rank, mpi_size, tag, status(MPI_STATUS_SIZE), mpi_ierr !MPI
	integer :: mpi_master, mpi_last ! Besondere Prozess IDs
	! data:
	double precision, dimension(:,:), allocatable :: matrix ! or whatever fits
	!double precision, dimension(:,:), allocatable :: temp
	! auxiliary
	integer :: lineCount ! die Anzahl der Zeilen in der einzelnen Matrix
	integer :: offset ! = Index der ersten eigenen Zeile in Gesamtmatrix
	logical :: checkExit =.False. ! Abbruchbedingung
	integer :: i, j !Schleifenvariablen
	! für Ausgabe	
	integer :: interlines ! Abstand zwischen Zeilen, die ausgegeben werden sollen
	integer :: currentRow = 1 ! Laufender Zähler für Ausgabe über alle Prozesse
	interlines = edgeLength / (outputSize - 1)

	! INIT
	call MPI_INIT(mpi_ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,mpi_ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,mpi_rank,mpi_ierr)

	! Fehlerabfrage
	mpi_master = 0
	mpi_last = mpi_size - 1
	if (mpi_rank == mpi_master) then
		if (mod((edgeLength-1),(outputSize-1)) /= 0) then
			write(*,*) "Die Variable outputSize muss so gewählt werden, dass bei der ", & 
			&		  "Rechnung [edgeLength-1 / ( outputSize - 1 )] kein Rest entsteht"
			stop
		endif
	endif

	! Initialisiert Variablen zur Orientierung in Gesamtmatrix
	call initLineCount(lineCount, offset, edgeLength, mpi_size, mpi_rank)
	! Eigene Matrix nach Bedarf
	call allocateMatrix(matrix, mpi_rank, mpi_size, lineCount, edgeLength)
	! Startbelegung der Matrix
	call initializeMatrix(matrix, mpi_rank, mpi_size, lineCount, offset)
	
	call MPI_BARRIER(MPI_COMM_WORLD,mpi_ierr)
	
	!calculating:
    !do i=1,loopSize
      !  call calculate(matrix,temp)
      !  call writeTempEdges(matrix,temp,mpi_rank,mpi_master,mpi_last)
      !  call sendrec(temp,mpi_rank, mpi_master,mpi_last,status(MPI_STATUS_SIZE))
     !   call writeTempOnMatrix(matrix,temp)
    !enddo
	!calculating:
	do i=1,loopSize
		call calculate(matrix, mpi_rank, mpi_master, mpi_last, mpi_ierr)
	end do
	
	call MPI_BARRIER(MPI_COMM_WORLD,mpi_ierr)
	! PROLOG
	interlines = (edgeLength-1) / (outputSize - 1)
	currentRow = 1
	if (mpi_rank == mpi_master) then
		! Prolog:
		print *, ""
		print *, "Fertige Matrix"
		! Dann alles andere (eventuell)
		do while (currentRow < (offset + lineCount))
			call printRow(matrix, currentRow, interlines)
			currentRow = currentRow + interlines
		end do
		call mpi_send(currentRow, 1, mpi_integer, 1, 2017, mpi_comm_world, mpi_ierr)
	else 
		call mpi_recv(currentRow, 1, mpi_integer, (mpi_rank - 1), 2017, mpi_comm_world, status, mpi_ierr)
		
		do while (currentRow <= (offset + lineCount))
!			print *, mpi_rank, "printing:", currentRow, "d.h.", currentRow - offset + 1, "of", ubound(matrix, 1)
			call printRow(matrix, (currentRow - offset + 1), interlines)
			currentRow = currentRow + interlines
		end do
		if (mpi_rank .ne. mpi_last) then
			call mpi_send(currentRow, 1, mpi_integer, (mpi_rank + 1), 2017, mpi_comm_world, mpi_ierr)
		endif
	endif

	! FINAL
	deallocate(matrix)
	call MPI_FINALIZE(mpi_ierr)

end program poisson
