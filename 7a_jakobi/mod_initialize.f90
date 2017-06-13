MODULE initialize
	IMPLICIT NONE

	CONTAINS
	! subroutine allocateMatrix	Alloziiert die Teilmatrix
	! subroutine initLineCount	Initialisiert Orientierung der Teilmatrix
	! subroutine initializeMatrix	Initalbelegung der Matrix 


	! Initialisiert lineCounts = Anzahl Zeilen die der Matrix zugeordnet sind
	! und offset = Versatz der Matrix gegen Gesamtmatrix
	subroutine initLineCount(lineCount, offset, matrixSize, numproc, rank)
	! check
		integer, intent(inout) :: linecount ! Anzahl der Zeilen, die der jeweiligen Matrix zugeordnet sind
		integer, intent(inout) :: offset ! Versatz der Matrix gegen Gesamtmatrix
		integer, intent(in) :: matrixSize ! Kantenlänge der Matrix
		integer, intent(in) :: numproc ! Anzahl der beteiligten Prozesse
		integer, intent(in) :: rank ! Rang des Prozessses
		integer, dimension(:), allocatable :: allLineCounts
		integer :: linesPerChunk, linesLastChunk
		integer :: i

		allocate(allLineCounts(numproc))
		
		! lineCounts
		linesPerChunk = matrixSize/numproc

		linesLastChunk = matrixSize - (linesPerChunk * (numproc - 1))

		do i=1,numproc-1
			allLineCounts(i) = linesPerChunk
		enddo
		allLineCounts(numproc) = linesLastChunk
		lineCount = allLineCounts(rank + 1)
		
		! Offset
		offset = 0
		do i = 1, rank
			offset = offset + allLineCounts(i)
		end do
		
		deallocate(allLineCounts)
		
	end subroutine initLineCount
	
	! Alloziiert Teilmatrix + Halolines 
	subroutine allocateMatrix(matrix, rank, numproc, lineCount, matrixSize)
		double precision, dimension(:,:), allocatable, intent(inout) :: matrix		
		integer, intent(in) :: matrixSize ! Kantenlńge der Matrix
		integer, intent(in) :: rank ! Rang des Prozessses
		integer, intent(in) :: numproc ! Anzahl der Prozesse
		integer, intent(in) :: linecount ! Anzahl der Zeilen, die der jeweiligen Matrix zugeordnet sind
		integer :: master = 0, last
		last = numproc - 1
		
		! master und last je eine Haloline
		if ((rank ==  master) .or. (rank == last)) then
			allocate(matrix((lineCount +1), matrixSize))
		else
		! Der Rest zwei.
			allocate(matrix((lineCount +2), matrixSize))
		endif
	end subroutine allocateMatrix
		
		
	! Belegt Ränder mit Startwerten
	subroutine initializeMatrix(matrix, rank, numproc, lineCount, offset)
		double precision, dimension(:,:), allocatable, intent(inout) :: matrix
		double precision, dimension(:), allocatable :: stepLine ! Vektor zur initialisierung der Steps		
		integer, intent(in) :: rank ! Rang des Prozessses
		integer, intent(in) :: numproc ! Anzahl der Prozesse
		integer, intent(in) :: lineCount ! Anzahl der Zeilen, die der jeweiligen Matrix zugeordnet sind
		integer, intent(in) :: offset ! Versatz der Matrix gegener Gesamtmatrix
		double precision :: smallestStep ! Schrittgrösse für initiale Belegung an den Rändern
		integer :: master = 0, last ! Besondere Prozess IDs
		integer :: i, j, start, ende ! Schleifenvariable, Start und Endlinie der einzelene Matrizen bezogen auf Gesamtmatrix
		
		allocate(stepLine(ubound(matrix, 2)))
		matrix = 0.	

		smallestStep = 1.0/(ubound(matrix, 2) - 1) !Schrittweite an den Rändern

		
		do i=1, ubound(matrix,2)-1
			stepLine(i) = REAL(ubound(matrix,2)-i)/REAL(ubound(matrix,2)-1)
		end do

		!print*, offset
		
		! start und ende der belegung:
		start = offset
		ende = offset + lineCount + 1
		if (rank == master) then
			start = start + 1
		else if (rank == (numproc - 1)) then
			ende = ende -1
		end if
	
		! oberer Rand
		if (rank == master) then
			matrix(lbound(matrix,1), :) = stepLine(:)
		endif
		! links belegen:
		matrix(lbound(matrix,1):ubound(matrix,1), lbound(matrix,2)) = stepLine(start:ende)
		
		! Belegungsvektor umdrehen
		stepLine = 1. - stepLine
		
		! rechts belegen:
		matrix(lbound(matrix,1):ubound(matrix,1), ubound(matrix,2)) = stepLine(start:ende)
		! unterer Rand
		if (rank == (numproc - 1)) then
			matrix(ubound(matrix,1), :) = stepLine(:)
		endif
		
		deallocate(stepLine)
	
	end subroutine initializeMatrix		
	
END MODULE initialize
	
