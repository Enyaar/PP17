module run
	!
	use mpi
	use finalize
	!
	implicit none
	!
	contains

	! Wendet den Jacobi Algorhytmus einmal auf eine Teilmatrix an, kommuniziert mit den Nachbarn
	subroutine calculate(matrix, mpi_rank, mpi_master, mpi_last, mpi_ierr, loopSize, counter)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		! MPI:
		integer, intent(in):: mpi_rank, mpi_master, mpi_last, mpi_ierr, loopSize, counter
		integer :: status(MPI_STATUS_SIZE)
		! aux:
		double precision, allocatable, dimension(:) :: secondToLast, secondlastToFirst ! Briefumschläge
		double precision :: star, corr !Hilfsvariablen
		integer :: send_request1, recv_request1, send_request2, recv_request2  !Hilfsvariablen
		integer :: i, j !Schleifenvariablen


		! Alloziieren:
		allocate(secondToLast(ubound(matrix, 1)))
		allocate(secondlastToFirst(ubound(matrix, 1)))
		
		! Senden vorletzte Zeile, ab iteration 2 bis loopSize + 1
		if ((mpi_rank + 1 < counter) .and.  (counter <= (loopSize+mpi_rank+1))) then
			if (mpi_rank /= mpi_last) then 
				secondlastToFirst = matrix(:,(ubound(matrix, 2)-1))
				call mpi_isend(secondlastToFirst, size(secondlastToFirst), & 
								& mpi_double, (mpi_rank + 1), 2005, &
								& mpi_comm_world, send_request2, mpi_ierr)
				call mpi_wait(send_request2, status, mpi_ierr)
				!print*, 'In Iterationsschritt', counter,  'hat Prozess' ,mpi_rank, 'gerade zweite Zeile an Prozess ',mpi_rank+1, 'gesendet'
			end if	
		end if
		
		! Empfangen, ab iteration 2, bis loopSize + 1
		! vorletzte Zeile von rank - 1, auf erste schreiben
		if ((mpi_rank < counter) .and.  (counter <= (loopSize+mpi_rank))) then
			if (mpi_rank /= mpi_master) then
				call MPI_IRECV(secondlastToFirst, size(secondlastToFirst), & 
							   & MPI_DOUBLE, (mpi_rank-1), 2005, &
							   & MPI_COMM_WORLD, recv_request2, mpi_ierr)
				! Sicherstellen, dass Zeile auch empfangen wurde
				call mpi_wait(recv_request2, status, mpi_ierr)
				matrix(:,1) = secondlastToFirst(:)
				!print*, 'In Iterationsschritt', counter, 'hat Prozess' ,mpi_rank, 'gerade letzte Zeile von Prozess ',mpi_rank+1, 'empfangen'
			end if
			! Zweite Reihe ausrechnen
			call calculateRow(matrix, 2)
			! Zweite Zeile an letzte des Vorgaengers senden
			if (mpi_rank /= mpi_master) then
				secondToLast = matrix(:, 2)
				call mpi_isend(secondToLast, size(secondToLast), & 
							   & mpi_double, (mpi_rank - 1), 2002, &
							   & mpi_comm_world, send_request1, mpi_ierr)
			end if
			! dann rest ausrechnen
			do j = 3, (ubound(matrix, 2)-2)
				call calculateRow(matrix, j)
			end do
			! Sicherstellen dass zw. Zeile versendet
			if (mpi_rank /= mpi_master) then
				call mpi_wait(send_request1, status, mpi_ierr)
			end if
		end if
		
		if ((mpi_rank + 1 < counter) .and.  (counter <= (loopSize+mpi_rank+1))) then		
			if (mpi_rank /= mpi_last) then
				call MPI_IRECV(secondToLast, size(secondToLast), & 
							   & MPI_DOUBLE, (mpi_rank + 1), 2002, &
							   & MPI_COMM_WORLD, recv_request1, mpi_ierr)
				! Sicherstellen, dass Zeile auch empfangen wurde
				call mpi_wait(recv_request1, status, mpi_ierr)
				matrix(:,ubound(matrix,2)) = secondToLast(:)
			end if
			! letzte Zeile ausrechnen

		end if	

		if ((mpi_rank < counter) .and.  (counter <= (loopSize+mpi_rank))) then
			call calculateRow(matrix, (ubound(matrix,2) - 1))		
		end if
		
		deallocate(secondToLast)
		deallocate(secondlastToFirst)

	end subroutine  calculate


	! Errechnet eine neue Zeile und trägt sie in temp ein.
	subroutine calculateRow(matrix, row)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		integer, intent(in) :: row ! Zeilennummer auf der gerechnet wird

		double precision :: star, corr !Hilfsvariable
		integer :: i !Schleifenvariablen

		! Rechnet für eine Zeile
		do i = lbound(matrix, 1)+1, ubound(matrix, 1)-1
			! Stern
			star = -matrix(i+1,row)-matrix(i,row-1)+4*matrix(i,row)-matrix(i,row+1)-matrix(i-1,row)
			! Korrektur
			corr = - star/4
			! Übertrag
			matrix(i,row) = matrix(i,row) + corr
		end do
	end subroutine  calculateRow

end module run
