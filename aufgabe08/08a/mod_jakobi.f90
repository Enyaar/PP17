module run
	!
	use mpi
	use finalize
	!
	implicit none
	!
	contains

	! Wendet den Jacobi Algorhytmus einmal auf eine Teilmatrix an, kommuniziert mit den Nachbarn
	subroutine calculate(matrix, mpi_rank, mpi_master, mpi_last, mpi_ierr)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		double precision, allocatable, dimension(:,:) :: temp !temporäre Kopie
		! MPI:
		integer, intent(in):: mpi_rank, mpi_master, mpi_last, mpi_ierr
		integer :: status(MPI_STATUS_SIZE)
		! aux:
		double precision, allocatable, dimension(:) :: secondToLast, secondlastToFirst ! Briefumschläge
		double precision :: star, corr !Hilfsvariablen
		integer :: send_request1, recv_request1, send_request2, recv_request2  !Hilfsvariablen
		integer :: i, j !Schleifenvariablen

		! Alloziieren:
		allocate(temp(ubound(matrix, 1), ubound(matrix, 2)))
		allocate(secondToLast(ubound(matrix, 1)))
		allocate(secondlastToFirst(ubound(matrix, 1)))
		temp = 0.

		! Erste Reihe ausrechnen
		call calculateRow(matrix, temp, 2)
		! Und (falls nicht master) auch versenden
		if (mpi_rank /= mpi_master) then
			secondToLast = temp(:, 2)
			call mpi_isend(secondToLast, size(secondToLast), mpi_double, (mpi_rank - 1), 2002, &
					mpi_comm_world, send_request1, mpi_ierr)
		end if
		
		! Letzte Zeile ausrechnen
		call calculateRow(matrix, temp, (ubound(matrix,2) - 1))
		! Und (falls nicht last) auch versenden
		if (mpi_rank /= mpi_last) then 
			secondlastToFirst = temp(:,(ubound(temp, 2)-1))
			call mpi_isend(secondlastToFirst, size(secondlastToFirst), mpi_double, (mpi_rank + 1), 2005, &
					mpi_comm_world, send_request2, mpi_ierr)
		end if


		! Den ganzen Rest ausrechnen.
		do j = 3, ubound(matrix, 2)-2
			call calculateRow(matrix, temp, j)
		end do


		! sicherstellen, dass die zweite letzte Zeile auch versendet wurde
		if (mpi_rank /= mpi_master) then
			call mpi_wait(send_request1, status, mpi_ierr)
		end if
			
		! Wenn last übertragen der letzen zeile
		if (mpi_rank == mpi_last) then
			temp(:,ubound(temp,2)) = matrix(:,ubound(matrix,2))
		! Sonst empfangen
		else
			call MPI_IRECV(secondToLast, size(secondToLast), MPI_DOUBLE, (mpi_rank + 1), 2002, &
			& MPI_COMM_WORLD, recv_request1, mpi_ierr)
			! Sicherstellen, dass Zeile auch empfangen wurde
			call mpi_wait(recv_request1, status, mpi_ierr)
			temp(:,ubound(temp,2)) = secondToLast(:)
		end if

		! sicherstellen, dass die vorletzte Zeile auch versendet wurde
		if (mpi_rank /= mpi_last) then
			call mpi_wait(send_request2, status, mpi_ierr)
		end if
		
		! Wenn master übertragen der ersten Zeile
		if (mpi_rank == mpi_master) then
			temp(:,1) = matrix(:,1)	
		! Sonst vorletzet Zeile empfangen und auf erste schreiben
		else
			call MPI_IRECV(secondlastToFirst, size(secondlastToFirst), MPI_DOUBLE, (mpi_rank-1), 2005, &
			& MPI_COMM_WORLD, recv_request2, mpi_ierr)
			! Sicherstellen, dass Zeile auch empfangen wurde
			call mpi_wait(recv_request2, status, mpi_ierr)
			temp(:,1) = secondlastToFirst(:)
		end if

		! Übertrag
		matrix = temp
		! Speicher freigeben
		deallocate(temp)
		deallocate(secondToLast)
		deallocate(secondlastToFirst)

	end subroutine  calculate


	! Errechnet eine neue Zeile und trägt sie in temp ein.
	subroutine calculateRow(matrix,temp, row)
		double precision, allocatable, dimension(:,:), intent(in) :: matrix !matrix
		double precision, allocatable, dimension(:,:), intent(inout) :: temp !temporäre Kopie
		integer, intent(in) :: row ! Zeilennummer auf der gerechnet wird

		double precision :: star, corr !Hilfsvariable
		integer :: i !Schleifenvariablen

		! Ränder:
		temp(1, row) = matrix(1,row)
		temp(ubound(matrix,1),row) = matrix(ubound(matrix,1), row)

		! Rechnet für eine Zeile
		do i = lbound(matrix, 1)+1, ubound(matrix, 1)-1
			! Stern
			star = -matrix(i+1,row)-matrix(i,row-1)+4*matrix(i,row)-matrix(i,row+1)-matrix(i-1,row)
			! Korrektur
			corr = - star/4
			! Übertrag
			temp(i,row) = matrix(i,row) + corr
		end do
	end subroutine  calculateRow

end module run
