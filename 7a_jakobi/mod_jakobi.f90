MODULE run

	use mpi
	use finalize
	IMPLICIT NONE
	CONTAINS

	subroutine calculate(matrix, mpi_rank, mpi_master, mpi_last, mpi_ierr)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		double precision, allocatable, dimension(:,:) :: temp !temporäre Kopie
		integer, intent(in):: mpi_rank, mpi_master, mpi_last, mpi_ierr
		double precision, allocatable, dimension(:) :: first, last
		double precision :: star, corr !Hilfsvariable
		integer :: i, j, request1, request2 !Schleifenvariablen

		allocate(temp(ubound(matrix, 1), ubound(matrix, 2)))
		allocate(first(ubound(matrix, 2)))
		allocate(last(ubound(matrix, 2)))

		! 0. Schicke vorletze Zeile (n-1)
		! Schicke matrix(:,ubound(temp,2)-1) zu prog + 1
		
		
		last(:) = matrix(ubound(matrix,1)-1,:)
	
		if (mpi_rank /= mpi_last) then
			call MPI_ISEND(last, size(last), MPI_DOUBLE, mpi_rank+1, 2001, &
			& MPI_COMM_WORLD, request2, mpi_ierr)
		end if

		! 1. Rechne erste Zeile (2)
		j = lbound(matrix, 2)+1
		
		call calculateRow(matrix, temp, j)

		if (mpi_rank == 0) then
			call printRow(temp,lbound(matrix, 1), 1)
		end if
		
		! 2. Schicke erste Zeile (2)
		! schicke temp(:,2) zu prog-1
		first(:) = matrix(lbound(matrix,1)+1,:)
		if (mpi_rank /= mpi_master) then
			call MPI_ISEND(first, size(first), MPI_DOUBLE, mpi_rank-1, 2002, &
			& MPI_COMM_WORLD, request1, mpi_ierr)
		end if

		! 3. Rechne bis Ende-2
		do j = lbound(matrix, 2)+2, ubound(matrix, 2)-2
			call calculateRow(matrix, temp, j)
		end do

		! 5. Empfange letzte Zeile
		! Empfange temp(:,n) von prog+1
		if (mpi_rank /= mpi_last) then
			call MPI_IRECV(last, size(last), MPI_DOUBLE, mpi_rank+1, 2001, &
			& MPI_COMM_WORLD, request2, mpi_ierr)
		end if

		matrix(ubound(matrix,1),:) = last(:)

		! 6. Rechne letzte Zeile
		j = ubound(matrix, 2)-1
		call calculateRow(matrix, temp, j)

		! 7. Empfange erste Zeile
		! Empfange temp(:,1) von prog-1
		if (mpi_rank /= mpi_master) then
			call MPI_IRECV(first, size(first), MPI_DOUBLE, mpi_rank-1, 2001, &
			& MPI_COMM_WORLD, request1, mpi_ierr)
		end if
		
		!if (mpi_rank == 1) then
		!	call printRow(matrix,ubound(matrix, 1), 12)
		!end if

		matrix(lbound(matrix,1),:) = first(:)


		matrix = temp
		deallocate(temp)
		deallocate(first)
		deallocate(last)

	end subroutine  calculate


	subroutine calculateRow(matrix,temp, j)
		double precision, allocatable, dimension(:,:), intent(in) :: matrix !matrix
		double precision, allocatable, dimension(:,:), intent(inout) :: temp !temporäre Kopie
		integer, intent(in) :: j !"Schleifenvariable" Zeilenzahl

		double precision :: star, corr !Hilfsvariable
		integer :: i !Schleifenvariablen


		! Rechnet für eine Zeile
		do i = lbound(matrix, 2)+1, ubound(matrix, 2)-1
			! Stern
			star = -matrix(i,j+1)-matrix(i-1,j)+4*matrix(i,j)-matrix(i+1,j)-matrix(i,j-1)
			! Korrektur
			corr = - star/4
			! Übertrag
			temp(i,j) = matrix(i,j) + corr
		end do

	end subroutine  calculateRow


END MODULE run


