module mod_lifecycle
	implicit none
contains

	subroutine developeLife(matrix)
		logical, dimension(:,:), intent(inout) :: matrix
		logical, dimension(:,:), pointer :: matrixIn
		integer :: width, height, n, i, j
		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		allocate(matrixIn(1:width,1:height))
		matrixIn = matrix
		do i = 1,width
			do j = 1,height
				call countNeighbors(matrixIn, i, j, n)
				if((n < 2) .and. (matrixIn(i,j) .eqv. .true.)) then
					matrix(i,j) = .false.
				else if (n >= 2 .and. n <= 3 .and. matrixIn(i,j) .eqv. .true.) then
					matrix(i,j) = .true.
				else if(n > 3) then
					matrix(i,j) = .false.
				else if((n == 3) .and. (matrixIn(i,j) .eqv. .false.)) then
					matrix(i,j) = .true.
				end if
			end do 
		end do
	end subroutine

	subroutine countNeighbors(matrix, posX, posY, neighbors)
		logical, dimension(:,:), intent(in) :: matrix
		integer, intent(in) :: posX, posY
		integer, intent(out) :: neighbors
		integer :: i1, i2, j1, j2, width, height, i, j
		neighbors = 0
		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		i1 = posX-1
		i2 = posX+1
		j1 = posY-1
		j2 = posY+1
		if(posX == 1) i1 = posX
		if(posX == width) i2 = posX
		if(posY == 1) j1 = posY
		if(posY == height) j2 = posY
		do i = i1,i2
			do j = j1,j2
!				print*,((i /= posX) .and. (j /= posY))
				if(matrix(i,j).eqv. .true.) then!.and. ((i /= posX) .and. (j /= posY))) then
					neighbors = neighbors + 1
				end if
				if((i == posX) .and. (j == posY) .and. (matrix(i,j) .eqv. .true.)) then
					neighbors = neighbors -1 
				end if
			end do
		end do
	end subroutine
end module

