module mod_lifecycle
	implicit none
contains
	! subroutine developeLife(matrix)	
	! subroutine countNeighbors(matrix, posX, posY, neighbors)


	! Develops the given matrix by the following rules: (where alive means logical true)
	! - Any alive cell that is touching less than two alive neighbours dies.
	! - Any alive cell touching four or more alive neighbours dies.
	! - Any alive cell touching two or three alive neighbours does nothing.
	! - Any dead cell touching exactly three alive neighbours becomes alive.
	!
	subroutine developeLife(matrix)
		!
		! field
		logical, dimension(:,:), intent(inout) :: matrix
		! internal copy of the field
		logical, dimension(:,:), pointer :: matrixCopy
		! dimensions of the field
		integer :: width, height
		! loop counter, tempory storage
		integer :: i, j, neighbours

		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		allocate(matrixCopy(1:width,1:height))
		matrixCopy = matrix

		do i = 1,width
			do j = 1,height
				call countNeighbors(matrixCopy, i, j, neighbours)
				if (matrixCopy(i, j) .eqv. .true.) then
					matrix(i,j) = (neighbours > 1) .and. (neighbours < 4))
				else
					matrix(i,j) = (neighbours == 3)
				end if

			end do 
		end do
	end subroutine


	! Counts alive neighbours of one cell 
	!	matrix, posX, posY: matrix and coordinates of the cell in this matrix
	!	neighbours: alive neighbours of the cell
	!
	subroutine countNeighbors(matrix, posX, posY, neighbors)
		!
		! field
		logical, dimension(:,:), intent(in) :: matrix
		! coordinates
		integer, intent(in) :: posX, posY
		! return value: number of alive neighbors
		integer, intent(out) :: neighbors
		! dimensions of the field
		integer :: width, height
		! boundarys, between which neighbors are counted
		integer :: left, right, top, bottom
		! loop counter
		integer :: i, j

		neighbors = 0
		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		! setup of boundarys with distance to cell = 1
		left = posX-1
		right = posX+1
		top = posY-1
		bottom = posY+1
		! Anpassung der Boundarys für Felder am Spielfeldrand
		if(posX == 1) left = posX
		if(posX == width) right = posX
		if(posY == 1) top = posY
		if(posY == height) bottom = posY

		! counting of alive neighbours over 3x3 box  
		do i = left,right
			do j = top,bottom
				if(matrix(i,j).eqv. .true.) then       
					neighbors = neighbors + 1
				end if
			end do
		end do

		! correction for alive cells
		if matrix(posX, posY) .eqv. .true. then
					neighbors = neighbors -1 
		end if

	end subroutine
end module
