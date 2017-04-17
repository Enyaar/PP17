module mod_initializeField
	use mo_utilities
	implicit none
	private utf32
	!This character kind allows us to access the entire unicode characters et. For details, see the wikipedia article for UTF-32.
	integer, parameter :: utf32=selected_char_kind('ISO_10646')
contains

	subroutine createField(matrix)
		logical, dimension(:,:), pointer, intent(out) :: matrix
		allocate(matrix(1:30,1:20))
		matrix = .false.
	end subroutine

	subroutine createFigures(blinker, toad, beacon)
		logical, dimension(:,:), pointer, intent(out) :: blinker, &
														 toad, beacon
		allocate(blinker(1:3,1:1))
		blinker = .true.
		
		allocate(toad(1:4,1:2))
		toad = .true.
		toad(1,1) = .false.
		toad(4,2) = .false.
		
		allocate(beacon(1:6,1:6))
		beacon = .false.
		beacon(2:3,2) = .true.
		beacon(2,3) = .true.
		beacon(5,4) = .true.
		beacon(4:5,5) = .true.
	end subroutine 

	
	!The outputUnit must be opened with UTF-8 as the encoding!
	subroutine printTwoDLogical(outputUnit, matrix)
		integer, intent(in) :: outputUnit
		logical, dimension(:,:), intent(in) :: matrix
		integer :: width, height
		character(kind=utf32, len=1), dimension(:,:), allocatable :: characterMatrix
		! z'1b' hexadezimale Zahl, dezimaler Wert ist 27 
		! char(int(z'00B7'), utf32) ist das Zeichen mit der hexad. Nummer 00B7 
		! des Zeichensatzes 'ISO_10646'
		! 00B7 = middle dot, 2588 = full block
		character(kind=utf32, len=1) :: blockChar = char(int(z'2588'), utf32), emptyChar = char(int(z'00B7'), utf32)
		character(len=22) :: formatString
		
		!Create the character matrix.
		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		allocate(characterMatrix(1:width, 1:height))
		where (matrix)
			characterMatrix = blockChar
		else where
			characterMatrix = emptyChar
		end where

		!Write it out.
		formatString = '(' // intToStr(width) // 'a)'
		write(outputUnit, formatString) characterMatrix
		formatString = '(' // intToStr(width) // '("="))'
		write(outputUnit, formatString)
		
		!Waste some time.
		call portable_sleep(0.3)
		deallocate(characterMatrix)
	end subroutine
	
end module
