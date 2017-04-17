! (c) 2012 Körner, Hübbe
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

program glider_movie
	use mod_initializeField
	use mod_lifecycle
	implicit none
	integer :: positionX, positionY, i, outputUnit = 6
	logical, dimension(:,:), pointer :: blinker, toad, beacon, world

	open(outputUnit, encoding='UTF-8')  ! change character set to UTF-8 for stdout, printTwoDLogical needs that.
	call createFigures(blinker, toad, beacon)
	call createField(world)
	call printTwoDLogical(outputUnit, blinker)
	call printTwoDLogical(outputUnit, toad)
	call printTwoDLogical(outputUnit, beacon)


	! The current position of the northwest corner of the glider in world coordinates.
	positionX = 4
	positionY = 3
	world(positionX:,positionY:) = blinker
	world(positionX+10:,positionY+5:) = toad
	world(positionX+15:,positionY+10:) = beacon

	do i = 1,10

		call printTwoDLogical(outputUnit, world)
		call developeLife(world)



	end do

	call portable_sleep(0.3)

	deallocate(blinker)
	deallocate(world)

end program
