program main
	use mod_initializeField
	use mod_lifecycle
	implicit none
	integer :: i, outputUnit = 6
	logical, dimension(:,:), pointer :: world

	open(outputUnit, encoding='UTF-8')  ! change character set to UTF-8 for stdout, printTwoDLogical needs that.

	! Subroutine in mod_initializeField inizilisiert Matrix world
	! gibt Matrix zurück
	call createField(world)
	! Subroutine in mod_initializeField belegt Matrix world mit figuren
	! gibt Matrix zurück
	call createFigures(world)

	! Subroutine in mod_initializeField gibt world auf Konsole aus
	call printTwoDLogical(outputUnit, world) 


	do i = 1,10
		! Subroutine in mod_lifecycle "Entwickelt leben" in Matrix world
		! gibt Matrix um ein lebenschritt weiter zurück
		call developeLife(world)
		! Subroutine in mod_initializeField gibt world auf Konsole aus
		call printTwoDLogical(outputUnit, world)
	end do

	call portable_sleep(0.3)
	deallocate(world)

end program
