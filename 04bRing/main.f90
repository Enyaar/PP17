program main

	use mpi

	implicit none

	integer :: myid, ierr, numproc, status(mpi_status_size)
	integer :: i, j, summe = 0, recId, receiver, sender

	call mpi_init(ierr) 

	call mpi_comm_rank(mpi_comm_world, myId, ierr)
	call mpi_comm_size(mpi_comm_world, numproc, ierr)
	! zuerst wird die Empfangs-ID auf die eigene gesetzt
	recId = myId

	! Ausgabe Konsole (nur 1 mal)
	if (myId == 0) then
		print*, "Anzahl der Prozessoren:",numproc
		print*, "          ID,         Summe"
	end if

	! Iteration = Anzahl Prozesse 
	do i=0, numproc-1
		! Empfänger festlegen
		receiver = modulo(myId+1,numproc)
		! Eigene / Empfangene ID senden
		call mpi_send(recId, 1, mpi_integer, receiver, 2017, mpi_comm_world, ierr)

		! Sender festlegen
		sender = modulo(myId-1,numproc) 
		! ID Empfangen
		call mpi_recv(recId, 1, mpi_integer, sender, 2017, mpi_comm_world, status, ierr)
		! ID aufsummieren
		summe = summe + recId

	end do

	! Ausgabe auf Konsole
	print*, myId, summe

	call mpi_finalize(ierr) 

end program

