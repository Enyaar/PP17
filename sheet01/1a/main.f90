program main
! das Programm streicht hintereinadner jede dritte Zahl in einem 32 
! Zahlen langem Kreis und schreibt diese in eine Liste. Ist die Zahl 32
! erreicht, wird der Kreis geschlossen und es wird von der 1. Position
! im Kreis weiter gezählt, bis alle 32 Zahlen in einer Liste stehen und 
! der Kreis "leer" ist.
implicit none
integer , dimension(32) :: circle, list ! Zahlenkreis und output Liste
integer :: counter = 0, ind1=0, ind2=1, i ! Dekl. (+ Init.)
!
do, i=1,32
 circle(i) = i
 list(i) = 0
end do
do while(32 > counter) !do while schleife
 i = 0 ! i zaehlt hoch, falls eine Zahl im Kreis noch exisitiert
 do 
  ind1 = ind1+1
  if(ind1 > 32) then ! Prueft ob ind1 groesser als 32 (index von cirlce)
  ind1 = ind1-32 ! Wenn T, dann wird 32 abgezogen
  endif
  if (circle(ind1)/=0) then ! Prueft ob an Index ind1 Zahl im circle existiert
   i = i+1 ! zahlt nur hoch, wenn Zahl im Kreis noch vorhanden
  endif
  if (i == 3) exit
 end do
 list(ind2) = circle(ind1)
 counter = counter + 1 ! controlSum wird hochgezaehlt
 circle(ind1) = 0
 ind2 = ind2+1
end do
print*,'Reihenfolge jeder 3. Person im Kreis'
print*,'      Index', ' Reihenfolge'
do, i=1,32 ! Kontrollstruktur zum Drucken der Liste auf Konsole
  print*,i,list(i)
end do
end program main
