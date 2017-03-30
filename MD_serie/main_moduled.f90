program md
!include 'mpif.h'
use analisis
!use search_arguments
!use integrate_verlet
!use integrate_euler
!use forces
!use pbc
implicit none
real(8):: deltat, BoxSize, mass,rc,epot, ekin,temperatura,pression
integer(8):: N,dimnsion,Nsteps,i,j,step
real(8), dimension(:,:), allocatable:: positions,accel,velocities
!datos de entrada de prueba
deltat=0.0032
Nsteps=10000
N = get_int_arg(1,int(256,8))
dimnsion=3
BoxSize=6.1984
mass=1.0
rc=2.5

allocate (positions(N,dimnsion))
allocate (accel(N,dimnsion))
allocate (velocities(N,dimnsion))
open (unit=10, File='coordenadas.dat')
do i=1,N
 !read(10,*) positions(i,:)
 positions(i,:) = (/(rand(), i=1,3)/)
end do

do i=1,N
  velocities(i,:) = (/(rand(), i=1,3)/)
end do
close (10)
!MAIN

!call init()

!open(unit=123,file='energy.dat',status='replace',action='write')

do step=1,Nsteps
 temperatura = T_compute(N,velocities)
 pression = P_compute(N,BoxSize,positions,accel,temperatura)
enddo

contains
!subrutinas
include 'get_int.inc'
end program md
