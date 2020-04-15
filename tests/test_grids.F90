! Copyright (C) 2020 Bolding & Bruggeman

!> This test program provides very simple tests of allocating, configuring
!> and printing 1D, 2D and 3D grids.

PROGRAM test_grids

   use grid_module
   IMPLICIT NONE

!  Local constants
   integer, parameter :: imin=1,imax=30,jmin=1,jmax=50,kmin=0,kmax=25,halo=2

!  Local variables
   class(type_1d_grid), allocatable :: grid_1d
   class(type_2d_grid), allocatable :: grid_2d
   class(type_3d_grid), allocatable :: grid_3d

   allocate(grid_1d)
   call grid_1d%create(imin=imin,imax=imax)
   call grid_1d%print(6)
   write(6,*) ''

   allocate(grid_2d)
   call grid_2d%create(imin=imin,imax=imax,jmin=jmin,jmax=jmax,halo=(/halo, halo/))
   call grid_2d%print(6)
   write(6,*) ''

   allocate(grid_3d)
   call grid_3d%create(imin=imin,imax=imax,jmin=jmin,jmax=jmax,kmin=kmin)
   call grid_3d%print(6)

   write(6,*) ''
   write(6,*) 'adding kmax and halo'
   call grid_3d%create(kmax=kmax,halo=(/halo, halo, 0/))
   call grid_3d%print(6)


!CONTAINS 

END PROGRAM test_grids
