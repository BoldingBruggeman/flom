! Copyright (C) 2020 Bolding & Bruggeman
!> Test program for the memory_manager module

PROGRAM test_memory_manager
   !! Testing time related functions and subroutines

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use memory_manager
   IMPLICIT NONE

!  Local constants

!  Local variables
   integer, allocatable, target :: int1d_s(:), int2d_s(:,:), int3d_s(:,:,:)
   real(real64), allocatable, target :: real1d_s(:), real2d_s(:,:), real3d_s(:,:,:)
   integer, dimension(3) :: l,u
   integer :: n,stat
   character(len=16) :: arg
!---------------------------------------------------------------------------
   write(*,*) 'Test mmemory_manageremory_manager()'

   if (command_argument_count() .ne. 6) then
      write(*,*) 'ERROR, must be called like:'
      STOP ' test_memory_manager imin imax jmin jmax kmin kmax'
   end if

   do n=1,6
      call get_command_argument(n,arg)
      select case (n)
         case(1,3,5)
            read(arg,*,iostat=stat)  l((n+1)/2)
         case(2,4,6)
            read(arg,*,iostat=stat)  u((n+1)/2)
      end select
   end do

   ! 1D
   call mm_s('int1d_s',int1d_s,l(1),u(1),stat=stat)
   call mm_s('real1d_s',real1d_s,l(1),u(1),stat=stat)
   int1d_s = 10
   real1d_s = 10._real64
   write(*,*)
   call mm_print('int1d_s',int1d_s)
   write(*,*)
   call mm_print('real1d_s',real1d_s)

   ! 2D
   call mm_s('int2d_s',int2d_s,l(1:2),u(1:2),stat=stat)
   call mm_s('real2d_s',real2d_s,l(1:2),u(1:2),stat=stat)
   int2d_s = 20
   real2d_s = 20._real64
   write(*,*)
   call mm_print('int2d_s',real2d_s)
   write(*,*)
   call mm_print('real2d_s',real2d_s)

   ! 3D
   call mm_s('int3d_s',int3d_s,l,u,stat=stat)
   call mm_s('real3d_s',real3d_s,l,u,stat=stat)
   int3d_s = 30
   real3d_s = 30._real64
   write(*,*)
   call mm_print('int3d_s',real3d_s)
   write(*,*)
   call mm_print('real3d_s',real3d_s)

!CONTAINS 

!---------------------------------------------------------------------------

END PROGRAM test_memory_manager
