! Copyright (C) 2020 Bolding & Bruggeman

!> @note
!> maybe put all grid tests in one big program - test_bathymetry
!> @endnote

PROGRAM test_read_netcdf
   !! Testing calculation of time varying depths at S, U and V points

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use input_module
   IMPLICIT NONE

!  Local constants

!  Local variables
   TYPE(type_netcdf_input) :: input
   integer :: stat
   real(real64), dimension(:), allocatable, target :: var1d
   real(real64), dimension(:,:), allocatable, target :: var2d
   real(real64), dimension(:,:,:), allocatable, target :: var3d
   integer :: i,j,k,l,m,n
   integer :: idx(3)
!-----------------------------------------------------------------------

   if (command_argument_count() .ne. 2 ) then
      write(*,*) 'ERROR, must be called like:'
      STOP ' test_input <file_name> <var_name>'
   end if

   call get_command_argument(1,input%f)
   call get_command_argument(2,input%v)
   call input%initialize()
   call input%print_info()
   i=-1; j=-1; k=-1
   m=1
   select case (input%ndims-1)
      case (1)
         do l=1,input%ndims
            if (l /= input%time_index) then
              idx(m) = l
              m=m+1
            end if
         end do
         i=input%dimlens(idx(1))
         allocate(var1d(i))
         input%p1dreal64 => var1d
      case (2)
         do l=1,input%ndims
            if (l /= input%time_index) then
              idx(m) = l
              m=m+1
            end if
         end do
         i=input%dimlens(idx(1))
         j=input%dimlens(idx(2))
         allocate(var2d(i,j))
         input%p2dreal64 => var2d
      case (3)
         do l=1,input%ndims
            if (l /= input%time_index) then
              idx(m) = l
              m=m+1
            end if
         end do
         i=input%dimlens(idx(1))
         j=input%dimlens(idx(2))
         k=input%dimlens(idx(3))
         allocate(var3d(i,j,k))
         input%p3dreal64 => var3d
   end select
!   do n=1,input%dimlens(input%time_index)
   do n=1,24
      call input%next(stat)
      write(*,*) n,var2d(10,90)
   end do
   call input%close()

!-----------------------------------------------------------------------

END PROGRAM test_read_netcdf
