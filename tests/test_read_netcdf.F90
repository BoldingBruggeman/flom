! Copyright (C) 2020 Bolding & Bruggeman

!> @note
!> maybe put all grid tests in one big program - test_bathymetry
!> @endnote

PROGRAM test_read_netcdf
   !! Testing the input module by reading NetCDF formatted files

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use input_module
   use datetime_module
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
   type(datetime) :: t
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
         call input%link_data(var1d)
!KB         input%p1dreal64 => var1d
      case (2)
real64_2d: block
!         real(real64), dimension(:,:), allocatable, target :: var2d
         do l=1,input%ndims
            if (l /= input%time_index) then
              idx(m) = l
              m=m+1
            end if
         end do
         i=input%dimlens(idx(1))
         j=input%dimlens(idx(2))
         allocate(var2d(i,j))
         call input%link_data(var2d)
!KB         input%p2dreal64 => var2d
end block real64_2d
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
         call input%link_data(var3d)
!KB         input%p3dreal64 => var3d
   end select
   t = input%datetimes(8)
   call input%attime(t,stat)
   write(*,*) n,t%isoformat(),var2d(10,90)
   do n=1,input%timelen
      t = input%datetimes(n)
      call input%attime(t,stat)
!      call input%prev(stat)
      write(*,*) n,t%isoformat(),var2d(10,90)
   end do
   call input%close()

!-----------------------------------------------------------------------

END PROGRAM test_read_netcdf
