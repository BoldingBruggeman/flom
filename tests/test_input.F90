! Copyright (C) 2020 Bolding & Bruggeman

!> @note
!> maybe put all grid tests in one big program - test_bathymetry
!> @endnote

PROGRAM test_input
   !! Testing calculation of time varying depths at S, U and V points

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use input_module
   IMPLICIT NONE

!  Local constants

!  Local variables
   TYPE(type_netcdf_input) :: input
!-----------------------------------------------------------------------

   if (command_argument_count() .ne. 2 ) then
      write(*,*) 'ERROR, must be called like:'
      STOP ' test_input <file_name> <var_name>'
   end if

   call get_command_argument(1,input%f)
   call get_command_argument(2,input%v)
   call input%initialize()
   call input%print_info()
   !input%strptime = '%Y-%m-%d %H:%M:%S' 

!-----------------------------------------------------------------------

!CONTAINS 

!-----------------------------------------------------------------------

END PROGRAM test_input
