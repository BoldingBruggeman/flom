! Copyright (C) 2020 Bolding & Bruggeman

!!{!./geostrophic/code/input.md!}

MODULE geostrophic_input

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use logging
   use input_module
   use geostrophic_domain

   IMPLICIT NONE

!-----------------------------------------------------------------------------

   PRIVATE  ! Private scope by default

!  Module constants

!  Module types and variables
   character(len=*), parameter :: basename="woa18_decav_"

   type, public :: type_geostrophic_input
      class(type_logging), pointer :: logs
      class(type_geostrophic_domain), pointer :: domain
      class(type_netcdf_input), allocatable :: lat,lon,depth
      class(type_netcdf_input), dimension(:), allocatable :: Tinput,Sinput
      character(len=128) :: datadir
      character(len=8) :: resolution
      character(len=2) :: a
      contains
      procedure :: configure => configure_input
      procedure :: initialize => initialize_input
      procedure :: update => update_input
      procedure :: close => close_input
   end type type_geostrophic_input
!-----------------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------------

SUBROUTINE configure_input(self,logs,start,count)
   !! Configure - optional - hyperslab to read

   IMPLICIT NONE

! Subroutine arguments
   class(type_geostrophic_input), intent(inout) :: self
   class(type_logging), intent(in), target :: logs
   integer, dimension(:), intent(in), optional :: start,count

! Local constants

! Local variables
   integer :: n
!-----------------------------------------------------------------------------
   self%logs => logs
   call self%logs%info('input_configure()',level=1)
   allocate(self%lon)
   call self%lon%configure(start=start(1:1),count=count(1:1))
   allocate(self%lat)
   call self%lat%configure(start=start(2:2),count=count(2:2))
   allocate(self%depth)
   call self%depth%configure(start=start(3:3),count=count(3:3))
   allocate(self%Sinput(12))
   allocate(self%Tinput(12))
   do n=1,12
      call self%Sinput(n)%configure(start=start,count=count)
      call self%Tinput(n)%configure(start=start,count=count)
   end do
   return
END SUBROUTINE configure_input

!-----------------------------------------------------------------------------

SUBROUTINE initialize_input(self,domain,S,T)
   !! Open a NetCDF file and get a variable id

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_input), intent(inout) :: self
   class(type_geostrophic_domain), intent(in), target :: domain
   real(real64), dimension(:,:,:), target :: S,T

!  Local constants

!  Local variables
   integer :: rc
   integer :: i,n
   character(len=256) :: path
!-----------------------------------------------------------------------------
   call self%logs%info('input_initialize()',level=1)
   self%domain => domain
   write(path,'(3A,I2.2,3A)') trim(self%datadir),basename,'t',1,'_',self%a,'.nc'
   self%lon%f = path; self%lon%v = 'lon'
   call self%lon%initialize()
   self%lon%p1dreal64 => self%domain%A%lon
   call self%lon%get(); call self%lon%close()
   self%lat%f = path; self%lat%v = 'lat'
   call self%lat%initialize()
   self%lat%p1dreal64 => self%domain%A%lat
   call self%lat%get(); call self%lat%close()
   self%depth%f = path; self%depth%v = 'depth'
   call self%depth%initialize()
   self%depth%p1dreal64 => self%domain%A%depth
   call self%depth%get(); call self%depth%close()

   do n=1,12
      write(path,'(3A,I2.2,3A)') trim(self%datadir),basename,'s',n,'_',self%a,'.nc'
      self%Sinput(n)%f = path
      if (self%resolution(1:1) == 'l') then
         self%Sinput(n)%v = 's_mn'
      else
         self%Sinput(n)%v = 's_an'
      end if
      call self%Sinput(n)%initialize()

      write(path,'(3A,I2.2,3A)') trim(self%datadir),basename,'t',n,'_',self%a,'.nc'
      self%Tinput(n)%f = path
      if (self%resolution(1:1) == 'l') then
         self%Tinput(n)%v = 't_mn'
      else
         self%Tinput(n)%v = 't_an'
      end if
      call self%Tinput(n)%initialize()

      self%Sinput(n)%p3dreal64 => S
      self%Tinput(n)%p3dreal64 => T
   end do
   return
END SUBROUTINE initialize_input

!-----------------------------------------------------------------------------

SUBROUTINE update_input(self,n)
   !! Read variable at time indes n

   IMPLICIT NONE

! Subroutine arguments
   class(type_geostrophic_input), intent(inout) :: self
   integer, intent(in) :: n

! Local constants

! Local variables
!-----------------------------------------------------------------------------
   call self%logs%info('input_update()',level=3)
   call self%Tinput(n)%get()
   call self%Sinput(n)%get()
   return
END SUBROUTINE update_input

!-----------------------------------------------------------------------------

SUBROUTINE close_input(self)

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_input), intent(inout) :: self

!  Local constants

!  Local variables
!-----------------------------------------------------------------------------
   return
END SUBROUTINE close_input

!-----------------------------------------------------------------------------

END MODULE geostrophic_input
