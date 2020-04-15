! Copyright (C) 2020 Bolding & Bruggeman

!!{!./geostrophic/code/salinity.md!}
!! @todo
!! check is salinity is in-situ or potential - in any case convert to absolute

MODULE geostrophic_salinity
   !! Description:

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use memory_manager
   use logging
   use field_manager
   use geostrophic_domain

   IMPLICIT NONE

   PRIVATE  ! Private scope by default

!  Module constants

!  Module types and variables
   type, public :: type_salinity
      !! salinity type

      class(type_logging), pointer :: logs
      class(type_field_manager), pointer :: fm
      class(type_geostrophic_domain), pointer :: domain
      character(len=256) :: f = "salinity.nc"

      real(real64), dimension(:,:,:), allocatable :: S

      contains

      procedure :: configuration => salinity_configuration
      procedure :: initialize => salinity_initialize
      procedure :: update => salinity_update
      procedure :: finalize => salinity_finalize

   end type type_salinity

CONTAINS

!---------------------------------------------------------------------------

SUBROUTINE salinity_configuration(self,logs,fm)
   !! Configure the salinity

   IMPLICIT NONE

!  Subroutine arguments
   class(type_salinity), intent(out) :: self
   class(type_logging), intent(in), target :: logs
   class(type_field_manager), intent(inout), target :: fm

!  Local constants

!  Local variables
   integer :: rc
!---------------------------------------------------------------------------
   self%logs => logs
   call logs%info('salinity_configuration()',level=2)
!   call logs%info('reading initial salinity from: ',level=3,msg2=trim(self%f))
   self%fm => fm
   return
END SUBROUTINE salinity_configuration

!---------------------------------------------------------------------------

SUBROUTINE salinity_initialize(self,domain)
   !! Initialize the salinity

   IMPLICIT NONE

!  Subroutine arguments
   class(type_salinity), intent(inout) :: self
   class(type_geostrophic_domain), intent(in), pointer :: domain

!  Local constants

!  Local variables
   integer :: stat
!---------------------------------------------------------------------------
   call self%logs%info('salinity_initialize()',level=2)
   self%domain => domain
   write(*,*) self%domain%A%l
   write(*,*) self%domain%A%u
   call mm_s('S',self%S,self%domain%A%l,self%domain%A%u,def=35._real64,stat=stat)
   write(*,*) self%domain%A%l
   write(*,*) self%domain%A%u
   write(*,*) 'stat= ',stat
!   stop 'kaj'
   call self%fm%register('salt', 'kg/kg', 'absolute salinity', &
                          standard_name='sea_water_absolute_salinity', &
                          dimensions=(/id_dim_z/), &
                          category='temperature_and_salinity', &
                          part_of_state=.true.)
   call self%fm%send_data('salt', self%S)
   return
END SUBROUTINE salinity_initialize

!---------------------------------------------------------------------------

SUBROUTINE salinity_update(self)

   IMPLICIT NONE

!  Subroutine arguments
   class(type_salinity), intent(inout) :: self

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
   call self%logs%info('salinity_update()',level=3)
   return
END SUBROUTINE salinity_update

!---------------------------------------------------------------------------

SUBROUTINE salinity_finalize(self)
   !! Clean up the salinity module

   IMPLICIT NONE

!  Subroutine arguments
   class(type_salinity), intent(out) :: self

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
!   call self%logs%info('salinity_finalize()',level=2)
   if(allocated(self%S)) deallocate(self%S)
   return
END SUBROUTINE salinity_finalize

!---------------------------------------------------------------------------

END MODULE geostrophic_salinity
