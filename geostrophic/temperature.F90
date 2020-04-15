! Copyright (C) 2020 Bolding & Bruggeman

!!{!./geostrophic/code/temperature.md!}
!! @todo
!! check if temperature is in-situ or potential - in any case convert to conservative


MODULE geostrophic_temperature

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use memory_manager
   use logging
   use field_manager
   use geostrophic_domain

   IMPLICIT NONE

   PRIVATE  ! Private scope by default

!  Module constants

!  Module types and variables
   type, public :: type_temperature
      !! temperature type

      class(type_logging), pointer :: logs
      class(type_field_manager), pointer :: fm
      class(type_geostrophic_domain), pointer :: domain
      real(real64), dimension(:,:,:), allocatable :: T
      character(len=256) :: f = "temperature.nc"

      contains

      procedure :: configuration => temperature_configuration
      procedure :: initialize => temperature_initialize
      procedure :: update => temperature_update
      procedure :: finalize => temperature_finalize
   end type type_temperature

CONTAINS

!---------------------------------------------------------------------------

SUBROUTINE temperature_configuration(self,logs,fm)

   !! Configure the the temperature module 

   IMPLICIT NONE

!  Subroutine arguments
   class(type_temperature), intent(out) :: self
   class(type_logging), intent(in), target :: logs
   TYPE(type_field_manager), intent(inout), target :: fm

!  Local constants

!  Local variables
   integer :: rc
!---------------------------------------------------------------------------
   self%logs => logs
   call logs%info('temperature_configuration()',level=2)
!   call logs%info('reading initial temperature from: ',level=3,msg2=trim(self%f))
   self%fm => fm
   return
END SUBROUTINE temperature_configuration

!---------------------------------------------------------------------------

SUBROUTINE temperature_initialize(self,domain)

   !! Initialize the temperature field

   IMPLICIT NONE

!  Subroutine arguments
   class(type_temperature), intent(inout) :: self
   class(type_geostrophic_domain), intent(in), target :: domain

!  Local constants

!  Local variables
   integer :: stat
!---------------------------------------------------------------------------
   call self%logs%info('temperature_initialize()',level=2)
   self%domain => domain
   call mm_s('T',self%T,self%domain%A%l,self%domain%A%u,def=15._real64,stat=stat)
   call self%fm%register('temp', 'Celsius', 'potential temperature', &
                         standard_name='sea_water_conservative_temperature', &
                         dimensions=(/id_dim_z/), &
                         category='temperature_and_salinity', &
                         part_of_state=.true.)
   call self%fm%send_data('temp', self%T)
!KB   call mm_print('temp',self%T)
   return
END SUBROUTINE temperature_initialize

!---------------------------------------------------------------------------

SUBROUTINE temperature_update(self)

   IMPLICIT NONE

!  Subroutine arguments
   class(type_temperature), intent(inout) :: self

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
   call self%logs%info('temperature_update()',level=3)
   return
END SUBROUTINE temperature_update

!---------------------------------------------------------------------------

SUBROUTINE temperature_finalize(self)
   !! Clean up the temperature module 

   IMPLICIT NONE

!  Subroutine arguments
   class(type_temperature), intent(out) :: self

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
!   call self%logs%info('temperature_finalize()',level=2)
   if(allocated(self%T)) deallocate(self%T)
   return
END SUBROUTINE temperature_finalize

!---------------------------------------------------------------------------

END MODULE geostrophic_temperature
