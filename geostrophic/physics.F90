! Copyright (C) 2020 Bolding & Bruggeman

!!{!./example/code/physics.md!}

MODULE geostrophic_physics
   !! Description:

   use iso_fortran_env
   use logging
   use field_manager
   use geostrophic_domain
   use geostrophic_salinity
   use geostrophic_temperature
   use geostrophic_density

   IMPLICIT NONE

   PRIVATE  ! Private scope by default

!  Module constants

!  Module types and variables
   type, public :: type_geostrophic_physics
      TYPE(type_salinity), public :: salinity
      TYPE(type_temperature), public :: temperature
      TYPE(type_density), public :: density

      contains

      procedure :: configure => physics_configure
      procedure :: initialize => physics_initialize
      procedure :: update => physics_update

   end type type_geostrophic_physics

!---------------------------------------------------------------------------

CONTAINS

!---------------------------------------------------------------------------

SUBROUTINE physics_configure(self,logs,fm)
   !! Configure all physical components

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_physics), intent(inout) :: self
   class(type_logging), intent(in) :: logs
   class(type_field_manager), intent(inout) :: fm

!  Local constants

!  Local variables
! ---------------------------------------------------------------------------
   call logs%info('physics_configure()',level=1)
   call self%salinity%configuration(logs,fm)
   call self%temperature%configuration(logs,fm)
   call self%density%configure(logs,fm)
   return
END SUBROUTINE physics_configure

!---------------------------------------------------------------------------

SUBROUTINE physics_initialize(self,logs,domain)
   !! Initialize all physical components

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_physics), intent(inout) :: self
   class(type_logging), intent(in) :: logs
   class(type_geostrophic_domain), intent(in), target :: domain

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
   call logs%info('physics_initialize()',level=1)
   call self%salinity%initialize(domain)
   call self%temperature%initialize(domain)
   call self%density%initialize(domain)
   return
END SUBROUTINE physics_initialize

!---------------------------------------------------------------------------

SUBROUTINE physics_update(self,logs)
   !! Update all physical components

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_physics), intent(inout) :: self
   class(type_logging), intent(in) :: logs

!  Local constants

!  Local variables
   integer :: rc

!---------------------------------------------------------------------------
   call logs%info('physics_update()',level=1)
!KB   call self%salinity%update(logs)
!KB   call self%temperature%update(logs)
!KB   call self%density%update(logs)
   return
END SUBROUTINE physics_update

!---------------------------------------------------------------------------

END MODULE geostrophic_physics
