! Copyright (C) 2020 Bolding & Bruggeman

!!{!./example/code/dynamics.md!}

MODULE geostrophic_dynamics
   !! Description:

   use logging
   use field_manager
   use geostrophic_domain, only: type_geostrophic_domain
   use geostrophic_currents, only: type_geostrophic_currents
   use geostrophic_pressure, only: type_geostrophic_pressure

   IMPLICIT NONE

   PRIVATE  ! Private scope by default

!  Module constants

!  Module types and variables
   type, public :: type_geostrophic_dynamics

      TYPE(type_geostrophic_currents), public :: currents
      TYPE(type_geostrophic_pressure), public :: pressure

      contains

      procedure :: configure => dynamics_configure
      procedure :: initialize => dynamics_initialize
      procedure :: update => dynamics_update

   end type type_geostrophic_dynamics

!---------------------------------------------------------------------------

CONTAINS

!---------------------------------------------------------------------------

SUBROUTINE dynamics_configure(self,logs,fm)
   !! Configure all dynamical components

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_dynamics), intent(inout) :: self
   class(type_logging), intent(in) :: logs
   TYPE(type_field_manager) :: fm

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
   call logs%info('dynamics_configure()',level=1)
   call self%currents%configure(logs,fm)
   call self%pressure%configure(logs,fm)
   return
END SUBROUTINE dynamics_configure

!---------------------------------------------------------------------------

SUBROUTINE dynamics_initialize(self,logs,domain)
   !! Initialize all dynamical components

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_dynamics), intent(inout) :: self
   class(type_logging), intent(in) :: logs
   class(type_geostrophic_domain), intent(in) :: domain

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
   call logs%info('dynamics_initialize()',level=1)
   call self%currents%initialize(domain)
   call self%pressure%initialize(domain)
   return
END SUBROUTINE dynamics_initialize

!---------------------------------------------------------------------------

SUBROUTINE dynamics_update(self,logs)
   !! Update all dynamical components

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_dynamics), intent(inout) :: self
   class(type_logging), intent(in) :: logs

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
   call logs%info('dynamics_update()',level=1)
   !KBcall self%currents%update(logs,domain,fm)
   !KBcall self%pressure%update(logs,domain,fm)
   return
END SUBROUTINE dynamics_update

!---------------------------------------------------------------------------

END MODULE geostrophic_dynamics
