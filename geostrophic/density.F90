! Copyright (C) 2020 Bolding & Bruggeman

!!{!./geostrophic/code/density.md!}

MODULE geostrophic_density

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use gsw_mod_toolbox, only: gsw_rho
   use memory_manager
   use logging
   use field_manager
   use geostrophic_domain, only: type_geostrophic_domain

   IMPLICIT NONE

   PRIVATE  ! Private scope by default

!  Module constants

!  Module types and variables
   type, public :: type_density

      class(type_logging), pointer :: logs
      class(type_field_manager), pointer :: fm
      class(type_geostrophic_domain), pointer :: domain

      real(real64), dimension(:,:,:), allocatable :: rho
         !! density [\({kg}/m^3\)]
      real(real64), dimension(:,:,:), allocatable :: buoy
         !! buoyancy [\({m}/s^2\)]
!KB      real(real64), dimension(:,:,:), allocatable :: NN
      contains

      procedure :: configure => density_configure
      procedure :: initialize => density_initialize
      procedure :: calculate => density_calculate
      procedure :: buoyancy => buoyancy_calculate
      procedure :: finalize => density_finalize
!KB      procedure :: brunt_vaisala => brunt_vaisala_calculate

   end type type_density

CONTAINS

SUBROUTINE density_configure(self,logs,fm)

   !! Configure the the density module 

   IMPLICIT NONE

!  Subroutine arguments
   class(type_density), intent(inout) :: self
   class(type_logging), intent(in), target :: logs
   TYPE(type_field_manager), intent(inout), target :: fm

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
   self%logs => logs
   call self%logs%info('density_configuration()',level=2)
   self%fm => fm
   return
END SUBROUTINE density_configure

!---------------------------------------------------------------------------

SUBROUTINE density_initialize(self,domain)
   !! Allocate density memory and add density and buoyancy to the field_manager

   IMPLICIT NONE

!  Subroutine arguments
   class(type_density), intent(inout) :: self
   class(type_geostrophic_domain), intent(in), target :: domain

!  Local constants

!  Local variables
   integer :: stat
!-----------------------------------------------------------------------------
   call self%logs%info('density_initialize()',level=2)

   self%domain => domain
   call mm_s('rho',self%rho,self%domain%A%l,self%domain%A%u,def=-9999._real64,stat=stat)
   call mm_s('buoy',self%buoy,self%domain%A%l,self%domain%A%u,def=-9999._real64,stat=stat)
!KB   call mm_s('NN',self%NN,domain%A%l,domain%A%u,def=0._real64)
   call self%fm%register('rho', 'kg/m3', 'density', &
                         standard_name='sea_water_density', &
                         dimensions=(/id_dim_z/), &
                         category='temperature_and_salinity', &
                         fill_value=-9999._real64)
   call self%fm%send_data('rho', self%rho)
   call self%fm%register('buoy', 'm/s2', 'buoyancy', &
                         standard_name='', &
                         dimensions=(/id_dim_z/), &
                         category='temperature_and_salinity', &
                         fill_value=-9999._real64)
   call self%fm%send_data('buoy', self%buoy)
  return
END SUBROUTINE density_initialize

!---------------------------------------------------------------------------

SUBROUTINE density_calculate(self,S,T,p)
   !! Calculate the density based on updated temperature and salinity fields

   IMPLICIT NONE

!  Subroutine arguments
   class(type_density), intent(inout) :: self
   real(real64), dimension(:,:,:), intent(in) :: S
      !! salinity
   real(real64), dimension(:,:,:), intent(in) :: T
      !! temperature
   real(real64), dimension(:,:,:), optional, intent(in) :: p
      !! Pressure

!  Local constants

!  Local variables
   integer :: i,j,k
   integer :: imin,imax
   integer :: jmin,jmax
   integer :: kmin,kmax
!-----------------------------------------------------------------------------
   call self%logs%info('density_calculate()',level=3)
   imin=lbound(self%rho,1); imax=ubound(self%rho,1)
   jmin=lbound(self%rho,2); jmax=ubound(self%rho,2)
   kmin=lbound(self%rho,3); kmax=ubound(self%rho,3)
#if 1
   do k=kmin,kmax
      do j=jmin,jmax
         do i=imin,imax
            if (self%domain%A%mask(i,j,k) .gt. 0) then
               self%rho(i,j,k) = gsw_rho(S(i,j,k), T(i,j,k), 0._real64)
            end if
         end do
      end do
   end do
#else
   if (present(p)) then
      self%rho = gsw_rho(S,T,p)
   end if
#endif
  return
END SUBROUTINE density_calculate

!-----------------------------------------------------------------------------

SUBROUTINE buoyancy_calculate(self)
   !! Calculate the buoyancy based on updated density field

   IMPLICIT NONE

!  Subroutine arguments
   class(type_density), intent(inout) :: self

!  Local constants
   real(real64), parameter :: g = 9.81_real64
      !! g = gravitational acceleration
   real(real64), parameter :: rho_0 = 1025._real64
      !! (/ rho_0 /) = reference density
   real(real64), parameter :: x = g/rho_0

!  Local variables
   integer :: i, j, k
   integer :: imin,imax
   integer :: jmin,jmax
   integer :: kmin,kmax
!-----------------------------------------------------------------------------
   call self%logs%info('buoyancy_calculate()',3)
   imin=lbound(self%rho,1); imax=ubound(self%rho,1)
   jmin=lbound(self%rho,2); jmax=ubound(self%rho,2)
   kmin=lbound(self%rho,3); kmax=ubound(self%rho,3)
   do k=kmin,kmax
      do j=jmin,jmax
         do i=imin,imax
            if (self%domain%A%mask(i,j,k) > 0) then
               self%buoy(i,j,k) = x*(self%rho(i,j,k)-rho_0)
            end  if
         end do
      end do
   end do

  return
END SUBROUTINE buoyancy_calculate

!---------------------------------------------------------------------------

SUBROUTINE density_finalize(self)
   !! Clean up the density module

   IMPLICIT NONE

!  Subroutine arguments
   class(type_density), intent(inout) :: self

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
!   call self%logs%info('density_finalize()',level=2)
   if(allocated(self%rho)) deallocate(self%rho)
   if(allocated(self%buoy)) deallocate(self%buoy)
   return
END SUBROUTINE density_finalize

#if 0
!---------------------------------------------------------------------------
!> @note
!> How to optimize this calculation - the loop order
!>
!> use gsw - either direct or via alpha and beta
!> [link](http://www.teos-10.org/pubs/gsw/html/gsw_Nsquared.html)
!> @endnote

SUBROUTINE brunt_vaisala_calculate(self,logs)
   !! Feeds your cats and dogs, if enough food is available. If not enough
   !! food is available, some of your pets will get angry.

   IMPLICIT NONE

!  Subroutine arguments
   class(type_density), intent(inout) :: self
   class(type_logging), intent(in) :: logs

!  Local constants

!  Local variables
   integer :: il, jl, kl
   real(real64), allocatable, dimension(:,:,:) :: x
!-----------------------------------------------------------------------------
   call logs%info('brunt_vaisala()',2)

   self%NN = 13._real64

  return
END SUBROUTINE brunt_vaisala_calculate
#endif

!---------------------------------------------------------------------------

END MODULE geostrophic_density
