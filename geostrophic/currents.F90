! Copyright (C) 2020 Bolding & Bruggeman

!!{!./example/code/currents.md!}

MODULE geostrophic_currents

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use memory_manager
   use logging
   use field_manager
   use geostrophic_domain

   IMPLICIT NONE

   PRIVATE  ! Private scope by default

!  Module constants

!  Module types and variables
   type, public :: type_geostrophic_currents

      class(type_logging), pointer :: logs
      class(type_field_manager), pointer :: fm
      class(type_geostrophic_domain), pointer :: domain

      real(real64), dimension(:,:,:), allocatable :: ug, vg
      real(real64), dimension(:,:), allocatable :: U, V

      contains

      procedure :: configure => currents_configure
      procedure :: initialize => currents_initialize
      procedure :: update => currents_update
      procedure :: transports => transports

   end type type_geostrophic_currents

!---------------------------------------------------------------------------

CONTAINS

!---------------------------------------------------------------------------

SUBROUTINE currents_configure(self,logs,fm)

   !! Configure the components belonging to the dynamics

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_currents), intent(inout) :: self
   class(type_logging), intent(in), target :: logs
   class(type_field_manager), intent(inout), target :: fm

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
   self%logs => logs
   call logs%info('velocity_configure()',level=2)
   self%fm => fm
   return
END SUBROUTINE currents_configure

!---------------------------------------------------------------------------

SUBROUTINE currents_initialize(self,domain)

   !! Initialize all dynamical components

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_currents), intent(inout) :: self
   class(type_geostrophic_domain), intent(in), target :: domain

!  Local constants

!  Local variables
   integer :: stat
!---------------------------------------------------------------------------
   call self%logs%info('currents_initialize()',level=2)
   self%domain => domain
   call mm_s('ug',self%ug,self%domain%A%l,self%domain%A%u,def=0._real64,stat=stat)
   call mm_s('vg',self%vg,self%domain%A%l,self%domain%A%u,def=0._real64,stat=stat)
   call mm_s('U',self%U,self%domain%A%l(1:2),self%domain%A%u(1:2),def=0._real64,stat=stat)
   call mm_s('V',self%V,self%domain%A%l(1:2),self%domain%A%u(1:2),def=0._real64,stat=stat)
   call self%fm%register('ug', 'm/s', 'zonal velocity', &
                  standard_name='baroclinic_eastward_sea_water_velocity', &
                  dimensions=(/id_dim_z/), &
                  category='velocities', &
                  fill_value=-0._real64, &
                  part_of_state=.true.)
   call self%fm%send_data('ug', self%ug)
   call self%fm%register('vg', 'm/s', 'meridional velocity', &
                  standard_name='baroclinic_northward_sea_water_velocity', &
                  dimensions=(/id_dim_z/), &
                  category='velocities', &
                  fill_value=-0._real64, &
                  part_of_state=.true.)
   call self%fm%send_data('vg', self%vg)
   call self%fm%register('U', 'm3/s', 'zonal transport', &
                  standard_name='baroclinic_eastward_sea_water_velocity', &
                  category='velocities', &
                  fill_value=0._real64)
   call self%fm%send_data('U', self%U)
   call self%fm%register('V', 'm3/s', 'meridional transport', &
                  standard_name='baroclinic_northward_sea_water_velocity', &
                  category='velocities', &
                  fill_value=0._real64)
   call self%fm%send_data('V', self%V)
   return
END SUBROUTINE currents_initialize

!---------------------------------------------------------------------------

SUBROUTINE currents_update(self,rho,idpdx,idpdy)
   !! update the geostrophic currents based on updated density and pressure
   !! gradients
   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_currents), intent(inout) :: self
      !! Geostrophic currents
   real(real64), dimension(:,:,:), intent(in) :: rho
      !! density
   real(real64), dimension(:,:,:), intent(in) :: idpdx
      !! internal pressure gradient - x-direction
   real(real64), dimension(:,:,:), intent(in) :: idpdy
      !! internal pressure gradient - y-direction

!  Local constants

!  Local variables
   integer :: i,j,k
!---------------------------------------------------------------------------
   call self%logs%info('currents_update()',level=3)
   do k=self%domain%A%l(3),self%domain%A%u(3)
      do j=self%domain%A%l(2),self%domain%A%u(2)
         do i=self%domain%A%l(1),self%domain%A%u(1)
            if (self%domain%A%mask(i,j,k) == 1) then
               if (abs(self%domain%A%lat(j)) .gt. 1._real64) then
                  self%ug(i,j,k)=-1._real64/(self%domain%A%f(j)*rho(i,j,k))*idpdy(i,j,k)
                  self%vg(i,j,k)= 1._real64/(self%domain%A%f(j)*rho(i,j,k))*idpdx(i,j,k)
               else
                  self%ug(i,j,k)=0._real64
                  self%vg(i,j,k)=0._real64
               end if
            end if
         end do
      end do
   end do
   return
END SUBROUTINE currents_update

!---------------------------------------------------------------------------

SUBROUTINE transports(self)
   !! update the geostrophic currents based on updated density and pressure
   !! gradients
   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_currents), intent(inout) :: self
      !! Geostrophic currents

!  Local constants

!  Local variables
   integer :: i,j,k
   real(real64) :: dz
!---------------------------------------------------------------------------
   call self%logs%info('transports_update()',level=3)
   do j=self%domain%A%l(2),self%domain%A%u(2)
      do i=self%domain%A%l(1),self%domain%A%u(1)
         if (self%domain%A%mask(i,j,1) == 1) then
            self%U(i,j) = 0._real64
            self%V(i,j) = 0._real64
         end if
      end do
   end do
   do k=self%domain%A%l(3)+1,self%domain%A%u(3)
      do j=self%domain%A%l(2),self%domain%A%u(2)
         do i=self%domain%A%l(1),self%domain%A%u(1)
            if (self%domain%A%mask(i,j,k) == 1) then
               dz = (self%domain%A%depth(k-1)+self%domain%A%depth(k))/2._real64
               self%U(i,j)=self%U(i,j)+self%domain%A%dx(j)*dz*self%ug(i,j,k)
               self%V(i,j)=self%V(i,j)+self%domain%A%dy(j)*dz*self%vg(i,j,k)
            end if
         end do
      end do
   end do
   return
END SUBROUTINE transports

!---------------------------------------------------------------------------

END MODULE geostrophic_currents
