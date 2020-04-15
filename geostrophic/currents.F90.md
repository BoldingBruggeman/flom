! Copyright (C) 2020 Bolding & Bruggeman

!! The geostrophic horizontal velocity field \((u_g,v_g)\) is directly
!! calculated from the pressure gradient field:
!! \begin{equation}\label{eq_ug_vg}
!! u_g=\frac{1}{f\rho_0} \partial_y p,\quad
!! v_g=-\frac{1}{f\rho_0} \partial_x p,
!! \end{equation}
!! with the hydrostatic pressure \(p\) the Coriolis parameter \(f\)
!! and the reference density \(\rho_0\)
!! When decomposing the pressure into barotropic
!! (slope of sea surface elevation \(\eta\) and gradient
!! of atmospheric pressure \(p_a\)
!! and baroclinic (density gradient)
!! contributions, the geostrophic velocity field can be expressed as
!! \begin{equation}\label{eq_ug_vg_full}
!! u_g=\frac{1}{f}\left(g \partial_y \eta +\frac{1}{\rho_0}\partial_y p_a
!! -\int_z^{\eta} \partial_y p_i\,\mbox{d}\xi\right), \quad
!! v_g=-\frac{1}{f}\left(g \partial_x \eta +\frac{1}{\rho_0}\partial_x p_a
!! -\int_z^{\eta} \partial_x p_i\,\mbox{d}\xi\right),
!! \end{equation}
!! Or using the buoyancy
!! \begin{equation}\label{def_b}
!! b=-\frac{g}{\rho_0}(\rho-\rho_0)
!! \end{equation}
!! and the gravitational acceleration \(g\), becomes
!! \begin{equation}\label{eq_ug_vg_buoy}
!! u_g=\frac{1}{f}\left(g \partial_y \eta +\frac{1}{\rho_0}\partial_y p_a
!! -\int_z^{\eta} \partial_y b\,\mbox{d}\xi\right), \quad
!! v_g=-\frac{1}{f}\left(g \partial_x \eta +\frac{1}{\rho_0}\partial_x p_a
!! -\int_z^{\eta} \partial_x b\,\mbox{d}\xi\right),
!! \end{equation}

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

      real(real64), dimension(:,:,:), allocatable :: U, V

      contains

      procedure :: configure => currents_configure
      procedure :: initialize => currents_initialize
      procedure :: update => currents_update

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
   call mm_s('U',self%U,self%domain%A%l,self%domain%A%u,def=0._real64,stat=stat)
   call mm_s('V',self%V,self%domain%A%l,self%domain%A%u,def=0._real64,stat=stat)
   call self%fm%register('U', 'm/s', 'zonal velocity', &
                  standard_name='baroclinic_eastward_sea_water_velocity', &
                  dimensions=(/id_dim_z/), &
                  category='velocities', &
                  part_of_state=.true.)
   call self%fm%send_data('U', self%U)
   call self%fm%register('V', 'm/s', 'meridional velocity', &
                  standard_name='baroclinic_northward_sea_water_velocity', &
                  dimensions=(/id_dim_z/), &
                  category='velocities', &
                  part_of_state=.true.)
   call self%fm%send_data('V', self%V)
   return
END SUBROUTINE currents_initialize

!---------------------------------------------------------------------------

!HB
!> Here, the vertically integrated \(U\)momentum equation (\ref{UMOM}) given
!> on page \pageref{UMOM} including a
!> number of slow terms is calculated. One slight modification is that
!> for better stability of drying and flooding processes the slow friction
!> term \(S^x_F\) is now also multiplied with the parameter \(alpha\) defined
!> in eq.\ (\ref{alpha}).

SUBROUTINE currents_update(self,rho,idpdx,idpdy)
  !! \begin{equation}\label{eq_ug_vg_full}
  !! u_g=\frac{1}{f}\left(g \partial_y \eta +\frac{1}{\rho_0}\partial_y p_a
  !! -\int_z^{\eta} \partial_y b\,\mbox{d}\xi\right), \quad
  !! v_g=-\frac{1}{f}\left(g \partial_x \eta +\frac{1}{\rho_0}\partial_x p_a
  !! -\int_z^{\eta} \partial_x b\,\mbox{d}\xi\right),
  !! \end{equation}
  !! with the buoyancy
  !! \begin{equation}\label{def_b}
  !! b=-\frac{g}{\rho_0}(\rho-\rho_0)
  !! \end{equation}
  !! and the gravitational acceleration \(g\)

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
               self%U(i,j,k)=1._real64/(self%domain%A%f(j)*rho(i,j,k))*idpdx(i,j,k)
               self%V(i,j,k)=1._real64/(self%domain%A%f(j)*rho(i,j,k))*idpdy(i,j,k)
            end if
         end do
      end do
   end do
   return
END SUBROUTINE currents_update

!---------------------------------------------------------------------------

END MODULE geostrophic_currents
