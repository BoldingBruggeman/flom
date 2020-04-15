! Copyright (C) 2020 Bolding & Bruggeman

!!{!./geostrophic/code/pressure.md!}

MODULE geostrophic_pressure

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use memory_manager
   use logging
   use field_manager
   use geostrophic_domain

   IMPLICIT NONE

   PRIVATE  ! Private scope by default

!  Module constants

!  Module types and variables
   type, public :: type_geostrophic_pressure

      class(type_logging), pointer :: logs
      class(type_field_manager), pointer :: fm
      class(type_geostrophic_domain), pointer :: domain

!KB      real(real64), dimension(:,:), allocatable :: dpdx, dpdy
      real(real64), dimension(:,:,:), allocatable :: idpdx, idpdy

      contains

      procedure :: configure => pressure_configure
      procedure :: initialize => pressure_initialize
!KB      procedure :: surface => pressure_surface
      procedure :: internal => pressure_internal

   end type type_geostrophic_pressure

!---------------------------------------------------------------------------

CONTAINS

!---------------------------------------------------------------------------

SUBROUTINE pressure_configure(self,logs,fm)
   !! Configure pressure components

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_pressure), intent(inout) :: self
   class(type_logging), intent(in), target :: logs
   class(type_field_manager), target:: fm

!  Local constants

!  Local variables
!---------------------------------------------------------------------------
   self%logs => logs
   call logs%info('pressure_configure()',level=2)
   self%fm => fm
   return
END SUBROUTINE pressure_configure

!---------------------------------------------------------------------------

SUBROUTINE pressure_initialize(self,domain)
   !! Initialize pressure components

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_pressure), intent(inout) :: self
   class(type_geostrophic_domain), intent(in), target :: domain

!  Local constants

!  Local variables
   integer :: stat
!---------------------------------------------------------------------------
   call self%logs%info('pressure_initialize()',level=2)
   self%domain => domain
!KB   call mm_s('dpdx',self%dpdx,domain%l(1:2),domain%u(1:2),def=0._real64)
!KB   call mm_s('dpdy',self%dpdy,domain%l(1:2),domain%u(1:2),def=0._real64)
   call mm_s('idpdx',self%idpdx,self%domain%A%l(1:3),self%domain%A%u(1:3),def=-9999._real64,stat=stat)
   call mm_s('idpdy',self%idpdy,self%domain%A%l(1:3),self%domain%A%u(1:3),def=-9999._real64,stat=stat)

   call self%fm%register('idpdx', 'm2/s2', 'baroclinic pressure gradient - x', &
                         standard_name='', &
                         dimensions=(/id_dim_z/), &
                         category='baroclinic', &
                         fill_value=-9999._real64)
     !                    , output_level=output_level_debug)
   call self%fm%send_data('idpdx', self%idpdx)

   call self%fm%register('idpdy', 'm2/s2', 'baroclinic pressure gradient - y', &
                         standard_name='', &
                         dimensions=(/id_dim_z/), &
                         category='baroclinic', &
                         fill_value=-9999._real64)
     !                    , output_level=output_level_debug)
   call self%fm%send_data('idpdy', self%idpdy)

   return
END SUBROUTINE pressure_initialize

#if 0
!---------------------------------------------------------------------------

SUBROUTINE pressure_surface(self,z,sp)

   !! Surface pressure gradients

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_pressure), intent(inout) :: self
   real(rk), dimension(:,:), intent(in) :: z,sp

!  Local constants

!  Local variables
   real(real64) :: zp,zm
!KB
   real(real64) :: gammai= 10., min_depth=0.5_real64
   integer :: i,j
!---------------------------------------------------------------------------
   call logs%info('pressure_external()',level=2)
   do j=self%domain%U%l(2),self%domain%U%u(2)
      do i=self%domain%U%l(1),self%domain%U%u(1)
         if (self%domain%U%mask(i,j) == 1 .or. self%domain%U%mask(i,j) == 2) then
            zp = max( z(i+1,j) , -self%domain%S%H(i  ,j)+min( min_depth , self%domain%S%D(i+1,j) ) )
            zm = max( z(i  ,j) , -self%domain%S%H(i+1,j)+min( min_depth , self%domain%S%D(i  ,j) ) )
            self%dpdx(i,j) = ( zp - zm + (sp(i+1,j)-sp(i,j))*gammai ) / self%domain%U%dx(i,j)
         end if
      end do      
   end do   
   do j=self%domain%V%l(2),self%domain%V%u(2)
      do i=self%domain%V%l(1),self%domain%V%u(1)
         if (self%domain%V%mask(i,j,1) == 1 .or. self%domain%V%mask(i,j,1) == 2) then
            zp = max( z(i,j+1) , -self%domain%S%H(i  ,j)+min( min_depth , self%domain%S%D(i,j+1) ) )
            zm = max( z(i,j  ) , -self%domain%S%H(i,j+1)+min( min_depth , self%domain%S%D(i,j  ) ) )
            self%dpdy(i,j) = ( zp - zm + (sp(i,j+1)-sp(i,j))*gammai ) / self%domain%V%dy(i,j)
         end if
      end do      
   end do   
   return
END SUBROUTINE pressure_surface
#endif

!---------------------------------------------------------------------------
!> Documentation - including Latex equations
!> HB
!> use !> as line start

SUBROUTINE pressure_internal(self,buoy)
   !! Interlnal pressure gradient

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_pressure), intent(inout) :: self
   real(real64), dimension(:,:,:), intent(in) :: buoy

!  Local constants

!  Local variables
   integer :: i,j,k
!---------------------------------------------------------------------------
   call self%logs%info('pressure_internal()',level=3)
   k=self%domain%A%l(3)
   do j=self%domain%A%l(2),self%domain%A%u(2)
      do i=self%domain%A%l(1),self%domain%A%u(1)
         if (self%domain%A%mask(i,j,k) == 1) then
            self%idpdx(i,j,k) = 0._real64
            self%idpdy(i,j,k) = 0._real64
         end if
      end do      
   end do   
   ! x
   do k=self%domain%A%l(3)+1,self%domain%A%u(3)
      do j=self%domain%A%l(2),self%domain%A%u(2)
         do i=self%domain%A%l(1)+1,self%domain%A%u(1)-1
            if (self%domain%A%mask(i,j,k) == 1) then
               self%idpdx(i,j,k) = 0._real64
               if (self%domain%A%mask(i-1,j,k) == 1 .and. self%domain%A%mask(i+1,j,k) == 1) then
                  self%idpdx(i,j,k) = self%idpdx(i,j,k-1) &
                             +(buoy(i+1,j,k)-buoy(i-1,j,k)) &
                             /(self%domain%A%dx(j)+self%domain%A%dx(j))*self%domain%A%depth(k)
                  cycle
               end if
               if (self%domain%A%mask(i-1,j,k) == 0 .and. self%domain%A%mask(i+1,j,k) == 1) then
                  self%idpdx(i,j,k) = self%idpdx(i,j,k-1) &
                             +(buoy(i+1,j,k)-buoy(i,j,k)) &
                             /(self%domain%A%dx(j))*self%domain%A%depth(k)
                  cycle
               end if
               if (self%domain%A%mask(i-1,j,k) == 1 .and. self%domain%A%mask(i+1,j,k) == 0) then
                  self%idpdx(i,j,k) = self%idpdx(i,j,k-1) &
                             +(buoy(i,j,k)-buoy(i-1,j,k)) &
                             /(self%domain%A%dx(j))*self%domain%A%depth(k)
                  cycle
               end if
            end if
         end do      
      end do   
   end do   
   i=self%domain%A%l(1)
   where (self%domain%A%mask(i,:,:) == 1) self%idpdx(i,:,:) = self%idpdx(i+1,:,:)
   where (self%domain%A%mask(i,:,:) == 1) self%idpdx(i,:,:) = 0._real64
   i=self%domain%A%u(1)
   where (self%domain%A%mask(i,:,:) == 1) self%idpdx(i,:,:) = self%idpdx(i-1,:,:)
   where (self%domain%A%mask(i,:,:) == 1) self%idpdx(i,:,:) = 0._real64
   ! y
   do k=self%domain%A%l(3)+1,self%domain%A%u(3)
      do j=self%domain%A%l(2)+1,self%domain%A%u(2)-1
         do i=self%domain%A%l(1),self%domain%A%u(1)
            if (self%domain%A%mask(i,j,k) == 1) then
               self%idpdy(i,j,k) = 0._real64
               if (self%domain%A%mask(i,j-1,k) == 1 .and. self%domain%A%mask(i,j+1,k) == 1) then
                  self%idpdy(i,j,k) = self%idpdy(i,j,k-1) &
                             +(buoy(i,j+1,k)-buoy(i,j-1,k)) &
                             /(self%domain%A%dy(j)+self%domain%A%dy(j))*self%domain%A%depth(k)
                  cycle
               end if
               if (self%domain%A%mask(i,j-1,k) == 0 .and. self%domain%A%mask(i,j+1,k) == 1) then
                  self%idpdy(i,j,k) = self%idpdy(i,j,k-1) &
                             +(buoy(i,j+1,k)-buoy(i,j,k)) &
                             /(self%domain%A%dy(j))*self%domain%A%depth(k)
                  cycle
               end if
               if (self%domain%A%mask(i,j-1,k) == 1 .and. self%domain%A%mask(i,j+1,k) == 0) then
                  self%idpdy(i,j,k) = self%idpdy(i,j,k-1) &
                             +(buoy(i,j,k)-buoy(i,j-1,k)) &
                             /(self%domain%A%dy(j))*self%domain%A%depth(k)
                  cycle
               end if
            end if
         end do      
      end do   
   end do   
   j=self%domain%A%l(2)
   where (self%domain%A%mask(:,j,:) == 1) self%idpdy(:,j,:) = self%idpdx(:,j+1,:)
   where (self%domain%A%mask(:,j,:) == 1) self%idpdy(:,j,:) = 0._real64
   j=self%domain%A%u(2)
   where (self%domain%A%mask(:,j,:) == 1) self%idpdy(:,j,:) = self%idpdx(:,j-1,:)
   where (self%domain%A%mask(:,j,:) == 1) self%idpdy(:,j,:) = 0._real64
   return
END SUBROUTINE pressure_internal

!---------------------------------------------------------------------------

END MODULE geostrophic_pressure
