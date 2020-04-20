! Copyright (C) 2020 Bolding & Bruggeman

!!{!./geostrophic/code/domain.md!}

MODULE geostrophic_domain

!> Sets up the calculation domain for the geostrophic model.
!> All domain and grid related quantities are set and calculated in 
!> this module like - dimensions, coordinates, metrics and related
!> meta-data.
!> For now the Arakawa A-grid is used.

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use grid_module
   use memory_manager
   use field_manager
   use logging

   IMPLICIT NONE

   PRIVATE  ! Private scope by default

!  Module constants
   integer, parameter :: halo = 0
      !! Allocate all arrays with halo zones
   real(real64), parameter :: rearth = 6378815._real64
      !! Earth radius
   real(real64), parameter :: omega =  7.2921e-5_real64
      !! Earth rotation rate
   real(real64), parameter :: pi = atan(1._real64)*4._real64
      !! \( \pi \)
   real(real64), parameter :: deg2rad = pi/180._real64
      !! degrees to radians conversion

!  Module types and variables
   type, private, extends(type_3d_grid) :: type_geostrophic_grid
      !!
      !! The Geostrophic calculation grid

      real(real64), dimension(:), allocatable :: lon
        !! Longitude from NetCDF file
      real(real64), dimension(:), allocatable :: lat
        !! Latitude from NetCDF file
      real(real64), dimension(:), allocatable :: depth
        !! Depth from NetCDF file
      real(real64), dimension(:), allocatable :: f
        !! Coriolis term
      real(real64), dimension(:), allocatable :: dx
        !! Grid spacing
      real(real64), dimension(:), allocatable :: dy
        !! Grid spacing
#if 0        
      real(real64), dimension(:,:), allocatable :: H
        !! undisturbed water depth
      real(real64), dimension(:,:), allocatable :: D
        !! total water depth - time varying
      real(real64), dimension(:,:,:), allocatable :: ho, hn
        !! layer heights - old and new time step
      real(real64), dimension(:,:,:), allocatable :: zf, zc
#endif      
      integer, dimension(:,:,:), allocatable :: mask
        !! mask=1 -> water

      contains

!      procedure :: initialize => grid_initialize
!      procedure, public :: uv_depths => grid_uv_depths
!      procedure :: print => grid_print
      procedure :: configure => grid_configure
      procedure :: print_info => grid_print_info
      procedure :: print_mask => grid_print_mask
      procedure :: report => grid_report
      procedure :: metrics => metrics
   end type type_geostrophic_grid

   TYPE, public :: type_geostrophic_domain
      logical :: domain_ready = .false.
      class(type_field_manager), pointer :: fm
      TYPE(type_geostrophic_grid) :: A
         !! A-grid

      contains

      procedure :: configure => domain_configure
      procedure :: initialize => domain_initialize
      procedure :: report => domain_report
      procedure :: allocate => allocate_variables

   end type type_geostrophic_domain

!-----------------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------------

SUBROUTINE domain_configure(self,logs,fm,imin,imax,jmin,jmax,kmin,kmax)
   !! Configure the domain 

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_domain), intent(inout) :: self
   class(type_logging), intent(in) :: logs
   TYPE(type_field_manager), intent(inout), target :: fm
   integer, intent(in), optional :: imin,imax
   integer, intent(in), optional :: jmin,jmax
   integer, intent(in), optional :: kmin,kmax
!KB   integer, intent(in), dimension(3), optional :: halo

!  Local constants

!  Local variables
!-----------------------------------------------------------------------
   call logs%info('domain_configure()',level=1)
   self%fm => fm
   call self%A%type_3d_grid%create(imin=imin,imax=imax,jmin=jmin,jmax=jmax,kmin=kmin,kmax=kmax)
   call self%allocate()
   return
END SUBROUTINE domain_configure

!-----------------------------------------------------------------------------

SUBROUTINE domain_initialize(self)
   !! Configure the domain 

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_domain), intent(inout) :: self

!  Local constants

!  Local variables
!-----------------------------------------------------------------------
!   call self%logs%info('domain_initialize()',level=1)
   if (self%A%grid_ready) then
!      call self%A%metrics()
   else
      stop 'grid is not ready'
   end if
   call self%fm%register('lon', 'degrees_east', 'longitude', &
                      standard_name='longitude', &
                      dimensions=(/id_dim_lon/), &
                      no_default_dimensions=.true., &
                      category='domain')
   call self%fm%register('lat', 'degrees_north', 'latitude', &
                      standard_name='latitude', &
                      dimensions=(/id_dim_lat/), &
                      no_default_dimensions=.true., &
                      category='domain')
   call self%fm%register('depth', 'm', 'depth', &
                      standard_name='depth', &
                      dimensions=(/id_dim_z/), &
                      no_default_dimensions=.true., &
                      category='domain')
   call self%fm%register('f', 's-1', 'Coriolis', &
                      standard_name='Coriolis', &
                      dimensions=(/id_dim_lat/), &
                      no_default_dimensions=.true., &
                      category='domain')
   self%domain_ready = self%A%grid_ready
   return
END SUBROUTINE domain_initialize

!-----------------------------------------------------------------------------

SUBROUTINE domain_report(self,logs)
   !! Report the configured domain

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_domain), intent(in) :: self
   class(type_logging), intent(in) :: logs

!  Local constants
   integer, parameter :: aunit=150

!  Local variables
!-----------------------------------------------------------------------
   call logs%info('domain_report()',level=1)
   if (.not. self%domain_ready) then
      write(*,*) 'domain is not - fully - configured yet'
   else
      call self%A%report(logs,aunit,'A-grid info: ')
   end if
   return
END SUBROUTINE domain_report

!-----------------------------------------------------------------------------

SUBROUTINE allocate_variables(self)
   !! Allocate all domain related variables

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_domain), intent(inout) :: self

!  Local constants

!  Local variables
!-----------------------------------------------------------------------------
   ! A-grid
   call allocate_grid_variables(self%A)
   return
END SUBROUTINE allocate_variables

!-----------------------------------------------------------------------------

SUBROUTINE allocate_grid_variables(self)
   !! Allocate all domain related variables

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_grid), intent(inout) :: self

!  Local constants

!  Local variables
   integer :: stat
!-----------------------------------------------------------------------------
   call mm_s('lon',self%lon,self%l(1),self%u(1),stat=stat)
   call mm_s('lat',self%lat,self%l(2),self%u(2),stat=stat)
   call mm_s('depth',self%depth,self%l(3),self%u(3),stat=stat)
   call mm_s('f',self%f,self%lat,def=-9999._real64,stat=stat)
   call mm_s('dx',self%dx,self%lat,def=-9999._real64,stat=stat)
   call mm_s('dy',self%dy,self%lat,def=-9999._real64,stat=stat)
   call mm_s('mask',self%mask,self%l(1:3),self%u(1:3),def=0,stat=stat)
   return
END SUBROUTINE allocate_grid_variables

!-----------------------------------------------------------------------------

SUBROUTINE metrics(self)
   !! Calculate grid metrics 

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_grid), intent(inout) :: self

!  Local constants

!  Local variables
   real(real64) :: dlon,dlat
!-----------------------------------------------------------------------
!KB   call self%logs%info('metrics()',level=3)
   dlon = (self%lon(self%u(1))-self%lon(self%l(1)))/(size(self%lon)-1)
   dlat = (self%lat(self%u(2))-self%lat(self%l(2)))/(size(self%lat)-1)
   self%dx = deg2rad*dlon*rearth*cos(deg2rad*self%lat)
   self%dy = deg2rad*dlat*rearth
   self%f = 2._real64*omega*sin(deg2rad*self%lat)
   return
END SUBROUTINE metrics

!-----------------------------------------------------------------------------

SUBROUTINE grid_configure(self,logs,imin,imax,jmin,jmax,kmin,kmax,halo)
   !! Configure the type_grid 

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_grid), intent(inout) :: self
   class(type_logging), intent(in) :: logs
   integer, intent(in), optional :: imin,imax
   integer, intent(in), optional :: jmin,jmax
   integer, intent(in), optional :: kmin,kmax
   integer, intent(in), dimension(3), optional :: halo
      !! grid dim in case of dynamic memory allocation

!  Local constants

!  Local variables
!-----------------------------------------------------------------------
   call logs%info('grid_configure()',level=2)
   call self%type_3d_grid%create(imin=imin,imax=imax,jmin=jmin,jmax=jmax,kmin=kmin,kmax=kmax,halo=halo)
   return
END SUBROUTINE grid_configure

!-----------------------------------------------------------------------------

SUBROUTINE grid_report(self,logs,unit,header)
   !! Report the configured grid

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_grid), intent(in) :: self
   class(type_logging), intent(in) :: logs
   integer, intent(in) :: unit
   character(len=*), intent(in) :: header

!  Local constants

!  Local variables
!-----------------------------------------------------------------------
   call logs%info('grid_report()',level=2)
   call logs%costum(unit,trim(header))
   call self%print(unit)
!   call logs%costum(unit,'mask')
!   call self%print_mask(unit)
!KB   call logs%costum(unit,'')
   return
END SUBROUTINE grid_report

!-----------------------------------------------------------------------------

SUBROUTINE grid_print_info(self,logs)
   class(type_geostrophic_grid), intent(in) :: self
   class(type_logging), intent(in) :: logs

   character(len=16) :: str

   call self%type_3d_grid%print(6)
   return
END SUBROUTINE grid_print_info

!-----------------------------------------------------------------------------

SUBROUTINE grid_print_mask(self,unit)

  IMPLICIT NONE

! Subroutine arguments
   class(type_geostrophic_grid), intent(in) :: self
   integer, intent(in) :: unit

!  Local constants

!  Local variables
   integer :: i,j
!-----------------------------------------------------------------------------
#if 0
   do j=jmax+HALO,jmin-HALO,-1
!      write(0,'(5000(i1,1x))') (mask(i,j), i=imin,imax)
      write(unit,'(5000(i1))') (mask(i,j), i=imin-HALO,imax+HALO,1)
   end do
#else
   do j=self%u(2)-halo,self%l(2)+halo,-1
!      write(0,'(5000(i1,1x))') (mask(i,j), i=imin,imax)
      write(unit,'(5000(i1))') (self%mask(i,j,1), i=self%l(1)+halo,self%u(1)-halo)
   end do
#endif
   return
END SUBROUTINE grid_print_mask

!---------------------------------------------------------------------------

END MODULE geostrophic_domain
