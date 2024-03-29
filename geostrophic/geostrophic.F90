! Copyright (C) 2020 Bolding & Bruggeman

!!{!./geostrophic/code/geostrophic.md!}

!! https://www.mdpi.com/2072-4292/10/5/715/htm

MODULE geostrophic_model

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use datetime_module, only: datetime, timedelta, clock, strptime
   use logging
   use field_manager
   use memory_manager
   use geostrophic_input
   use geostrophic_output
   use geostrophic_domain
   use geostrophic_physics
   use geostrophic_dynamics

   use gsw_mod_toolbox

   IMPLICIT NONE

   PRIVATE  ! Private scope by default

!  Module constants

!  Module types and variables
   type, public :: type_geostrophic_model
      !! Geostrophic model implemeted using FLOM

      character(len=256) :: name = "geostrophic"
      TYPE(type_logging) :: logs
      TYPE(type_field_manager) :: fm
      TYPE(type_geostrophic_input) :: input
      TYPE(type_geostrophic_output) :: output
      TYPE(type_geostrophic_domain) :: domain
      TYPE(type_geostrophic_physics) :: physics
      TYPE(type_geostrophic_dynamics) :: dynamics

      integer, dimension(4) :: start,count

      contains

      procedure :: settings => geostrophic_settings
      procedure :: configure => geostrophic_configure
      procedure :: initialize => geostrophic_initialize
      procedure :: integrate => geostrophic_integrate
      procedure :: finalize => geostrophic_finalize

   end type type_geostrophic_model

CONTAINS

!---------------------------------------------------------------------------

SUBROUTINE geostrophic_settings(self)

   !! Configure all component of the model

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_model) :: self

!  Local constants
   character(len=256), parameter :: time_format='%Y-%m-%d %H:%M:%S'

!  Local variables
   integer :: stat
!---------------------------------------------------------------------------
   self%logs%prepend = ''
   call self%logs%info('geostrophic_settings()')
   CALL get_environment_variable("WOA_DIR", self%input%datadir, status=stat, trim_name=.true.)
   if (stat == 1) then
      write(*,*) 'Need to set WOA_DIR to the folder where WOA data are stored'
      stop 1
   end if
   call self%logs%info('World Ocean Atlas data:',level=1)
   call self%logs%info('data directory: ',msg2=trim(self%input%datadir),level=2)
   CALL get_environment_variable("WOA_RESOLUTION", self%input%resolution, status=stat)
   if (stat == 0) then
      select case (self%input%resolution(1:1))
         case ('l')
            self%input%a = '5d'
            call self%logs%info('resolution: ',msg2='l(ow)',level=2)
         case ('m')
            self%input%a = '01'
            call self%logs%info('resolution: ',msg2='m(edium)',level=2)
         case ('h')
            self%input%a = '04'
            call self%logs%info('resolution: ',msg2='h(igh)',level=2)
      end select
   else
      self%input%resolution(1:1) = 'm'
      self%input%a = '01'
      call self%logs%info('resolution: ',msg2='m(edium)',level=2)
   end if
   return
END SUBROUTINE geostrophic_settings

!---------------------------------------------------------------------------

SUBROUTINE geostrophic_configure(self)
   !! Configure all components of the model

   IMPLICIT NONE

! Subroutine arguments
   class(type_geostrophic_model) :: self

! Local constants

! Local variables
   integer :: length
   integer :: imin,imax,jmin,jmax,kmin,kmax
   integer :: lon0,lon1,lat0,lat1
!-----------------------------------------------------------------------------
   self%logs%prepend = ''
   call self%logs%info('geostrophic_configure()')

#if 1
   select case (self%input%resolution(1:1))
      case ('l')
         self%start=(/1, 1, 1, 1/)
         self%count=(/72, 36 ,57, 1/)
         stop 'low resolution WOA does not work yet - in-complete S and T'
      case ('m')
         self%start=(/1, 1 ,1, 1/)
         self%count=(/360, 180 ,57, 1/)
      case ('h')
         self%start=(/1, 1 ,1, 1/)
         self%count=(/1440, 720 ,57, 1/)
   end select
   imin=1; imax=self%count(1)
   jmin=1; jmax=self%count(2)
   kmin=1; kmax=self%count(3)
#else
   if (command_argument_count() .ne. 6) then
      write(*,*) 'ERROR, must be called like:'
      STOP ' ./geostrophic lon0 lat0<start> <stop> - timeformat "yyyy-mm-dd hh:mi:ss"'
   end if
#endif

   call self%input%configure(self%logs,start=self%start,count=self%count)

   call self%domain%configure(self%logs,self%fm, &
                              imin=imin,imax=imax, &
                              jmin=jmin,jmax=jmax, &
                              kmin=kmin,kmax=kmax)
   ! now we can configure the field_manager
   call self%fm%register_dimension('lon',imax-imin+1,id=id_dim_lon)
   call self%fm%register_dimension('lat',jmax-jmin+1,id=id_dim_lat)
   call self%fm%register_dimension('depth',kmax-kmin+1,id=id_dim_z)
   call self%fm%register_dimension('time',id=id_dim_time)
   call self%fm%initialize(prepend_by_default=(/id_dim_lon,id_dim_lat/),append_by_default=(/id_dim_time/))
   call self%physics%configure(self%logs,self%fm)
   call self%dynamics%configure(self%logs,self%fm)
   call self%output%configure(self%logs)
   return
END SUBROUTINE geostrophic_configure

!---------------------------------------------------------------------------

SUBROUTINE geostrophic_initialize(self)
   !! Initialize all components of the model

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_model) :: self

! Local constants

! Local variables
   integer :: jd,y,m,d
!-----------------------------------------------------------------------------
   call self%logs%info('geostrophic_initialize()')

   call self%domain%initialize()
   call self%physics%initialize(self%logs,self%domain)
   call self%dynamics%initialize(self%logs,self%domain)
   call self%input%initialize(self%domain,self%physics%salinity%sp,self%physics%temperature%T)
   call self%domain%A%metrics()
   call self%fm%send_data('lon', self%domain%A%lon)
   call self%fm%send_data('lat', self%domain%A%lat)
   call self%fm%send_data('depth', self%domain%A%depth)
   call self%fm%send_data('f', self%domain%A%f)
!   call self%domain%initialize()
!KB   call self%fm%list()
   call self%domain%report(self%logs)
   call self%output%initialize(self%fm)
   return
END SUBROUTINE geostrophic_initialize

!---------------------------------------------------------------------------

SUBROUTINE geostrophic_integrate(self)
   !! Calculate monthly  geostrophic currents and save the results

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_model) :: self

! Local constants

! Local variables
  integer :: n
  type(datetime) :: sim_time
  real(real64) :: kaj=0._real64
!-----------------------------------------------------------------------------
   call self%logs%info('geostrophic_integrate()')
   do n=1,12
      sim_time = datetime(2000, n, 15)
      call self%logs%info(sim_time % strftime('%Y-%m-%d'),level=2)
      call self%input%update(n)
      where(self%physics%temperature%t .lt. 100._real64 .and. self%physics%salinity%sp .lt. 100._real64)
         self%domain%A%mask=1
!KB         self%physics%salinity%sa = gsw_sa_from_sp(self%physics%salinity%sp,kaj,self%domain%A%lon,self%domain%A%lat)
         self%physics%salinity%sa = self%physics%salinity%sp
         self%physics%temperature%ct = gsw_ct_from_t(self%physics%salinity%sa,self%physics%temperature%t,kaj)
      else where
         self%physics%salinity%sa = -9999._real64
      end where
      call self%physics%salinity%update()
      call self%physics%temperature%update()
      call self%physics%density%calculate(self%physics%salinity%sa, &
                                          self%physics%temperature%ct )
      call self%physics%density%buoyancy()
      call self%dynamics%pressure%internal(self%physics%density%buoy)
      call self%dynamics%currents%update(self%physics%density%rho, &
                                         self%dynamics%pressure%idpdx, &
                                         self%dynamics%pressure%idpdy)
      call self%dynamics%currents%transports()
      call self%output%do_output(sim_time)
   end do
   call self%logs%info('integration done',level=1)

   return
END SUBROUTINE geostrophic_integrate

!---------------------------------------------------------------------------

SUBROUTINE geostrophic_finalize(self)
   !! Initialize all components of the model

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_model) :: self

! Local constants

! Local variables
   integer :: jd,y,m,d
!-----------------------------------------------------------------------------
   where(self%physics%temperature%T .lt. 100._real64) self%domain%A%mask=1
   write(150,*) 'field size:   ',size(self%domain%A%mask)
   write(150,*) 'water points: ',count(self%domain%A%mask .gt. 0)
   call self%domain%A%print_mask(150)
   call self%logs%info('geostrophic_finalize()')
   call self%physics%salinity%finalize()
   call self%physics%temperature%finalize()
   call self%physics%density%finalize()
   call self%logs%info('geostrophic model finished normally')
   return
END SUBROUTINE geostrophic_finalize

!---------------------------------------------------------------------------

END MODULE geostrophic_model

