! Copyright (C) 2020 Bolding & Bruggeman

!!{!./example/code/output.md!}

MODULE geostrophic_output

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use datetime_module
   use logging
   use field_manager
   use output_manager_core, only:output_manager_host=>host, type_output_manager_host=>type_host
   use output_manager

   IMPLICIT NONE

!-----------------------------------------------------------------------------

   PRIVATE  ! Private scope by default

!  Module constants
   integer, parameter :: rjd=2400000
     !! Modified Julian Day - 0h Nov 16, 1858

!  Module types and variables
   type(datetime) :: epoch
      !! used as reference time for Julian Days calculations

   type, public, extends(type_output_manager_host) :: type_geostrophic_output
      class(type_logging), pointer :: logs
      class(type_field_manager), pointer :: fm
   contains
      procedure :: configure => configure_output
      procedure :: initialize => initialize_output
      procedure :: do_output => do_output
      procedure :: julian_day => geostrophic_julian_day
      procedure :: calendar_date => geostrophic_calendar_date
   end type type_geostrophic_output

!-----------------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------------

SUBROUTINE configure_output(self,logs)
   !! Configure the geostrophic adapted output_manager

   IMPLICIT NONE

! Subroutine arguments
   class(type_geostrophic_output), intent(inout) :: self
   class(type_logging), intent(in), target :: logs

! Local constants

! Local variables
   integer :: n
!-----------------------------------------------------------------------------
   self%logs => logs
   call self%logs%info('output_configure()',level=1)
   epoch = datetime(1858,11,17,0)
   return
END SUBROUTINE configure_output

!-----------------------------------------------------------------------------

SUBROUTINE initialize_output(self,fm)
   !! Initialize the geostrophic adapted output_manager

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_output), intent(inout) :: self
   class(type_field_manager), intent(in), target :: fm

!  Local constants

!  Local variables
   character(len=128) :: title='geostrophic'
!-----------------------------------------------------------------------------
   call self%logs%info('output_initialize()',level=1)
   self%fm => fm
   allocate(type_geostrophic_output::output_manager_host)
!   call self%fm%list()
   call output_manager_init(fm,title)
   return
END SUBROUTINE initialize_output

!-----------------------------------------------------------------------------

SUBROUTINE do_output(self,t)
   !! Save data to NetCDF files

   IMPLICIT NONE

! Subroutine arguments
   class(type_geostrophic_output), intent(inout) :: self
   type(datetime), intent(in) :: t

! Local constants

! Local variables
  integer :: jd,n=1
!-----------------------------------------------------------------------------
   call self%logs%info('do_output()',level=3)
   call self%julian_day(t%getYear(),t%getMonth(),t%getDay(),jd)
   call output_manager_save(jd,0,n)
!   write(*,*) t%isoformat(),' ',jd
   n=n+1
   return
END SUBROUTINE do_output

!-----------------------------------------------------------------------------

SUBROUTINE geostrophic_julian_day(self,yyyy,mm,dd,julian)
   !! Return the julian day based on year, month and day

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_output), intent(in) :: self
   integer, intent(in) :: yyyy,mm,dd
   integer, intent(out)  :: julian

!  Local constants

!  Local variables
   type(datetime) :: t
   type(timedelta) :: dt
!-----------------------------------------------------------------------------
   t = datetime(yyyy,mm,dd)
   dt = t-epoch
   julian = nint(dt%total_seconds()/86400)+rjd
   return
END SUBROUTINE geostrophic_julian_day

!-----------------------------------------------------------------------------

SUBROUTINE geostrophic_calendar_date(self,julian,yyyy,mm,dd)
   !! Return year, month and day for the julian day

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_output), intent(in) :: self
   integer, intent(in)  :: julian
   integer, intent(out) :: yyyy,mm,dd

!  Local constants

!  Local variables
   type(datetime) :: t
!-----------------------------------------------------------------------------
   t=epoch+timedelta(days=julian-rjd)
   yyyy=t%getYear()
   mm=t%getMonth()
   dd=t%getDay()
   return
END SUBROUTINE geostrophic_calendar_date

#if 0
!-----------------------------------------------------------------------------

SUBROUTINE close_output(self)

   IMPLICIT NONE

!  Subroutine arguments
   class(type_geostrophic_output), intent(inout) :: self

!  Local constants

!  Local variables
!-----------------------------------------------------------------------------
   return
END SUBROUTINE close_output
#endif

!-----------------------------------------------------------------------------

END MODULE geostrophic_output
