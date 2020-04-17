! Copyright (C) 2020 Bolding & Bruggeman
!! This module provides routines for converting back and forth between
!! calendar dates and [Julian days](https://en.wikipedia.org/wiki/Julian_day).
!! Relies on the datetime module for all calculations.
!! 
!-----------------------------------------------------------------------------

MODULE julian_days

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use datetime_module

   IMPLICIT NONE

   PRIVATE  ! Private scope by default

!  Module constants
   integer, parameter :: rjd=2400000
     !! Reduced Julian day - 12h Nov 16, 1858
   real(real64), parameter :: mjd=2400000.5_real64
     !! Modified Julian day - 0h Nov 17, 1858
   real(real64), parameter :: tjd=2440000.5_real64
     !! Truncated Julian day - 0h May 24, 1968
   real(real64), parameter :: djd=2415020_real64
     !! Dublin Julian day - 12h Dec 31, 1899

!  Module variables
   type(datetime) :: epoch, year1

! Public members
  PUBLIC :: init_epoch,julian_day,calendar_date

  interface julian_day
     module procedure julian_day_dt
     module procedure julian_day_date
  end interface

  interface calendar_date
     module procedure calendar_date_dt
     module procedure calendar_date_date
  end interface

!-----------------------------------------------------------------------------

CONTAINS
  
!-----------------------------------------------------------------------------

SUBROUTINE init_epoch()
   !! Initialize the epoch for calculating time differences.
   !! For now - use Reduced Julian day

   IMPLICIT NONE

!  Subroutine arguments

!  Local constants

!  Local variables
!-----------------------------------------------------------------------------
   epoch = datetime(1858,11,17,0)
   year1 = datetime(1,1,1,0)
!KB   write(*,*) year1%isoformat()
   return
END SUBROUTINE init_epoch

!-----------------------------------------------------------------------------

ELEMENTAL SUBROUTINE julian_day_dt(t,julian)
   !! Return the julian day based on datetime variable

   IMPLICIT NONE

!  Subroutine arguments
   type(datetime), intent(in) :: t
   integer, intent(out)  :: julian

!  Local constants

!  Local variables
   type(timedelta) :: dt
!-----------------------------------------------------------------------------
   dt = t-epoch
   julian = nint(dt%total_seconds()/86400)+rjd
   return
END SUBROUTINE julian_day_dt

!-----------------------------------------------------------------------------

ELEMENTAL SUBROUTINE julian_day_date(yyyy,mm,dd,julian)
   !! Return the julian day based on year, month and day

   IMPLICIT NONE

!  Subroutine arguments
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
END SUBROUTINE julian_day_date

!-----------------------------------------------------------------------------

ELEMENTAL SUBROUTINE calendar_date_dt(julian,t)
   !! Return datetime object for the Julian day

   IMPLICIT NONE

!  Subroutine arguments
   integer, intent(in)  :: julian
   type(datetime), intent(out) :: t

!  Local constants

!  Local variables
!-----------------------------------------------------------------------------
   t=epoch+timedelta(days=julian-rjd)
   return
END SUBROUTINE calendar_date_dt

!-----------------------------------------------------------------------------

ELEMENTAL SUBROUTINE calendar_date_date(julian,yyyy,mm,dd)
   !! Return year, month and day for the julian day

   IMPLICIT NONE

!  Subroutine arguments
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
END SUBROUTINE calendar_date_date
  
!-----------------------------------------------------------------------------

END MODULE julian_days

