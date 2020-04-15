! Copyright (C) 2020 Bolding & Bruggeman
!> Test program showing a very basic time loop.
!> 
!> The program uses the 
!> [datetime](https://github.com/wavebitscientific/datetime-fortran)
!> package by Milan Curcic.

PROGRAM test_julian

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use datetime_module
!   use geostrophic_output

   IMPLICIT NONE

!  Local constants
   integer, parameter :: rjd=2400000

!  Local variables
   integer :: jd=0,n
   type(datetime) :: epoch,t
   type(timedelta) :: dt
!---------------------------------------------------------------------------
   epoch = datetime(1858,11,17,0)
   call julian_day(1858,11,17,jd)
   write(*,*) 'epoch: ',epoch%isoformat(),' ',jd
   t=datetime(2000,01,01)
   call julian_day(2000,01,01,n)
   write(*,*) 't:     ',t%isoformat(),' ',n
   dt = t-epoch
   write(*,*) 'dt = ',nint(dt%total_seconds()/86400),n-jd

contains

!-----------------------------------------------------------------------------

SUBROUTINE julian_day(yyyy,mm,dd,julian)
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
END SUBROUTINE julian_day

!-----------------------------------------------------------------------------

SUBROUTINE calendar_date(julian,yyyy,mm,dd)
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
END SUBROUTINE calendar_date


END PROGRAM test_julian
