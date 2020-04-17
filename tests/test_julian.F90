! Copyright (C) 2020 Bolding & Bruggeman
!> Test program showing a very basic time loop.
!> 
!> The program uses the 
!> [datetime](https://github.com/wavebitscientific/datetime-fortran)
!> package by Milan Curcic.

PROGRAM test_julian

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use datetime_module
   use julian_days

   IMPLICIT NONE

!  Local variables
   integer :: jd=0,n
   integer :: y,m,d
   integer, dimension(3) :: ys,ms,ds,jds
   type(datetime) :: t1,t2
   type(timedelta) :: dt
   type(datetime) :: ts(3)
!---------------------------------------------------------------------------
   call init_epoch()
   call julian_day(1858,11,17,jd)
   call calendar_date(jd,y,m,d)
   t1=datetime(year=y,month=m,day=d)
   write(*,*) 'epoch: ',t1%isoformat(),' ',jd
   t2=datetime(2000,01,01)
   call julian_day(2000,01,01,n)
   write(*,*) 't2:    ',t2%isoformat(),' ',n
   dt = t2-t1
   write(*,*) 'dt= ',nint(dt%total_seconds()/86400),n-jd

   write(*,*)
   write(*,*) 'the Julian day routines are elemental'
   ys = (/ 1900, 1950, 2000 /)
   ms = 1
   ds = 1
   call julian_day(ys,ms,ds,jds)
   write(*,*) jds
   write(*,*)
   write(*,*) 'add 1 day'
   jds = jds+1
   write(*,*) jds
   write(*,*)
   write(*,*) 'and overloaded - yyyy, mm, dd'
   call calendar_date(jds,ys,ms,ds)
   write(*,*) ys
   write(*,*) ms
   write(*,*) ds
   write(*,*)
   write(*,*) '... datetime objects'
   call calendar_date(jds,ts)
   do n=1,size(ts)
      write(*,*) n,jds(n),ts(n)%isoformat()
   end do

END PROGRAM test_julian
