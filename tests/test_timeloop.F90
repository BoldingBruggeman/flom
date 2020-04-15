! Copyright (C) 2020 Bolding & Bruggeman
!> Test program showing a very basic time loop.
!> 
!> The program uses the 
!> [datetime](https://github.com/wavebitscientific/datetime-fortran)
!> package by Milan Curcic.

PROGRAM test_timeloop

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use datetime_module, only: datetime, timedelta, clock, strptime

   IMPLICIT NONE

!  Local constants
   character(len=256), parameter :: time_format='%Y-%m-%d %H:%M:%S' 
   real(real64), parameter :: timestep = 3600.0_real64

!  Local variables
   TYPE(datetime) :: sim_start
     !! datetime object
   TYPE(datetime) :: sim_stop
     !! datetime object
   TYPE(datetime) :: sim_time
     !! datetime object
   TYPE(timedelta) :: dt
     !! timedelta object
   TYPE(timedelta) :: remain
     !! timedelta object
   integer :: n
     !! counter of number of timesteps
   integer :: secs
     !! used to initialize the dt timedelta object
   integer :: msecs
     !! used to initialize the dt timedelta object
   character(len=64) :: timestr
!---------------------------------------------------------------------------
   if (command_argument_count() .ne. 2) then
      write(*,*) 'ERROR, must be called like (timestep set to 3600s):'
      STOP ' test_timeloop <start> <stop> - timeformat "yyyy-mm-dd hh:mi:ss"'
   end if

   call get_command_argument(1,timestr)
   sim_start = strptime( timestr, trim(time_format) )
   call get_command_argument(2,timestr)
   sim_stop  = strptime( timestr, trim(time_format) )
   sim_time = sim_start
   write(*,*) 'Start: ',sim_start % strftime(trim(time_format))
   write(*,*) 'Stop:  ',sim_stop % strftime(trim(time_format))

   remain = sim_stop - sim_time
   write(*,*) 'Total seconds to simulate: ',remain%total_seconds()
   if (remain%total_seconds() .le. 0._real64) stop

   secs = int(timestep); msecs = 1000*(timestep-secs)
   dt = timedelta(seconds = secs, milliseconds=msecs)
   n = 0
   do while (remain%total_seconds() .gt. 0._real64)
      n = n+1
      sim_time = sim_time + dt

      write(*,*) sim_time % strftime(trim(time_format))

      remain = sim_stop - sim_time
   end do
   write(*,*) 'Number of steps: ',n

CONTAINS 

END PROGRAM test_timeloop
