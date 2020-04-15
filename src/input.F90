! Copyright (C) 2020 Bolding & Bruggeman
!> This module provides a generic interface to reading 1D, 2D, 3D arrays
!> of integer, real32 and real64 types.
!> The data format is assumed to be 
!> [NetCDF Fortran90](https://www.unidata.ucar.edu/software/netcdf/docs-fortran/index.html)
!> The module uses the Fortran datetime module for all time manipulations
!> and make conversions between NetCDF units and time variable with datetime
!> type datetime and timedelta. Arrays of these type are create during
!> initialization of the input type and can be used later when specific
!> times of a given variable is needed - i.e. during time integration.
!>
!> @note
!> this needs to be fixed - i.e. units equal months
!> @endnote
!>

MODULE input_module

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   use netcdf
   use datetime_module

   IMPLICIT NONE

!-----------------------------------------------------------------------------

   PRIVATE  ! Private scope by default

!  Module constants

!  Module types and variables

   type, public :: type_input_data
      !! author: Karsten Bolding
      !! version: v0.1

      character(len=256) :: f
        !! Input file name
      integer :: format
         !! ASCII, NetCDF, ....
      logical :: time_dependent
        !! is the data in the file time dependent
      type(datetime) :: epoch
        !! datetime object
      type(datetime) :: first_time
        !! datetime object
      type(datetime) :: last_time
        !! datetime object
      character(len=:), allocatable :: strp_format
        !! time format of data

      contains

!      procedure :: initialize => initialize_data
!      procedure :: get => get_data
!      procedure :: close => data_data

   end type type_input_data

   type, extends(type_input_data), public :: type_netcdf_input
      !! author: Karsten Bolding
      !! version: v0.1

      character(len=256) :: v
        !! NetCDF variable name
      integer, private :: ncid
        !! NetCDF id
      integer, private :: varid
        !! variable id
      integer, private :: time_var_id
      !! time variable id associated with variable id
      integer :: ndims
        !! number of dimensions
      integer, private :: time_dim_id, unlimdimid 
        !! time_dim_id and unlimited id any - else -1
      integer, dimension(:), allocatable :: dimids, dimlens
        !! list of dimension ids and lengths
      character(len=64), dimension(:), allocatable :: cord_names
        !! list of dimension ids and lengths
      integer, private, dimension(:), allocatable :: start, count
        !! arguments to the NetCDF get routine
      logical, private :: extern_start=.false., extern_count=.false.
        !! are start and count provided externally
      integer :: timedim, timelen
      character(len=256) :: timeunit
      real(real64), dimension(:), allocatable :: times
        !! time variable if present
        !!https://www.unidata.ucar.edu/software/netcdf/docs/BestPractices.html
        !! https://www.cl.cam.ac.uk/~mgk25/iso-time.html
        type(datetime), dimension(:), allocatable :: datetimes
        type(timedelta), dimension(:), allocatable :: timedeltas
        !! time variable if present
      integer, private :: myreader 
        !! used in selct case to get the right NetCDF reader
      integer(int32), pointer :: pscalarint32 => null()
      integer(int32), dimension(:), pointer :: p1dint32 => null()
      integer(int32), dimension(:,:), pointer :: p2dint32 => null()
      integer(int32), dimension(:,:,:), pointer :: p3dint32 => null()
      integer(int32), dimension(:,:,:,:), pointer :: p4dint32 => null()

      real(real32), pointer :: pscalarreal32 => null()
      real(real32), dimension(:), pointer :: p1dreal32 => null()
      real(real32), dimension(:,:), pointer :: p2dreal32 => null()
      real(real32), dimension(:,:,:), pointer :: p3dreal32 => null()
      real(real32), dimension(:,:,:,:), pointer :: p4dreal32 => null()

      real(real64), pointer :: pscalarreal64 => null()
      real(real64), dimension(:), pointer :: p1dreal64 => null()
      real(real64), dimension(:,:), pointer :: p2dreal64 => null()
      real(real64), dimension(:,:,:), pointer :: p3dreal64 => null()
      real(real64), dimension(:,:,:,:), pointer :: p4dreal64 => null()

      contains

      procedure :: configure => configure_netcdf_input
      procedure :: initialize => initialize_netcdf_input
      procedure :: print_info => print_info_netcdf_input
!      procedure, deferred :: get => get_netcdf_input
!      procedure :: getsize => get_netcdf_variable_size
      procedure :: get => get_netcdf_input
      procedure :: close => close_netcdf_input

   end type type_netcdf_input

!-----------------------------------------------------------------------------

CONTAINS

SUBROUTINE configure_netcdf_input(self,start,count)
   !! Open a NetCDF file and get a variable id

   IMPLICIT NONE

!  Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self
   integer, dimension(:), intent(in), optional :: start,count

!  Local constants

!  Local variables
!-----------------------------------------------------------------------------
   ! the following allows to read a subset - determined by the calling program
   ! default is to read everything
   self%extern_start =.false.
   if (present(start)) then
      self%extern_start =.true.
      allocate(self%start(size(start)))
      self%start=start
   end if   
   self%extern_count =.false.
   if (present(count)) then
      self%extern_count =.true.
      allocate(self%count(size(count)))
      self%count=count
   end if   
   return
END SUBROUTINE configure_netcdf_input

!-----------------------------------------------------------------------------

SUBROUTINE initialize_netcdf_input(self)
   !! Open a NetCDF file and get a variable id

   IMPLICIT NONE

!  Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self

!  Local constants

!  Local variables
   integer :: rc
   integer :: n
!   type(datetime) :: reftime
   real(real64) :: reftime
   character(len=*), parameter :: kurt="0001-01-01 00:00:00"
     !! reference time for num2date Fortran routine
   type(timedelta) :: t,dt
!-----------------------------------------------------------------------------
!   self%time_dependent = .false. 
    self%strp_format = '%Y-%m-%d %H:%M:%S' 

   ! open the NetCDF file with the bathymetry
   call check( nf90_open(trim(self%f), NF90_NOWRITE, self%ncid) )

   ! check existence of the variable in the file
   call check(nf90_inq_varid(self%ncid, trim(self%v), self%varid))

   rc = nf90_inquire(self%ncid, unlimiteddimid=self%unlimdimid)

   ! get dimension ids for the obtained variable id
   call check( nf90_inquire_variable(self%ncid, self%varid, ndims=self%ndims) )
   allocate(self%dimids(self%ndims))
   call check(nf90_inquire_variable(self%ncid, self%varid, dimids=self%dimids(:)))

   ! get dimension lengths for variable varid
   allocate(self%dimlens(self%ndims))
   allocate(self%cord_names(self%ndims))
   if (self%extern_start) then
      if (size(self%start) /= self%ndims) then
              ! some error handling
      end if   
   else        
      allocate(self%start(self%ndims))
      self%start=1
   end if   
   if (self%extern_count) then
      if (size(self%count) /= self%ndims) then
              ! some error handling
      end if   
   else        
      allocate(self%count(self%ndims))
   end if        
   do n=1,self%ndims
      call check(nf90_inquire_dimension(self%ncid, self%dimids(n), name=self%cord_names(n), len=self%dimlens(n)))
      if (.not. self%extern_count) self%count(n)=self%dimlens(n)
   end do

   ! check if variable has time dimension - assume name of time variable is 'time'
   rc =  nf90_inq_dimid(self%ncid,'time',self%time_dim_id)
   if (rc == nf90_noerr) then
      ! check if time dimension is the unlimmited dimension (if any)
      call check(nf90_inquire_dimension(self%ncid, self%time_dim_id, len=self%timelen))
   end if

   rc =  nf90_inq_varid(self%ncid,'time',self%time_var_id)
   if(rc == nf90_noerr) then 
      call check(nf90_get_att(self%ncid,self%time_var_id,"units",self%timeunit))
      allocate(self%times(self%timelen))
      allocate(self%timedeltas(self%timelen))
      allocate(self%datetimes(self%timelen))
      call check(nf90_get_var(self%ncid, self%time_var_id, self%times))
      self%epoch = strptime( trim(self%timeunit(index(self%timeunit,'since')+5:)), trim(self%strp_format) )

      call datetime_conversion(self%epoch,trim(self%timeunit),self%times,self%timedeltas,self%datetimes)
      self%first_time = self%datetimes(1)
      self%last_time =  self%datetimes(self%timelen)
   else
      self%time_var_id=-1; self%timelen=-1
   end if
   return
END SUBROUTINE initialize_netcdf_input

!-----------------------------------------------------------------------------

SUBROUTINE print_info_netcdf_input(self)
   !! Open a NetCDF file and get a variable id

   IMPLICIT NONE

!  Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self

!  Local constants

!  Local variables
   integer :: n
!-----------------------------------------------------------------------------
   write(*,*) 'reading from file: '
   write(*,*) '   ',trim(self%f)
   write(*,*) '   ncid= ',self%ncid
   write(*,*) 'variable:          ',trim(self%v)
   write(*,*) '   varid=',self%varid
   write(*,*) '   ndims=',self%ndims
   do n=1,self%ndims
      write(*,*) '      ',self%dimids(n),self%dimlens(n),trim(self%cord_names(n))
   end do
   if (self%time_var_id .ne. -1) then
      write(*,*) 'associated time information'
      write(*,*) '   time_dim_id= ',self%time_dim_id
      write(*,*) '   unlimited=   ',self%time_var_id .eq. self%unlimdimid
      write(*,*) '   time_var_id= ',self%time_var_id
      write(*,*) '   timelen=     ',self%timelen
      write(*,*) '   timeunit=    ',trim(self%timeunit)
      write(*,*) '   epoch=       ',self%epoch%isoformat() 
      write(*,*) '   first=       ',self%first_time%isoformat() 
      write(*,*) '   last=        ',self%last_time%isoformat() 
      if (self%timelen .lt. 4) then
         write(*,*) '   time(s)= ',self%times
      else
         write(*,*) '   times=   ',self%times(1:2),' ... ',self%times(self%timelen-1:self%timelen)
      end if
   else
      write(*,*) 'no associated time information'
   end if
   write(*,*) 'reading this hyperslab'
   do n=1,self%ndims
      write(*,*) '   start, count= ',self%start(n),self%count(n)
   end do
   return
END SUBROUTINE print_info_netcdf_input

!-----------------------------------------------------------------------------

SUBROUTINE datetime_conversion(epoch,timeunit,times,timedeltas,datetimes)
   !! Convert NetCDF time information to timedelta objects

   IMPLICIT NONE

!  Subroutine arguments
   type(datetime), intent(inout) :: epoch
   character(len=*), intent(in) :: timeunit
   real(real64), dimension(:), intent(in) :: times
   type(timedelta), dimension(:), intent(inout) :: timedeltas
   type(datetime), dimension(:), intent(inout) :: datetimes

!  Local constants

!  Local variables
   integer :: years=0,months=0,days=0,hours=0
   integer :: minutes=0,seconds=0,milliseconds=0
   integer :: m,n,ndays
!-----------------------------------------------------------------------------
   if (index(timeunit,'milliseconds') .ne. 0) then
      write(*,*) 'using milliseconds'
      do n=1,size(times)
         milliseconds=int(times(n))
         timedeltas(n)=timedelta(milliseconds=milliseconds)
         datetimes(n)=epoch+timedeltas(n)
      end do
   else if (index(timeunit,'seconds') .ne. 0) then
!      write(*,*) 'using seconds'
      do n=1,size(times)
         seconds=int(times(n))
         milliseconds=int(1000*(times(n)-seconds))
         timedeltas(n)=timedelta(seconds=seconds, &
                                 milliseconds=milliseconds)
         datetimes(n)=epoch+timedeltas(n)
      end do
   else if (index(timeunit,'minutes') .ne. 0) then
!      write(*,*) 'using minutes'
      do n=1,size(times)
         minutes=int(times(n))
         timedeltas(n)=timedelta(minutes=minutes, &
                                 seconds=seconds, &
                                 milliseconds=milliseconds)
         datetimes(n)=epoch+timedeltas(n)
      end do
   else if (index(timeunit,'hours') .ne. 0) then
!      write(*,*) 'using hours'
      do n=1,size(times)
         hours=int(times(n))
         timedeltas(n)=timedelta(hours=hours, &
                                 minutes=minutes, &
                                 seconds=seconds, &
                                 milliseconds=milliseconds)
         datetimes(n)=epoch+timedeltas(n)
      end do
   else if (index(timeunit,'days').ne. 0) then
!      write(*,*) 'using days'
      do n=1,size(times)
         days=int(times(n))
         timedeltas(n)=timedelta(days=days, &
                                 hours=hours, &
                                 minutes=minutes, &
                                 seconds=seconds, &
                                 milliseconds=milliseconds)
         datetimes(n)=epoch+timedeltas(n)
      end do
   else if (index(timeunit,'months').ne. 0) then
!      write(*,*) 'using months'
      do n=1,size(times)
         days=int(30*(times(n)-int(times(n))))
         timedeltas(n)=timedelta(days=days, &
                                 hours=hours, &
                                 minutes=minutes, &
                                 seconds=seconds, &
                                 milliseconds=milliseconds)
         datetimes(n)=epoch+timedeltas(n)
      end do
   else
      stop 'initialize_netcdf_input - cant find egon'
   end if
   return
END SUBROUTINE datetime_conversion

!-----------------------------------------------------------------------------

SUBROUTINE get_netcdf_input(self,error_handler)
   !! Read the bathymetry from an external file and sets the mask

   IMPLICIT NONE

! Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self
   integer, intent(inout), optional :: error_handler

! Local constants

! Local variables
   integer, dimension(2) :: start, count
!-----------------------------------------------------------------------------
!KB   call self%print_info()

   if (associated(self%p1dreal64)) then
      call check(nf90_get_var(self%ncid,self%varid,self%p1dreal64, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
   end if 
   if (associated(self%p2dreal64)) then
      call check(nf90_get_var(self%ncid, self%varid, self%p2dreal64, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
   end if 
   if (associated(self%p3dreal64)) then
      call check(nf90_get_var(self%ncid, self%varid, self%p3dreal64, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
   end if 

   return
END SUBROUTINE get_netcdf_input

!-----------------------------------------------------------------------------

SUBROUTINE close_netcdf_input(self)
   !! Read the bathymetry from an external file and sets the mask

   IMPLICIT NONE

!  Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self

!  Local constants

!  Local variables
!-----------------------------------------------------------------------------
   call check( nf90_close(self%ncid) )
   self%ncid=-1
   return
END SUBROUTINE close_netcdf_input

!-----------------------------------------------------------------------------

subroutine  check(status,error_handler)
   integer, intent (in) :: status
   integer, intent(inout), optional :: error_handler

   if (present(error_handler)) then
      error_handler = 1
   else 
      if(status /= nf90_noerr) then 
        print *, trim(nf90_strerror(status))
        stop "Stopped"
      end if
   end if
   return
end subroutine check 

!-----------------------------------------------------------------------------

END MODULE input_module

#if 0
   TYPE, extends(type_input_data) :: type_netcdf_2d_real64
     !! netcdf_2d_real64 extends type_netcdf_input
      integer :: dimids(2)
      integer :: dimlens(2) 
      real(real64), dimension(:,:), pointer :: data
   contains
      procedure, :: get => get_netcdf_2d_real64
   END TYPE type_netcdf_2d_real64

   TYPE, extends(type_input_data) :: type_scalar_data
     !! 0D_data extends type_input_data
      integer :: data_rank=0
      real(real64), pointer :: data
   contains
      procedure :: initialize => initialize_netcdf_input
      procedure, deferred :: get => get_netcdf_input
      procedure :: close => close_netcdf_input
   END TYPE type_scalar_data

   TYPE, extends(type_input_data) :: type_1d_data
     !! 1D_data extends type_input_data
      integer :: data_rank=1
      real(real64), dimension(:), pointer :: data
   contains
   END TYPE type_1d_data

   TYPE, extends(type_input_data) :: type_2d_data
     !! 2D_data extends type_input_data
      integer :: data_rank=2
      real(real64), dimension(:,:), pointer :: data
   contains
   END TYPE type_2d_data

   TYPE, extends(type_input_data) :: type_3d_data
     !! 3D_data extends type_input_data
      integer :: data_rank=3
      real(real64), dimension(:,:,:), pointer :: data
   contains
   END TYPE type_3d_data

   PUBLIC type_scalar_data, type_1d_data, type_2d_data, type_3d_data

#endif
