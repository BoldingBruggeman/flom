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
!> @bug
!> need to fix scale_factor and add_offset when types are different
!> check WOA cordinate variables
!> @endbug

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

      character(len=256) :: f=''
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

      procedure :: link_data_1d_int32
      procedure :: link_data_2d_int32
      procedure :: link_data_3d_int32
      procedure :: link_data_4d_int32
!KB      procedure :: link_data_1d_real32
!KB      procedure :: link_data_2d_real32
!KB      procedure :: link_data_3d_real32
!KB      procedure :: link_data_4d_real32
      procedure :: link_data_1d_real64
      procedure :: link_data_2d_real64
      procedure :: link_data_3d_real64
      procedure :: link_data_4d_real64
      generic ::  link_data => link_data_1d_int32, link_data_2d_int32, link_data_3d_int32, link_data_4d_int32, &
!KB                               link_data_1d_real32, link_data_2d_real32, link_data_3d_real32, link_data_4d_real32, &
                               link_data_1d_real64, link_data_2d_real64, link_data_3d_real64, link_data_4d_real64

!      procedure :: initialize => initialize_data
!      procedure :: get => get_data
!      procedure :: close => data_data

   end type type_input_data

   type, extends(type_input_data), public :: type_netcdf_input
      !! author: Karsten Bolding
      !! version: v0.1

      character(len=32) :: v
        !! NetCDF variable name
      integer, private :: ncid=-1
        !! NetCDF id
      integer, private :: varid
        !! variable id
      integer, private :: time_var_id
      !! time variable id associated with variable id
      integer, public :: time_index
      !! the index in the dimension array that holds the time dimension
      integer :: xtype
        !! NetCDF variable data type
      integer :: ndims
        !! number of dimensions
      integer, private :: time_dim_id, unlimdimid
        !! time_dim_id and unlimited id any - else -1
      integer, dimension(:), allocatable :: dimids, dimlens
        !! list of dimension ids and lengths
      character(len=32), dimension(:), allocatable :: cord_names
        !! list of dimension ids and lengths
      character(len=:), allocatable :: units
        !! variable units - used to check coodinates
      integer, private, dimension(:), allocatable :: start, count
        !! arguments to the NetCDF get routine
      logical, private :: extern_start=.false., extern_count=.false.
        !! are start and count provided externally
      integer :: timedim, timelen
      character(len=:), allocatable :: timeunit
      real(real64), dimension(:), allocatable :: times
        !! time variable if present
        !! https://www.unidata.ucar.edu/software/netcdf/docs/BestPractices.html
        !! https://www.cl.cam.ac.uk/~mgk25/iso-time.html
      type(datetime), dimension(:), allocatable :: datetimes
        !! time variable if present
      integer :: cur=-1
!KB      integer, private :: myreader
        !! used in selct case to get the right NetCDF reader

      real(real32) :: scale_factor_real32=1._real32, add_offset_real32=0._real32
      real(real64) :: scale_factor_real64=1._real64, add_offset_real64=0._real64

      contains

      procedure :: configure => configure_netcdf_input
      procedure :: open => open_netcdf_input
      procedure :: dimlen => get_netcdf_dimlen
      procedure :: initialize => initialize_netcdf_input
      procedure :: print_info => print_info_netcdf_input
!      procedure, deferred :: get => get_netcdf_input
!      procedure :: getsize => get_netcdf_variable_size
      procedure :: get => get_netcdf_input
      procedure :: prev => get_prev
      procedure :: next => get_next
      procedure :: attime => get_attime
      procedure :: close => close_netcdf_input

   end type type_netcdf_input

!-----------------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------------

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
END SUBROUTINE configure_netcdf_input

!-----------------------------------------------------------------------------

SUBROUTINE open_netcdf_input(self,ncid)
   !! Open a NetCDF file for read access

   IMPLICIT NONE

!  Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self
   integer, intent(inout), optional :: ncid

!  Local constants

!  Local variables
!-----------------------------------------------------------------------------
   if (present(ncid)) then
      call check( nf90_open(trim(self%f), NF90_NOWRITE, ncid) )
   else
      call check( nf90_open(trim(self%f), NF90_NOWRITE, self%ncid) )
   end if
END SUBROUTINE open_netcdf_input

!-----------------------------------------------------------------------------

SUBROUTINE get_netcdf_dimlen(self,n,len)
   !! Open a NetCDF file for read access

   IMPLICIT NONE

!  Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self
   character(len=*), intent(in) :: n
   integer, intent(inout) :: len

!  Local constants

!  Local variables
   integer :: dimid
!-----------------------------------------------------------------------------
   call check( nf90_inq_dimid(self%ncid,trim(n),dimid) )
   call check( nf90_inquire_dimension(self%ncid,dimid,len=len) )
END SUBROUTINE get_netcdf_dimlen

!-----------------------------------------------------------------------------

SUBROUTINE initialize_netcdf_input(self,ncid,varname)
   !! Open a NetCDF file and get a variable id

   IMPLICIT NONE

!  Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self
   integer, intent(inout), optional :: ncid
   character(len=*), intent(in), optional :: varname

!  Local constants

!  Local variables
   integer :: rc
   integer :: n
!   type(datetime) :: reftime
   real(real64) :: reftime
   character(len=*), parameter :: kurt="0001-01-01 00:00:00"
     !! reference time for num2date Fortran routine
   type(timedelta) :: t,dt
   character(len=256) :: buf
   integer :: xtype
!-----------------------------------------------------------------------------
!   self%time_dependent = .false.
    self%strp_format = '%Y-%m-%d %H:%M:%S'

   ! open the NetCDF file
   if (ncid == -1) then
      call check( nf90_open(trim(self%f), NF90_NOWRITE, self%ncid) )
   else
      self%ncid=ncid
   end if

   ! check existence of the variable in the file
   call check(nf90_inq_varid(self%ncid, trim(self%v), self%varid), error_handler=rc)

   rc = nf90_inquire(self%ncid, unlimiteddimid=self%unlimdimid)

   ! get dimension ids for the obtained variable id
   call check( nf90_inquire_variable(self%ncid, self%varid, ndims=self%ndims) )
   allocate(self%dimids(self%ndims))
   call check(nf90_inquire_variable(self%ncid, self%varid, xtype=self%xtype, dimids=self%dimids(:)))

   rc = nf90_inquire_attribute(self%ncid, self%varid, "scale_factor", xtype=xtype)
   if(rc == nf90_noerr) then
      select case (xtype)
         case (NF90_FLOAT)
            rc = nf90_get_att(self%ncid, self%varid, "scale_factor", self%scale_factor_real32)
         case (NF90_DOUBLE)
            rc = nf90_get_att(self%ncid, self%varid, "scale_factor", self%scale_factor_real64)
      end select
   end if

   rc = nf90_inquire_attribute(self%ncid, self%varid, "add_offset", xtype=xtype)
   if(rc == nf90_noerr) then
      select case (xtype)
         case (NF90_FLOAT)
            rc = nf90_get_att(self%ncid, self%varid, "add_offset", self%add_offset_real64)
         case (NF90_DOUBLE)
            rc = nf90_get_att(self%ncid, self%varid, "add_offset", self%add_offset_real64)
      end select
   end if

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

   rc = nf90_get_att(self%ncid,self%varid,'units',buf)
   if(rc == nf90_noerr) then
      self%units = trim(buf)
      write(*,*) trim(self%units),len(self%units)
!KB      stop
   end if

   ! check if variable has time dimension - assume name of time variable is 'time'
   rc =  nf90_inq_dimid(self%ncid,'time',self%time_dim_id)
   if (rc == nf90_noerr) then
      ! check if time dimension is the unlimmited dimension (if any)
      call check(nf90_inquire_dimension(self%ncid, self%time_dim_id, len=self%timelen))
      do n=1,self%ndims
         if (self%dimids(n) == self%time_dim_id) exit
      end do
      self%time_index=n
   end if

   rc =  nf90_inq_varid(self%ncid,'time',self%time_var_id)
   if(rc == nf90_noerr) then
      call check(nf90_get_att(self%ncid,self%time_var_id,"units",buf))
      self%timeunit = trim(buf)
      allocate(self%times(self%timelen))
      allocate(self%datetimes(self%timelen))
      call check(nf90_get_var(self%ncid, self%time_var_id, self%times))
      self%epoch = strptime( trim(self%timeunit(index(self%timeunit,'since')+5:)), trim(self%strp_format) )

      call datetime_conversion(self%epoch,trim(self%timeunit),self%times,self%datetimes)
      self%first_time = self%datetimes(1)
      self%last_time =  self%datetimes(self%timelen)
   else
      self%time_var_id=-1; self%timelen=-1
   end if
END SUBROUTINE initialize_netcdf_input

!-----------------------------------------------------------------------------

SUBROUTINE link_data_1d_int32(self,a)
   class(type_input_data), intent(inout) :: self
   integer, dimension(:), target :: a
   integer :: stat
   self%p1dint32 => a
END SUBROUTINE link_data_1d_int32

SUBROUTINE link_data_2d_int32(self,a)
   class(type_input_data), intent(inout) :: self
   integer, dimension(:,:), target :: a
   integer :: stat
   self%p2dint32 => a
END SUBROUTINE link_data_2d_int32

SUBROUTINE link_data_3d_int32(self,a)
   class(type_input_data), intent(inout) :: self
   integer, dimension(:,:,:), target :: a
   integer :: stat
   self%p3dint32 => a
END SUBROUTINE link_data_3d_int32

SUBROUTINE link_data_4d_int32(self,a)
   class(type_input_data), intent(inout) :: self
   integer, dimension(:,:,:,:), target :: a
   integer :: stat
   self%p4dint32 => a
END SUBROUTINE link_data_4d_int32

SUBROUTINE link_data_1d_real64(self,a)
   class(type_input_data), intent(inout) :: self
   real(real64), dimension(:), target :: a
   integer :: stat
   self%p1dreal64 => a
END SUBROUTINE link_data_1d_real64

SUBROUTINE link_data_2d_real64(self,a)
   class(type_input_data), intent(inout) :: self
   real(real64), dimension(:,:), target :: a
   integer :: stat
   self%p2dreal64 => a
END SUBROUTINE link_data_2d_real64

SUBROUTINE link_data_3d_real64(self,a)
   class(type_input_data), intent(inout) :: self
   real(real64), dimension(:,:,:), target :: a
   integer :: stat
   self%p3dreal64 => a
END SUBROUTINE link_data_3d_real64

SUBROUTINE link_data_4d_real64(self,a)
   class(type_input_data), intent(inout) :: self
   real(real64), dimension(:,:,:,:), target :: a
   integer :: stat
   self%p4dreal64 => a
END SUBROUTINE link_data_4d_real64

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
   if (self%f == '') then
      write(*,*) 'reading from an open file'
   else
      write(*,*) 'reading from file: '
      write(*,*) '   ',trim(self%f)
   end if
   write(*,*) '   ncid= ',self%ncid
   write(*,*) 'variable:          ',trim(self%v)
   write(*,*) '   varid=',self%varid
   write(*,*) '   ndims=',self%ndims
   do n=1,self%ndims
      write(*,*) '      ',self%dimids(n),self%dimlens(n),trim(self%cord_names(n))
   end do
!KB   select case (self%xtype)
!KB      case (NF90_FLOAT)
         if(self%scale_factor_real32 /= 1._real32 .or. self%add_offset_real32 /= 0._real32) then
            write(*,*) '   scale(real32)= ',self%scale_factor_real32
            write(*,*) '   offset(real32)=',self%add_offset_real32
         end if
!KB      case (NF90_DOUBLE)
         if(self%scale_factor_real64 /= 1._real64 .or. self%add_offset_real64 /= 0._real64) then
            write(*,*) '   scale(real64)= ',self%scale_factor_real64
            write(*,*) '   offset(real64)=',self%add_offset_real64
         end if
!KB   end select
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
         write(*,*) '   time(s)= '
         do n=1,self%timelen
            write(*,*) '              ',self%times(n),self%datetimes(n)%isoformat()
         end do
      else
         n=self%timelen
         write(*,*) '   times=   '
         write(*,*) '              ',self%times(1),self%datetimes(1)%isoformat()
         write(*,*) '              ',self%times(2),self%datetimes(2)%isoformat()
         write(*,*) '             :'
         write(*,*) '             :'
         write(*,*) '              ',self%times(n-1),self%datetimes(n-1)%isoformat()
         write(*,*) '              ',self%times(n),self%datetimes(n)%isoformat()
      end if
   else
      write(*,*) 'no associated time information'
   end if
   write(*,*) 'reading this hyperslab'
   do n=1,self%ndims
      write(*,*) '   start, count= ',self%start(n),self%count(n)
   end do
END SUBROUTINE print_info_netcdf_input

!-----------------------------------------------------------------------------

SUBROUTINE datetime_conversion(epoch,timeunit,times,datetimes)
   !! Convert NetCDF time information to timedelta objects

   IMPLICIT NONE

!  Subroutine arguments
   type(datetime), intent(inout) :: epoch
   character(len=*), intent(in) :: timeunit
   real(real64), dimension(:), intent(in) :: times
   type(datetime), dimension(:), intent(inout) :: datetimes

!  Local constants

!  Local variables
   real(real64) :: dt
   real(real64), dimension(:), allocatable :: t
!-----------------------------------------------------------------------------
   dt = date2num(epoch)
   if (index(timeunit,'milliseconds') .ne. 0) then
      t=dt+times/(1000._real64*86400._real64)
   else if (index(timeunit,'seconds') .ne. 0) then
      t=dt+times/(86400._real64)
   else if (index(timeunit,'minutes') .ne. 0) then
      t=dt+times/(1440._real64)
   else if (index(timeunit,'hours') .ne. 0) then
      t=dt+times/(24._real64)
   else if (index(timeunit,'days').ne. 0) then
      t=dt+times
   else if (index(timeunit,'months').ne. 0) then
      t=dt+times*30._real64
   end if
   datetimes=num2date(t)
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
  integer :: stat
!-----------------------------------------------------------------------------
   ! reading integers
   if (associated(self%p1dint32)) then
      call check(nf90_get_var(self%ncid,self%varid,self%p1dint32, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
   end if
   if (associated(self%p2dint32)) then
      call check(nf90_get_var(self%ncid, self%varid, self%p2dint32, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
   end if
   if (associated(self%p3dint32)) then
      call check(nf90_get_var(self%ncid, self%varid, self%p3dint32, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
   end if
   if (associated(self%p4dint32)) then
      call check(nf90_get_var(self%ncid, self%varid, self%p3dint32, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
   end if

   ! reading doubles
   if (associated(self%p1dreal64)) then
      call check(nf90_get_var(self%ncid,self%varid,self%p1dreal64, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
      if(self%scale_factor_real64 /= 1._real64 .or. self%add_offset_real64 /= 0._real64) then
         self%p1dreal64 = self%scale_factor_real64*self%p1dreal64 + self%add_offset_real64
      end if
   end if
   if (associated(self%p2dreal64)) then
      call check(nf90_get_var(self%ncid, self%varid, self%p2dreal64, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
      if(self%scale_factor_real64 /= 1._real64 .or. self%add_offset_real64 /= 0._real64) then
         self%p2dreal64 = self%scale_factor_real64*self%p2dreal64 + self%add_offset_real64
      end if
   end if
   if (associated(self%p3dreal64)) then
      call check(nf90_get_var(self%ncid, self%varid, self%p3dreal64, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
      if(self%scale_factor_real64 /= 1._real64 .or. self%add_offset_real64 /= 0._real64) then
         self%p3dreal64 = self%scale_factor_real64*self%p3dreal64 + self%add_offset_real64
      end if
   end if
   if (associated(self%p4dreal64)) then
      call check(nf90_get_var(self%ncid, self%varid, self%p3dreal64, &
                              start=self%start(1:self%ndims), &
                              count=self%count(1:self%ndims)))
      if(self%scale_factor_real64 /= 1._real64 .or. self%add_offset_real64 /= 0._real64) then
         self%p4dreal64 = self%scale_factor_real64*self%p4dreal64 + self%add_offset_real64
      end if
   end if
END SUBROUTINE get_netcdf_input

!-----------------------------------------------------------------------------

SUBROUTINE get_prev(self,stat)
   !! Get the previous field in the file

   IMPLICIT NONE

! Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self
   integer, intent(inout) :: stat

! Local constants

! Local variables
!   integer, dimension(:), allocatable :: start, count
   integer :: n
!-----------------------------------------------------------------------------
   if (self%cur == -1) then
     self%cur = 1 ! first data set to read
   else
      self%cur=self%cur-1
   end if
   if (self%cur .lt. 1) then
      stat=1
      return
   end if
   self%start(self%time_index)=self%cur
   self%count(self%time_index)=1
   call self%get()
   stat=0
END SUBROUTINE get_prev

!-----------------------------------------------------------------------------

SUBROUTINE get_next(self,stat)
   !! Get the next field in the file

   IMPLICIT NONE

! Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self
   integer, intent(inout) :: stat

! Local constants

! Local variables
!   integer, dimension(:), allocatable :: start, count
   integer :: n
!-----------------------------------------------------------------------------
   if (self%cur == -1) then
     self%cur = 1 ! first data set to read
   else
      self%cur=self%cur+1
   end if
   if (self%cur .gt. self%timelen) then
      stat=1
      return
   end if
   self%start(self%time_index)=self%cur
   self%count(self%time_index)=1
   call self%get()
   stat=0
END SUBROUTINE get_next

!-----------------------------------------------------------------------------

SUBROUTINE get_attime(self,t,stat,kurt)
   !! Get the field at a specified time

   IMPLICIT NONE

! Subroutine arguments
   class(type_netcdf_input), intent(inout) :: self
   type(datetime) :: t
   integer, intent(inout) :: stat
   integer, intent(in), optional :: kurt

! Local constants

! Local variables
   integer :: n
   logical :: found
!-----------------------------------------------------------------------------
!   do n=1,len(self%datetimes)
#if 0
! use binary search
   found = .false.
   n = 1
   do while (.not. found)
      found = (t == self%datetimes(n))
   end do
#else
   do n=1,size(self%datetimes)
!KB      write(*,*) n
      if (t == self%datetimes(n)) exit
   end do
#endif
   self%cur = n
   self%start(self%time_index)=self%cur
   self%count(self%time_index)=1
   call self%get()
   stat=0
END SUBROUTINE get_attime

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
