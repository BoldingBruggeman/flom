title: Input 
Author: Karsten Bolding

NetCDF generic reader

CFconventions

UGrid, SGrid

Access to the input related parts for the FLOM framework the following must
appear in the source code:
```
use input_manager
```
and a new external variable is declared as:
```
class(type_netcdf_input), allocatable :: input
```

The link between the model variable and the pointer the reader operates on 
is setup like:
```
input%p2dreal64 => depth
```
where depth has been declared either as an allocatable array:
```
real(real64), dimension(:,:), allocatable, target :: depth
allocate(depth(imax,jmax))
```
or as a static array:
```
real(real64), target :: depth(imax,jmax)
```
where imax and jmax defines the extent of the grid.
Note the 'target' attribute allowing the pointer in the type\_netcdf\_input 
type to make an association. 

Two test programs are included to illustrate the use of the [[input\_module]]
- [[test\_input]] and [[test\_read\_netcdf]]


