title: Content
Author: Karsten Bolding

The following components are included in FLOM as of April 2020.

* A generic [NetCDF](https://www.unidata.ucar.edu/software/netcdf/) reader 
  allowing for creating an efficient and simple input manager.
* An [output manager]() configured through a simple 
  [YAML](https://yaml.org/)-based configuration file.
* A [date/time module](https://wavebitscientific.github.io/datetime-fortran/) 
  by [Milan Curcic](https://github.com/milancurcic) providing an API to 
  date/time manipulations similar to the Python 
  [datetime-module](https://docs.python.org/3/library/datetime.html). 
  The data-time module is enhanced with two routines to work with Julian days
  in [[julian\_days]].
* A logging module allowing for a granular facility to log ... during
  program execution.
* A very simple [[grid\_module]] with 1D, 2D and 3D grids. The idea is that 
  models will extend this type to a full fledged calculation domain i.e. 
  providing shape and cordinates for the model.
* A memory manager to dynamically allocated 1D, 2D and 3D arrays of different 
  types with optionally halo-zones used in domain decomposed parallel model 
  configurations.
* The [TEOS-10]() Fortran version of [Gibbs Sea Water](GSW-library).

For each of the components listed above FLOM contains testing programs to
1) test the functionallity and 2) easily test 'corner' cases without having
to apply a full model. The testing mechanism is implemented using facilities
in CMake.

External dependencies - NetCDF (MPI?).

As proof of concept a simple geostrophic model has been implemented using 
the FLOM framework. 
The model uses data from the [World Ocean Atlas](https://www.nodc.noaa.gov/OC5/woa18/) specifically 
[salinity](https://www.nodc.noaa.gov/cgi-bin/OC5/woa18/woa18.pl?parameter=s)
 and [temperature](https://www.nodc.noaa.gov/cgi-bin/OC5/woa18/woa18.pl?parameter=t) as a replacemnet for dynamic salinity and temperature equations.
The geostrophic model contains most of the features present in a full fledged
numerical model except for specific time integration and advection/diffusion.
The model reads monthly values of salinity and temperature - calculates 
density and buoyancy. Based on the buoyancy the internal (baroclinic) pressure-
gradients ar calculated. Finally, the geostropic currents are calcuted.
All of the above listed components of FLOM is used in this proof-of-concept
model implementation.

