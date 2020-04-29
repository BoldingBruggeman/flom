title: Example - geostrophic
Author: Karsten Bolding

The geostrophic currents has been implemented using the FLOM framework as
a proof of concept. The geostrophic currents is not a dynamic model but
an equialibrium between pressure gradients and the Coriolis term. 
Nevertheless, the model contains most of the components of a full dynamic 
model - but is much simpler.

The following shows the flow of calculations:

0. Set-up the calculation domain using [[grid\_module]] as a base type to create
the [Arakawa A-grid](https://en.wikipedia.org/wiki/Arakawa_grids) used in the 
model. Cordinate information is obtained from one of the monthly temperature 
files and and the [[domain\_type]] is an extension of 
[[grid\_module:type\_3d\_grid]].
1. Loop over 12 month and for each of them do the following steps.
2. Salinity and temperature are read from WOA monthly files - low (5 degrees),
medium (1 degree) and high (0.25 degree) resolution versions are supported.
The resolution is selected by setting an environmental variable - 
WOA\_RESOLUTION - to either 'l', 'm', or 'h'. Default is 'm'.
A subset of the globe can be selected by specifying imin, imax, jmin and
jmax on the command line. Data are read using the FLOM [[input\_module]].
3. Using WOA fields of salinity and temperature the density is calculated 
using routines from the [TEOS-10]() GSW-library. The GSW - among many other 
routines provides conversion between in-situ, potential and conservative 
temperature as well as pratical salinity unit (PSU) and absolute salinity. The 
documentation for TEOS-10 is very comprehensive and is strongly recommended 
for further studies. 
Based on the density field, a reference density \(rho_0\) and acceleration
of gravity \(g\) the buoyancy is calculated. Density and buoyancy calculations
are done in [[density]].
4. 


aa|url|bb
aa|media|bb

