title: Installation
Author: Karsten Bolding

FLOM is an initiative to provide a framework containing software components
needed by all models of lakes and oceans - but typically independent of the 
specific model. The components includes - but are not limited to - input,
output, time routines - and from the more physical side - density calculations,
air/sea interactions.

Historically model developers have implemented these components for their own
model - and only to a very limited extent - re-used components already
available. There are several reasons for this e.g. generic components
have typically not been developed with sharing in mind, software tools have not
provided flexible 'glueing' facilities and knowlegde of availabe components
have not been present.

Historically, geophysical models have been coded in Fortran. FLOM continues
this tradition but applies modern Fortran principles - notably the use
of [Fortran derived types](http://fortranwiki.org/fortran/show/Object-oriented+programming). 

Furthermore, the sharing of computer code - not only in the field of 
geophysical models - have been greatly enhanced through the use of public
code repositories available to everybody from everywhere. The main one being
[GitHub](https://github.com/) where also FLOM is stored.

Finally, cross platform software configuration has been greatly improved
by the arrival of [CMake](https://cmake.org/). CMake allows for a common 
configuration to be used across Windows, Mac and Linux without any changes.

20+ years involment in model developments in combination with the above 
given developments has lead to the idea of FLOM.
