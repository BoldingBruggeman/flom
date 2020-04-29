title: Configuring
Author: Karsten Bolding

[CMake](https://cmake.org/) provides a cross-platform tool for configuring
compilation of source code into libraries and executables.

The configuration recipes are given in one ore more CMakeLists.txt files
and support very complex configurations potentially comprising several
different software packages.

FLOM uses CMake to build one library with all components and expose this
library to a model.
FLOM also uses CMake to build several test programs used to assure the
consistency of FLOM.
Finally - FLOM also uses CMake to build the proof-of-concept geostrophic
model.

CMake comes with comprehensive documentation and allows for many configurations
either via the command line of through a GUI.

CMake promotes 'out-of-source' compilation - i.e. the compilation is 
done in a folder that is NOT part of the source code. The great advantage is
that there is no danger of over-writing/deleting important files. In addition
the entire build folde can be deleted afther the model has been compiled
and installed.

In the most simple setup - where NetCDF is installed in a proper way and
a Fortran compiler is in the PATH (the same brand and version of the Fortran
compiler used to compile the NetCDF library) - the following commands are
sufficient:

```
cd ~/source && mkdir -p build/flom && cd build/flom
cmake ../../repos/FLOM/flom
```
A costum Fortran compiler can be specified via:
```
cmake -DCMAKE-Fortran-COMPILER=intel ../../repos/FLOM/flom
```
