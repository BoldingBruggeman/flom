title: Compiling
Author: Karsten Bolding

After a succesful configuration the FLOM source code is ready to be compiled.

CMake supports different build tools on different platforms - e.g.
[Visual Studio]() on Windows and [make]() on Linux.

On Linux the compilation is as simple as:
```
cd ~/source/build/flom
make
```

a successful compilation will result in lines like:

```
:
[100%] Building Fortran object CMakeFiles/geostrophic.dir/geostrophic/geostrophic.F90.o
[100%] Building Fortran object CMakeFiles/geostrophic.dir/geostrophic/main.F90.o
[100%] Linking Fortran executable geostrophic
[100%] Built target geostrophic
```

To build all tests the following can be used:
```
make test_all_flom
```

specific test can be compiled using:
```
make test<TAB>
```
this will produce a list of Make targets starting with 'test'.

As an example to compile the test program - [[test\_julian]] do:
```
make test_julian
```

To execute the test program do:
```
./test_julian
```

