title: Cloning 
Author: Karsten Bolding

The FLOM code is hosted on [GitHub](https://github.com/) and can be 
cloned from there. 
To proceed [git](https://git-scm.com/) must be installed.

Below is provided the steps necessary to get a complete copy of the FLOM 
source install on the local computer using a terminal. A number of graphical 
frontends to Git exists. To use any of those please refer those for specific
instructions.
In the next section the FLOM code is configured.

The code can in principle be cloned to any folder on the computer - below
a folder in the users is used.

Commands can be copy/pasted directly to a terminal window and be executed.

First, the folder where the code will be installed is created:

```
cd ~
mkdir -p source/repos/FLOM
```

Cloning the source code a using [git](https://git-scm.com/)
Second, the newly folder is entered and the code is cloned:

```
cd ~/source/repos/FLOM
git clone --recurse-submodules https:/github.com/BoldingBruggeman/flom.git
```

The --recurse-submodules argument clones all FLOM submodules to the extern/
folder in the FLOM source code tree.

sub-modules

