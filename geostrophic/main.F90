! Copyright (C) 2020 Bolding & Bruggeman

!!{!./example/code/main.md!}

PROGRAM geostrophic

   use geostrophic_model
   IMPLICIT NONE

!  Local constants

!  Local variables
   TYPE(type_geostrophic_model) :: themodel
!-----------------------------------------------------------------------------

   call themodel%settings()
   call themodel%configure()
   call themodel%initialize()
   call themodel%integrate()
   call themodel%finalize()

END PROGRAM geostrophic

! https://gist.github.com/n-s-k/522f2669979ed6d0582b8e80cf6c95fd
