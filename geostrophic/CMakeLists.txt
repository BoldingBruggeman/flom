add_executable(geostrophic)
set_property( TARGET geostrophic PROPERTY FOLDER geostrophic )
target_sources( geostrophic PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/main.F90
    ${CMAKE_CURRENT_LIST_DIR}/geostrophic.F90
    ${CMAKE_CURRENT_LIST_DIR}/domain.F90
    ${CMAKE_CURRENT_LIST_DIR}/physics.F90
    ${CMAKE_CURRENT_LIST_DIR}/salinity.F90
    ${CMAKE_CURRENT_LIST_DIR}/temperature.F90
    ${CMAKE_CURRENT_LIST_DIR}/density.F90
    ${CMAKE_CURRENT_LIST_DIR}/dynamics.F90
    ${CMAKE_CURRENT_LIST_DIR}/currents.F90
    ${CMAKE_CURRENT_LIST_DIR}/pressure.F90
    ${CMAKE_CURRENT_LIST_DIR}/input.F90
    ${CMAKE_CURRENT_LIST_DIR}/output.F90
)
target_link_libraries(geostrophic PRIVATE flom)
