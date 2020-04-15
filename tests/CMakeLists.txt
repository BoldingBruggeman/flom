add_custom_target( test_all_flom )

add_executable(test_grids EXCLUDE_FROM_ALL
               ${CMAKE_CURRENT_LIST_DIR}/test_grids.F90
              )
target_link_libraries(test_grids PRIVATE flom )
add_dependencies( test_all_flom ${test_all_flom} test_grids )

add_executable(test_input EXCLUDE_FROM_ALL
               ${CMAKE_CURRENT_LIST_DIR}/test_input.F90
              )
target_link_libraries(test_input PRIVATE flom )
add_dependencies( test_all_flom ${test_all_flom} test_input )

add_executable(test_memory_manager EXCLUDE_FROM_ALL
               ${CMAKE_CURRENT_LIST_DIR}/test_memory_manager.F90
              )
target_link_libraries(test_memory_manager PRIVATE flom )
add_dependencies( test_all_flom ${test_all_flom} test_memory_manager )

add_executable(test_julian EXCLUDE_FROM_ALL
               ${CMAKE_CURRENT_LIST_DIR}/test_julian.F90
              )
target_link_libraries(test_julian PRIVATE flom)
add_dependencies( test_all_flom ${test_all_flom} test_julian )

add_executable(test_timeloop EXCLUDE_FROM_ALL
               ${CMAKE_CURRENT_LIST_DIR}/test_timeloop.F90
              )
target_link_libraries(test_timeloop PRIVATE flom )
add_dependencies( test_all_flom ${test_all_flom} test_timeloop )
