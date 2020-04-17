set_property( TARGET flom PROPERTY FOLDER flom )
target_sources( flom PRIVATE 
	${CMAKE_CURRENT_LIST_DIR}/grid.F90
	${CMAKE_CURRENT_LIST_DIR}/julian_day.F90
	${CMAKE_CURRENT_LIST_DIR}/logging.F90
	${CMAKE_CURRENT_LIST_DIR}/input.F90
	${CMAKE_CURRENT_LIST_DIR}/memory_manager.F90
	)
