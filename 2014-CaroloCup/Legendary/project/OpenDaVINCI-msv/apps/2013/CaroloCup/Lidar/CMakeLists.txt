#
# CaroloCup.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (Lidar)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../cc-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "sensorboard".
FILE(GLOB_RECURSE Lidar-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (Lidarlib STATIC ${Lidar-sources})
ADD_EXECUTABLE (Lidar "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (Lidar Lidarlib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "sensorboard".
INSTALL(TARGETS Lidar RUNTIME DESTINATION bin/2013/CaroloCup RENAME Lidar) 

