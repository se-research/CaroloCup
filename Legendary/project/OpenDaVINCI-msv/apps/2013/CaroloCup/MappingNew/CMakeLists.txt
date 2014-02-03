#
# CaroloCup.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (MappingNew)

# OpenCV libraries.
SET (OpenCV_LIBS opencv_core opencv_imgproc opencv_highgui opencv_calib3d)
SET (Ueye_LIBS ueye_api)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../cc-data/include)
INCLUDE_DIRECTORIES (include)

set(CMAKE_BUILD_TYPE Debug)

# Recipe for building "MappingNew".
FILE(GLOB_RECURSE MappingNew-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (MappingNewlib STATIC ${MappingNew-sources})
ADD_EXECUTABLE (MappingNew "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (MappingNew MappingNewlib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS} ${OpenCV_LIBS})

# Recipe for installing "MappingNew".
INSTALL(TARGETS MappingNew RUNTIME DESTINATION bin/2013/CaroloCup RENAME MappingNew)
