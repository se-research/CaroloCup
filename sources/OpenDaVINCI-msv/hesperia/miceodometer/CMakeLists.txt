#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (miceodometer)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (${libhesperia_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "miceodometer".
FILE(GLOB_RECURSE miceodometer-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (miceodometerlib STATIC ${miceodometer-sources})
ADD_EXECUTABLE (miceodometer "${CMAKE_CURRENT_SOURCE_DIR}/apps/miceodometer.cpp")
TARGET_LINK_LIBRARIES (miceodometer miceodometerlib hesperia ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "miceodometer".
INSTALL(TARGETS miceodometer RUNTIME DESTINATION bin)

