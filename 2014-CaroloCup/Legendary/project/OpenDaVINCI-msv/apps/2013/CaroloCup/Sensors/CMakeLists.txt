#
# CaroloCup.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (Sensors)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../cc-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "sensorboard".
FILE(GLOB_RECURSE Sensors-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (Sensorslib STATIC ${Sensors-sources})
ADD_EXECUTABLE (Sensors "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (Sensors Sensorslib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "sensorboard".
INSTALL(TARGETS Sensors RUNTIME DESTINATION bin/2013/CaroloCup RENAME Sensors) 

