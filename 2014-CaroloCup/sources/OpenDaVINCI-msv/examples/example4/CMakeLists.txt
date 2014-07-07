#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (example4)

# OpenCV libraries.
SET (OpenCV_LIBS opencv_core opencv_imgproc opencv_highgui)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

IF("${PANDABOARD}" STREQUAL "YES")
    SET (CMAKE_C_FLAGS "-pipe -ansi -D PANDABOARD=1")
    SET (CMAKE_CXX_FLAGS "-pipe -ansi -D PANDABOARD=1")

    INCLUDE_DIRECTORIES (/opt/PandaBoard-SD/usr/include)
    LINK_DIRECTORIES (/opt/PandaBoard/lib /opt/PandaBoard-SD/usr/lib /opt/PandaBoard-SD/usr/lib/arm-linux-gnueabihf /opt/PandaBoard-SD/usr/lib/lapack /opt/PandaBoard-SD/usr/lib/libblas)

    SET (LIBS ${LIBS} glib-2.0)
    SET (CMAKE_EXE_LINKER_FLAGS "-Wl,-rpath-link=/opt/PandaBoard-SD/lib:/opt/PandaBoard-SD/usr/lib:/opt/PandaBoard-SD/lib/arm-linux-gnueabihf:/opt/PandaBoard-SD/usr/lib/arm-linux-gnueabihf")
ENDIF("${PANDABOARD}" STREQUAL "YES")

# Recipe for building "example4".
FILE(GLOB_RECURSE example4-sources "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")
ADD_EXECUTABLE (example4 ${example4-sources})
TARGET_LINK_LIBRARIES (example4 ${OPENDAVINCI_LIBS} ${LIBS} ${OpenCV_LIBS}) 

# Recipe for installing "example4".
INSTALL(TARGETS example4 RUNTIME DESTINATION bin) 
