#
# UDP_Server written by Benjamin Vedder.
#

PROJECT (UDP_Server)

# Qt4 libraries.
FIND_PACKAGE(Qt4 REQUIRED)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../msv-data/include)
INCLUDE_DIRECTORIES (include)

# Libraries to link against.
SET (LIBS ${LIBS} QtNetwork QtCore)

IF("${PANDABOARD}" STREQUAL "YES")
    SET (CMAKE_C_FLAGS "-march=armv7-a -pipe -O2 -Wall -W -D_REENTRANT -DQT_WEBKIT -DQT_NO_DEBUG -DQT_NETWORK_LIB -DQT_CORE_LIB -DQT_SHARED -D PANDABOARD=1")
    SET (CMAKE_CXX_FLAGS "-march=armv7-a -pipe -O2 -Wall -W -D_REENTRANT -DQT_WEBKIT -DQT_NO_DEBUG -DQT_NETWORK_LIB -DQT_CORE_LIB -DQT_SHARED -D PANDABOARD=1")

    INCLUDE_DIRECTORIES (${PANDABOARD-ROOT}/usr/include)
    LINK_DIRECTORIES (${PANDABOARD-ROOT}/lib ${PANDABOARD-ROOT}/usr/lib ${PANDABOARD-ROOT}/usr/lib/arm-linux-gnueabi ${PANDABOARD-ROOT}/usr/lib/lapack ${PANDABOARD-ROOT}/usr/lib/libblas)

    SET (LIBS ${LIBS} glib-2.0)
    SET (CMAKE_EXE_LINKER_FLAGS "-Wl,-rpath-link=${PANDABOARD-ROOT}/lib:${PANDABOARD-ROOT}/usr/lib:${PANDABOARD-ROOT}/lib/arm-linux-gnueabi:${PANDABOARD-ROOT}/usr/lib/arm-linux-gnueabi")
ELSE("${PANDABOARD}" STREQUAL "YES")
    SET (CMAKE_C_FLAGS "-pipe -ansi -Wall")
    SET (CMAKE_CXX_FLAGS "-pipe -ansi -Wall")
ENDIF("${PANDABOARD}" STREQUAL "YES")

# Recipe for building "UDP_Server".
SET(UDP_Server_SOURCES main.cpp serialport.cpp udpserver.cpp)
SET(UDP_Server_HEADERS serialport.h udpserver.h)

# Generate the meta object files.
QT4_WRAP_CPP(UDP_Server_HEADERS_MOC ${UDP_Server_HEADERS})
SET(QT_USE_QTNETWORK TRUE)

# Include the Qt4 libraries definitions.
INCLUDE(${QT_USE_FILE})
ADD_DEFINITIONS(${QT_DEFINITIONS})

ADD_EXECUTABLE(UDP_Server ${UDP_Server_SOURCES} ${UDP_Server_HEADERS_MOC})
TARGET_LINK_LIBRARIES(UDP_Server ${LIBS})

# Recipe for installing "UDP_Server".
INSTALL(TARGETS UDP_Server RUNTIME DESTINATION bin) 

