#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

INCLUDE(CheckIncludeFiles)
INCLUDE(CheckIncludeFileCXX)

# On WIN32 platforms Boost is required. On UNIX, POSIX can be used safely instead.
IF(WIN32)
    FIND_PACKAGE( Boost REQUIRED COMPONENTS iostreams system thread )
    IF(NOT Boost_FOUND)
        MESSAGE("Boost is required on Windows platforms.")
    ENDIF(NOT Boost_FOUND)
ENDIF(WIN32)

IF(Boost_FOUND)
    LINK_DIRECTORIES ( ${Boost_LIBRARY_DIRS} )
    INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIRS})
    SET (LIBS ${LIBS} ${Boost_IOSTREAMS_LIBRARY})
    SET (LIBS ${LIBS} ${Boost_SYSTEM_LIBRARY})
    SET (LIBS ${LIBS} ${Boost_THREAD_LIBRARY})
ENDIF(Boost_FOUND)

IF(UNIX)
    IF("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
        SET (THIRDPARTY_LIBS pthread rt)
    ELSE("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
        SET (THIRDPARTY_LIBS pthread)
    ENDIF("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
ENDIF(UNIX)

# Set 3rd party libraries shipped with this distribution.
SET (THIRDPARTY_LIBS ${THIRDPARTY_LIBS} ziplib zlib dblib)

# Set libraries to link.
SET (OPENDAVINCI_LIBS opendavinci)
