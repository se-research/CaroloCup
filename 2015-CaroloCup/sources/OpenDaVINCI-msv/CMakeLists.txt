#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

CMAKE_MINIMUM_REQUIRED (VERSION 2.6)

PROJECT (OPENDAVINCI)

# Set compile flags.
IF(UNIX)
    SET (CMAKE_C_FLAGS ${CMAKE_C_FLAGS} "-fPIC")
    SET (CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} "-Wno-deprecated -Wall -Werror -Wextra -Wfloat-equal -Wshadow -Wpointer-arith -Wwrite-strings -Wpacked -Wredundant-decls -Wmissing-format-attribute -ansi -pipe")

    # Remove "-ansi" flag for clang on Darwin.
    IF(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
        SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ansi")
    ENDIF(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")

    # Additionally, test for Effective C++ warnings except on OpenBSD.
    IF(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "OpenBSD")
        SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-error=effc++ -Weffc++")
    ENDIF(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "OpenBSD")
ENDIF(UNIX)

# Set the search path for .cmake files.
SET (CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/_buildtools" ${CMAKE_MODULE_PATH})

######################################################################################

INCLUDE (FindRequiredLibraries)

######################################################################################

# Find CxxTest.
SET (CXXTEST_USE_PYTHON true)
SET (CXXTEST_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/_buildtools/cxxtest") 
FIND_PACKAGE(CxxTest)
IF(CXXTEST_FOUND)
    INCLUDE_DIRECTORIES(${CXXTEST_INCLUDE_DIR})
    ENABLE_TESTING()
ENDIF(CXXTEST_FOUND)

######################################################################################

# Add subdirectories.
ADD_SUBDIRECTORY (3rdParty)
ADD_SUBDIRECTORY (libopendavinci)
ADD_SUBDIRECTORY (hesperia)
ADD_SUBDIRECTORY (supercomponent)
ADD_SUBDIRECTORY (tools)
ADD_SUBDIRECTORY (examples)

# Apps contains msv, carolocup2013, ...
ADD_SUBDIRECTORY (apps)

