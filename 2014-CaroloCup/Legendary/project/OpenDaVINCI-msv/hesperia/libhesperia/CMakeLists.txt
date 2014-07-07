#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (libhesperia)

FIND_PACKAGE( Boost REQUIRED )
IF(NOT Boost_FOUND)
    MESSAGE("Boost is required to build hesperia.")
ENDIF(NOT Boost_FOUND)

IF(Boost_FOUND)
    LINK_DIRECTORIES ( ${Boost_LIBRARY_DIRS} )
    INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIRS})
ENDIF(Boost_FOUND)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

###############################################################################
# Collect all source files.
FILE(GLOB_RECURSE libhesperia-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")

###############################################################################
# Remove POSIX files on WIN32.
IF(WIN32)
    # Set flag for exporting symbols.
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DHESPERIA_EXPORTS")
    
    # Exclude POSIX files.
    FOREACH(item ${libhesperia-sources})
      IF(${item} MATCHES "POSIX.+.cpp")
        LIST(REMOVE_ITEM libhesperia-sources ${item})
      ENDIF(${item} MATCHES "POSIX.+.cpp")
    ENDFOREACH(item)
ENDIF(WIN32)

# Remove Boost files on UNIX if not available.
IF(UNIX)
    IF(NOT Boost_FOUND)
        MESSAGE("(libhesperia): Disabling Boost sources.")

        SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DHESPERIA_NOBOOST")

        # Exclude POSIX files.
        FOREACH(item ${libhesperia-sources})
          IF(${item} MATCHES "Boost.+.cpp")
            LIST(REMOVE_ITEM libhesperia-sources ${item})
          ENDIF(${item} MATCHES "Boost.+.cpp")
        ENDFOREACH(item)
    ENDIF(NOT Boost_FOUND)
ENDIF(UNIX)

###############################################################################
ADD_LIBRARY (hesperia STATIC ${libhesperia-sources})
TARGET_LINK_LIBRARIES(hesperia ${THIRDPARTY_LIBS})

###############################################################################
# Recipe for installing "libhesperia".
INSTALL(TARGETS hesperia DESTINATION lib)

# Install header files.
INSTALL(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/include/" DESTINATION include PATTERN ".svn" EXCLUDE PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

###############################################################################
# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
	FILE(GLOB_RECURSE libhesperia-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
	
	CXXTEST_ADD_TEST(HesperiaTestSuite HesperiaTestSuite.cpp ${libhesperia-testsuites})
	TARGET_LINK_LIBRARIES(HesperiaTestSuite hesperia ${OPENDAVINCI_LIBS} ${LIBS} ${THIRDPARTY_LIBS})
ENDIF(CXXTEST_FOUND)

