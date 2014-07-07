#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (types)

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
INCLUDE_DIRECTORIES (${libhesperia_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "types".
FILE(GLOB_RECURSE types-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (typeslib STATIC ${types-sources})
ADD_EXECUTABLE (types "${CMAKE_CURRENT_SOURCE_DIR}/apps/types.cpp")
TARGET_LINK_LIBRARIES (types typeslib hesperia ${OPENDAVINCI_LIBS} ${LIBS})

add_custom_command(
  OUTPUT stamped_file.lic
  COMMAND types any other arguments
#  DEPENDS any/dependency.file
  COMMENT "Stamping the license"
  VERBATIM
)

add_custom_target(
  StampTheLicense ALL
  DEPENDS stamped_file.lic
)

# Recipe for installing "types".
INSTALL(TARGETS types RUNTIME DESTINATION bin)

