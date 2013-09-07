#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (sll)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (${libhesperia_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "sll".
FILE(GLOB_RECURSE sll-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (slllib STATIC ${sll-sources})
ADD_EXECUTABLE (sll "${CMAKE_CURRENT_SOURCE_DIR}/apps/sll.cpp")
TARGET_LINK_LIBRARIES (sll slllib hesperia ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "sll".
INSTALL(TARGETS sll RUNTIME DESTINATION bin)

