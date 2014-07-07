#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (supercomponent)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "supercomponent".
FILE(GLOB_RECURSE supercomponent-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (supercomponentlib STATIC ${supercomponent-sources})
ADD_EXECUTABLE (supercomponent "${CMAKE_CURRENT_SOURCE_DIR}/apps/supercomponent.cpp")
TARGET_LINK_LIBRARIES (supercomponent supercomponentlib ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "supercomponent".
INSTALL(TARGETS supercomponent RUNTIME DESTINATION bin)
INSTALL_FILES(/bin FILES configuration)

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE supercomponent-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(SupercomponentTestSuite SupercomponentTestSuite.cpp ${supercomponent-testsuites})
    TARGET_LINK_LIBRARIES(SupercomponentTestSuite supercomponentlib ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
