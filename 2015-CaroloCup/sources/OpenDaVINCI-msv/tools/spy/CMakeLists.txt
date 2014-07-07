#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (spy)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "spy".
FILE(GLOB_RECURSE spy-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (spylib STATIC ${spy-sources})
ADD_EXECUTABLE (spy "${CMAKE_CURRENT_SOURCE_DIR}/apps/spy.cpp")
TARGET_LINK_LIBRARIES (spy spylib ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "spy".
INSTALL(TARGETS spy RUNTIME DESTINATION bin) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE spy-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(SpyTestSuite SpyTestSuite.cpp ${spy-testsuites})
    TARGET_LINK_LIBRARIES(SpyTestSuite spylib ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
