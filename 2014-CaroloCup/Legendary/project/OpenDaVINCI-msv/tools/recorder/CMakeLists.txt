#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (recorder)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "recorder".
FILE(GLOB_RECURSE recorder-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (recorderlib STATIC ${recorder-sources})
ADD_EXECUTABLE (recorder "${CMAKE_CURRENT_SOURCE_DIR}/apps/recorder.cpp")
TARGET_LINK_LIBRARIES (recorder recorderlib ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "recorder".
INSTALL(TARGETS recorder RUNTIME DESTINATION bin) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE recorder-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(RecorderTestSuite RecorderTestSuite.cpp ${recorder-testsuites})
    TARGET_LINK_LIBRARIES(RecorderTestSuite recorderlib ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
