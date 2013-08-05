#
# CaroloCup.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (2013-CaroloCup-testsensorboardsender)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../cc-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "testsensorboardsender".
FILE(GLOB_RECURSE testsensorboardsender-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (testsensorboardsenderlib STATIC ${testsensorboardsender-sources})
ADD_EXECUTABLE (testsensorboardsender "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (testsensorboardsender testsensorboardsenderlib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "testsensorboardsender".
INSTALL(TARGETS testsensorboardsender RUNTIME DESTINATION bin/2013/CaroloCup) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE testsensorboardsender-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(TestSensorBoardSenderTestSuite TestSensorBoardSenderTestSuite.cpp ${testsensorboardsender-testsuites})
    TARGET_LINK_LIBRARIES(TestSensorBoardSenderTestSuite testsensorboardsenderlib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
