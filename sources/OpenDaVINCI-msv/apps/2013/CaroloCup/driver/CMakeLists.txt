#
# CaroloCup.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (2013-CaroloCup-driver)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../cc-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "driver".
FILE(GLOB_RECURSE driver-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (2013-CaroloCup-driverlib STATIC ${driver-sources})
ADD_EXECUTABLE (2013-CaroloCup-driver "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (2013-CaroloCup-driver 2013-CaroloCup-driverlib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "driver".
INSTALL(TARGETS 2013-CaroloCup-driver RUNTIME DESTINATION bin/2013/CaroloCup RENAME driver) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE driver-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(2013-CaroloCup-driverTestSuite driverTestSuite.cpp ${driver-testsuites})
    TARGET_LINK_LIBRARIES(2013-CaroloCup-driverTestSuite 2013-CaroloCup-driverlib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
