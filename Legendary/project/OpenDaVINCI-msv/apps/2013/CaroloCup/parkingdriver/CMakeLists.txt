#
# CaroloCup.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (2013-CaroloCup-parkingdriver)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../cc-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "parkingdriver".
FILE(GLOB_RECURSE parkingdriver-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (parkingdriverlib STATIC ${parkingdriver-sources})
ADD_EXECUTABLE (parkingdriver "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (parkingdriver parkingdriverlib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "parkingdriver".
INSTALL(TARGETS parkingdriver RUNTIME DESTINATION bin/2013/CaroloCup) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE parkingdriver-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(parkingdriverTestSuite parkingdriverTestSuite.cpp ${parkingdriver-testsuites})
    TARGET_LINK_LIBRARIES(parkingdriverTestSuite parkingdriverlib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
