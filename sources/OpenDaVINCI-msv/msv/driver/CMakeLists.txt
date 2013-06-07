#
# Mini-Smart-Vehicles.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (driver)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../msv-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "driver".
FILE(GLOB_RECURSE driver-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (driverlib STATIC ${driver-sources})
ADD_EXECUTABLE (driver "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (driver driverlib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "driver".
INSTALL(TARGETS driver RUNTIME DESTINATION bin) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE driver-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(driverTestSuite driverTestSuite.cpp ${driver-testsuites})
    TARGET_LINK_LIBRARIES(driverTestSuite driverlib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
