#
# Mini-Smart-Vehicles.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (2013-DIT-168-project-template-driver)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../msv-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "driver".
FILE(GLOB_RECURSE driver-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/Driver.cpp")
ADD_LIBRARY (driverlib STATIC ${driver-sources})
ADD_EXECUTABLE (driver "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (driver driverlib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for building "lanedriver".

FILE(GLOB_RECURSE ldriver-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/laneDriver.cpp")
ADD_LIBRARY (ldriverlib STATIC ${ldriver-sources})
ADD_EXECUTABLE (lanedriver "${CMAKE_CURRENT_SOURCE_DIR}/apps/laneDriver_module.cpp")
TARGET_LINK_LIBRARIES (lanedriver ldriverlib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})

# Recipe for installing "driver".
INSTALL(TARGETS driver RUNTIME DESTINATION bin/2013/DIT-168/project-template) 
INSTALL(TARGETS lanedriver RUNTIME DESTINATION bin/2013/DIT-168/project-template) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB driver-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    FOREACH(testsuite ${driver-testsuites})
        STRING(REPLACE "/" ";" testsuite-list ${testsuite})

        LIST(LENGTH testsuite-list len)
        MATH(EXPR lastItem "${len}-1")
        LIST(GET testsuite-list "${lastItem}" testsuite-short)

	    CXXTEST_ADD_TEST(${testsuite-short}-TestSuite ${testsuite-short}-TestSuite.cpp ${testsuite})
	    TARGET_LINK_LIBRARIES(${testsuite-short}-TestSuite driverlib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
    ENDFOREACH()
ENDIF(CXXTEST_FOUND)
