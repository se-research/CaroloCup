#
# Mini-Smart-Vehicles.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (2013-DIT-168-project-template-proxy-STM32F4)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../msv-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "proxy-STM32F4".
FILE(GLOB_RECURSE proxy-STM32F4-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (proxy-STM32F4lib STATIC ${proxy-STM32F4-sources})
ADD_EXECUTABLE (proxy-STM32F4 "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
ADD_EXECUTABLE (STM32F4Test2 "${CMAKE_CURRENT_SOURCE_DIR}/apps/STM32F4Test2.cpp")
ADD_EXECUTABLE (STM32F4Test3 "${CMAKE_CURRENT_SOURCE_DIR}/apps/STM32F4Test3.cpp")
ADD_EXECUTABLE (STM32F4WrapperApp "${CMAKE_CURRENT_SOURCE_DIR}/apps/STM32F4WrapperApp.cpp")
TARGET_LINK_LIBRARIES (proxy-STM32F4 proxy-STM32F4lib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 
TARGET_LINK_LIBRARIES (STM32F4Test2 proxy-STM32F4lib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
TARGET_LINK_LIBRARIES (STM32F4Test3 proxy-STM32F4lib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
TARGET_LINK_LIBRARIES (STM32F4WrapperApp proxy-STM32F4lib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})

# Recipe for installing "proxy-STM32F4".
INSTALL(TARGETS proxy-STM32F4 RUNTIME DESTINATION bin/2013/DIT-168/project-template) 
INSTALL(TARGETS STM32F4Test2 RUNTIME DESTINATION bin/2013/DIT-168/project-template) 
INSTALL(TARGETS STM32F4Test3 RUNTIME DESTINATION bin/2013/DIT-168/project-template) 
INSTALL(TARGETS STM32F4WrapperApp RUNTIME DESTINATION bin/2013/DIT-168/project-template) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB proxy-STM32F4-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
       
    FOREACH(testsuite ${proxy-STM32F4-testsuites})
        STRING(REPLACE "/" ";" testsuite-list ${testsuite})

        LIST(LENGTH testsuite-list len)
        MATH(EXPR lastItem "${len}-1")
        LIST(GET testsuite-list "${lastItem}" testsuite-short)

	    CXXTEST_ADD_TEST(${testsuite-short}-TestSuite ${testsuite-short}-TestSuite.cpp ${testsuite})
	    TARGET_LINK_LIBRARIES(${testsuite-short}-TestSuite proxy-STM32F4lib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
    ENDFOREACH()
ENDIF(CXXTEST_FOUND)
