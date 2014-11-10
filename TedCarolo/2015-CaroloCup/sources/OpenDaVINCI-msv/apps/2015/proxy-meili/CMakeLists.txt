#
# Mini-Smart-Vehicles.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (2013-DIT-168-project-template-proxymeili)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../msv-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "proxymeili".
FILE(GLOB_RECURSE proxymeili-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (proxymeililib STATIC ${proxymeili-sources})
ADD_EXECUTABLE (proxymeili "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (proxymeili proxymeililib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "proxymeili".
INSTALL(TARGETS proxymeili RUNTIME DESTINATION bin/2013/DIT-168/project-template) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB proxymeili-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    FOREACH(testsuite ${proxymeili-testsuites})
        STRING(REPLACE "/" ";" testsuite-list ${testsuite})

        LIST(LENGTH testsuite-list len)
        MATH(EXPR lastItem "${len}-1")
        LIST(GET testsuite-list "${lastItem}" testsuite-short)

	    CXXTEST_ADD_TEST(proxymeili-${testsuite-short}-TestSuite ${testsuite-short}-TestSuite.cpp ${testsuite})
	    TARGET_LINK_LIBRARIES(proxymeili-${testsuite-short}-TestSuite proxymeililib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
    ENDFOREACH()
ENDIF(CXXTEST_FOUND)
