#
# Mini-Smart-Vehicles.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (2013-DIT-168-project-template-msv-data)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "msv-data".
FILE(GLOB_RECURSE msv-data-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/generated/msv/*.cpp")
ADD_LIBRARY (msv-data STATIC ${msv-data-sources})

# Recipe for installing "msv-data".
INSTALL(TARGETS msv-data DESTINATION lib/2013/DIT-168/project-template)

# Install header files.
INSTALL(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/include/" DESTINATION include/2013/DIT-168/project-template/msv-data PATTERN ".svn" EXCLUDE PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB msv-data-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    FOREACH(testsuite ${msv-data-testsuites})
        STRING(REPLACE "/" ";" testsuite-list ${testsuite})

        LIST(LENGTH testsuite-list len)
        MATH(EXPR lastItem "${len}-1")
        LIST(GET testsuite-list "${lastItem}" testsuite-short)

	    CXXTEST_ADD_TEST(${testsuite-short}-TestSuite ${testsuite-short}-TestSuite.cpp ${testsuite})
	    TARGET_LINK_LIBRARIES(${testsuite-short}-TestSuite msv-data ${OPENDAVINCI_LIBS} ${LIBS})
    ENDFOREACH()
ENDIF(CXXTEST_FOUND)

