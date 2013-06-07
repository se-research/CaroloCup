#
# CaroloCup.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (cc-data)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "cc-data".
FILE(GLOB_RECURSE cc-data-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (cc-data STATIC ${cc-data-sources})

# Recipe for installing "cc-data".
INSTALL(TARGETS cc-data DESTINATION lib)

# Install header files.
INSTALL(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/include/" DESTINATION include/cc-data PATTERN ".svn" EXCLUDE PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE cc-data-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(cc-dataTestSuite cc-dataTestSuite.cpp ${cc-data-testsuites})
    TARGET_LINK_LIBRARIES(cc-dataTestSuite cc-data ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)

