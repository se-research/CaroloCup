#
# Mini-Smart-Vehicles.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (msv-data)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "msv-data".
FILE(GLOB_RECURSE msv-data-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (msv-data STATIC ${msv-data-sources})

# Recipe for installing "msv-data".
INSTALL(TARGETS msv-data DESTINATION lib)

# Install header files.
INSTALL(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/include/" DESTINATION include/msv-data PATTERN ".svn" EXCLUDE PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE msv-data-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(msv-dataTestSuite msv-dataTestSuite.cpp ${msv-data-testsuites})
    TARGET_LINK_LIBRARIES(msv-dataTestSuite msv-data ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)

