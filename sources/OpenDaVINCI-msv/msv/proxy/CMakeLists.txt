#
# Mini-Smart-Vehicles.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (proxy)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../msv-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "proxy".
FILE(GLOB_RECURSE proxy-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (proxylib STATIC ${proxy-sources})
ADD_EXECUTABLE (proxy "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (proxy proxylib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "proxy".
INSTALL(TARGETS proxy RUNTIME DESTINATION bin) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE proxy-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(proxyTestSuite proxyTestSuite.cpp ${proxy-testsuites})
    TARGET_LINK_LIBRARIES(proxyTestSuite proxylib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
