#
# CaroloCup.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (2013-CaroloCup-proxy)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../cc-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "proxy".
FILE(GLOB_RECURSE proxy-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (2013-CaroloCup-proxylib STATIC ${proxy-sources})
ADD_EXECUTABLE (2013-CaroloCup-proxy "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (2013-CaroloCup-proxy 2013-CaroloCup-proxylib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "proxy".
INSTALL(TARGETS 2013-CaroloCup-proxy RUNTIME DESTINATION bin/2013/CaroloCup RENAME proxy) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE proxy-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(2013-CaroloCup-proxyTestSuite proxyTestSuite.cpp ${proxy-testsuites})
    TARGET_LINK_LIBRARIES(2013-CaroloCup-proxyTestSuite 2013-CaroloCup-proxylib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
