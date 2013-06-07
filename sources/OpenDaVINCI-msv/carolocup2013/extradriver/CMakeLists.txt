#
# CaroloCup.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (extradriver)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (../cc-data/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "extradriver".
FILE(GLOB_RECURSE extradriver-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (extradriverlib STATIC ${extradriver-sources})
ADD_EXECUTABLE (extradriver "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (extradriver extradriverlib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "extradriver".
INSTALL(TARGETS extradriver RUNTIME DESTINATION bin) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE extradriver-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(extradriverTestSuite extradriverTestSuite.cpp ${extradriver-testsuites})
    TARGET_LINK_LIBRARIES(extradriverTestSuite extradriverlib ${CCDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
