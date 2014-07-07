#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (recintegrity)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

# Recipe for building "recintegrity".
FILE(GLOB_RECURSE recintegrity-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (recintegritylib STATIC ${recintegrity-sources})
ADD_EXECUTABLE (recintegrity "${CMAKE_CURRENT_SOURCE_DIR}/apps/recintegrity.cpp")
TARGET_LINK_LIBRARIES (recintegrity recintegritylib ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "recintegrity".
INSTALL(TARGETS recintegrity RUNTIME DESTINATION bin) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE recintegrity-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(recintegrityTestSuite recintegrityTestSuite.cpp ${recintegrity-testsuites})
    TARGET_LINK_LIBRARIES(recintegrityTestSuite recintegritylib ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
