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
ADD_EXECUTABLE (STM32F4Test "${CMAKE_CURRENT_SOURCE_DIR}/apps/STM32F4Test.cpp")
TARGET_LINK_LIBRARIES (proxy-STM32F4 proxy-STM32F4lib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 
TARGET_LINK_LIBRARIES (STM32F4Test proxy-STM32F4lib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS}) 

# Recipe for installing "proxy-STM32F4".
INSTALL(TARGETS proxy-STM32F4 RUNTIME DESTINATION bin/2013/DIT-168/project-template) 
INSTALL(TARGETS STM32F4Test RUNTIME DESTINATION bin/2013/DIT-168/project-template) 

# Enable CxxTest for all available testsuites.
IF(CXXTEST_FOUND)
    FILE(GLOB_RECURSE proxy-STM32F4-testsuites "${CMAKE_CURRENT_SOURCE_DIR}/testsuites/*.h")
    
    CXXTEST_ADD_TEST(proxy-STM32F4TestSuite proxy-STM32F4TestSuite.cpp ${proxy-STM32F4-testsuites})
    TARGET_LINK_LIBRARIES(proxy-STM32F4TestSuite proxy-STM32F4lib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS})
ENDIF(CXXTEST_FOUND)
