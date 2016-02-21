cmake_minimum_required(VERSION 2.8)
project(CaroloCup-CameraPlayback)
SET (CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../cmake.Modules" ${CMAKE_MODULE_PATH})

# Find OpenCV.
SET(OpenCV_DIR "/usr")
FIND_PACKAGE (OpenCV REQUIRED)

###########################################################################
# Find OpenDaVINCI.
FIND_PACKAGE (OpenDaVINCI REQUIRED)

###########################################################################
# Set linking libraries to successfully link test suites and binaries.
SET (LIBRARIES ${OPENDAVINCI_LIBRARIES} ${OpenCV_LIBS})

###########################################################################
# Set header files from OpenDaVINCI.
INCLUDE_DIRECTORIES (${OPENDAVINCI_INCLUDE_DIRS})
# Set header files from OpenCV.
INCLUDE_DIRECTORIES (${OpenCV_INCLUDE_DIR})
# Set include directory.
INCLUDE_DIRECTORIES(include)
# Set compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

FILE(GLOB_RECURSE thisproject-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp" "${CMAKE_CURRENT_SOURCE_DIR}/include/*.h")
set(SOURCE_FILES "${CMAKE_CURRENT_SOURCE_DIR}/apps/CaroloCup-CameraPlayback.cpp" ${thisproject-sources})

add_executable(CaroloCup-CameraPlayback ${SOURCE_FILES})
target_link_libraries( CaroloCup-CameraPlayback ${LIBRARIES})
