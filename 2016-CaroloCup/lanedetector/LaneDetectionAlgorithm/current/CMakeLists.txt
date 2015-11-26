cmake_minimum_required(VERSION 2.8)
project(LaneDetectionAlgorithm)
SET (CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../cmake.Modules" ${CMAKE_MODULE_PATH})

# Find OpenCV.
SET(OpenCV_DIR "/usr")
FIND_PACKAGE (OpenCV REQUIRED)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES main.cpp main.h)
add_executable(LaneDetectionAlgorithm ${SOURCE_FILES})
target_link_libraries( LaneDetectionAlgorithm ${OpenCV_LIBS})



