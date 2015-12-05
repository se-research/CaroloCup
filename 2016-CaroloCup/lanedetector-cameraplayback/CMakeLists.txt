# CaroloCup-CameraPlayback - Simple application realized with
#                            OpenDaVINCI to playback recordings
#                            captured from our CaroloCup miniature
#                            cars.
# Copyright (C) 2015 Hugo Andrade, Christian Berger, Federico Giaimo.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

CMAKE_MINIMUM_REQUIRED (VERSION 2.8)

PROJECT (lanedetector-cameraplayback)

###########################################################################
# Set the search path for .cmake files.
SET (CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake.Modules" ${CMAKE_MODULE_PATH})

###########################################################################
# Find OpenDaVINCI.
FIND_PACKAGE (OpenDaVINCI REQUIRED)

###########################################################################
# Find OpenCV.
SET(OpenCV_DIR "/usr")
FIND_PACKAGE (OpenCV REQUIRED)

###########################################################################
# Set linking libraries to successfully link test suites and binaries.
SET (LIBRARIES ${OPENDAVINCI_LIBRARIES} ${OpenCV_LIBS})

###########################################################################
# Set header files from OpenDaVINCI.
INCLUDE_DIRECTORIES (${OPENDAVINCI_INCLUDE_DIRS}/opendavinci)
# Set header files from OpenCV.
INCLUDE_DIRECTORIES (${OpenCV_INCLUDE_DIR})
# Set include directory.
INCLUDE_DIRECTORIES(include)

###############################################################################
# Build this project.
ADD_EXECUTABLE (lanedetector-cameraplayback "lanedetector-cameraplayback.cpp")
TARGET_LINK_LIBRARIES (lanedetector-cameraplayback ${LIBRARIES})

###############################################################################
# Install this project.
INSTALL(TARGETS lanedetector-cameraplayback RUNTIME DESTINATION bin COMPONENT software)

