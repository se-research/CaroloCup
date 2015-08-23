# automotive.carolocup - Software for self-driving miniature vehicles
#                        for CaroloCup realized with OpenDaVINCI
# Copyright (C) 2012 - 2015 Christian Berger
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

PROJECT (automotive_carolocup)

###########################################################################
# Set the search path for .cmake files.
SET (CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake.Modules" ${CMAKE_MODULE_PATH})

###########################################################################
# Find and configure CxxTest.
SET (CXXTEST_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/cxxtest") 
INCLUDE (CheckCxxTestEnvironment)

###########################################################################
# Add subdirectories.

# Library that contains data to be interchanged between OpenDaVINCI and
# odSimulation, and the embedded software on the Arduino.
# TODO: Refactor to use .odvd
ADD_SUBDIRECTORY (msv-data)

# Application for receiving data from hesperia or a USB camera.
ADD_SUBDIRECTORY (lanedetector)

# Application for sending driving commands.
ADD_SUBDIRECTORY (driver)

# Application for connecting to the vehicle.
ADD_SUBDIRECTORY (proxy)

# Application for exporting data to CSV.
# TODO: Refactor to use a Visitor
ADD_SUBDIRECTORY (csvexporter)

# Add subdirectories for studies. These parts of the software will only
# be compiled and executed if libodsimulation is available.
ADD_SUBDIRECTORY (studies)

###########################################################################
# Enable CPack to create .deb and .rpm.
#
# Read version from first line of ChangeLog
FILE (STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/ChangeLog" BUILD_NUMBER)
MACRO (setup_package_version_variables _packageName)
        STRING (REGEX MATCHALL "[0-9]+" _versionComponents "${_packageName}")
        LIST (LENGTH _versionComponents _len)
        IF (${_len} GREATER 0)
            LIST(GET _versionComponents 0 MAJOR)
        ENDIF()
        IF (${_len} GREATER 1)
            LIST(GET _versionComponents 1 MINOR)
        ENDIF()
        IF (${_len} GREATER 2)
            LIST(GET _versionComponents 2 PATCH)
        ENDIF()
ENDMACRO()
setup_package_version_variables(${BUILD_NUMBER})

IF(    (UNIX)
   AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "DragonFly")
   AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "OpenBSD")
   AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "NetBSD") )
    SET(CPACK_GENERATOR "DEB;RPM")

    SET(CPACK_PACKAGE_CONTACT "Christian Berger")
    SET(CPACK_PACKAGE_VENDOR "${CPACK_PACKAGE_CONTACT}")
    SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "automotive.carolocup contains software to operate a self-driving miniature vehicle for CaroloCup realized with OpenDaVINCI.")
    SET(CPACK_PACKAGE_NAME "automotive.carolocup")
    SET(CPACK_PACKAGE_VERSION_MAJOR "${MAJOR}")
    SET(CPACK_PACKAGE_VERSION_MINOR "${MINOR}")
    SET(CPACK_PACKAGE_VERSION_PATCH "${PATCH}")
    SET(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
    SET(CPACK_COMPONENTS_ALL software)

    # Debian packages:
    SET(CPACK_DEBIAN_PACKAGE_SECTION "devel")
    SET(CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
    IF("${ARMHF}" STREQUAL "YES")
        SET(ARCH "armhf")
    ELSE()
        IF("${CMAKE_SIZEOF_VOID_P}" STREQUAL "8")
            SET(ARCH "amd64")
        ELSE()
            SET(ARCH "i386")
        ENDIF()
    ENDIF()
    SET(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "${ARCH}")
    SET(CPACK_DEB_COMPONENT_INSTALL ON)
    SET(CPACK_DEBIAN_PACKAGE_DEPENDS "opendavinci-odlib,odsimulation-odsimlib,libautomotivedata")

    # RPM packages:
    IF("${ARMHF}" STREQUAL "YES")
        SET(ARCH "armhf")
    ELSE()
        IF("${CMAKE_SIZEOF_VOID_P}" STREQUAL "8")
            SET(ARCH "x86_64")
        ELSE()
            SET(ARCH "i686")
        ENDIF()
    ENDIF()
    SET(CPACK_RPM_PACKAGE_BUILDARCH "Buildarch: ${CPACK_RPM_PACKAGE_ARCHITECTURE}")
    SET(CPACK_RPM_COMPONENT_INSTALL ON)
    SET(CPACK_RPM_PACKAGE_LICENSE "GPL")
    SET(CPACK_RPM_PACKAGE_REQUIRES "opendavinci-odlib, odsimulation-odsimlib, libautomotivedata")

    # Resulting package name:
    SET(CPACK_PACKAGE_FILE_NAME ${CPACK_PACKAGE_NAME}_${CPACK_PACKAGE_VERSION}_${ARCH})
ENDIF()

INCLUDE(CPack)

# Left over from 2013's Meili to push binaries to the vehicle.
## Special target to simplify the rsync command.
#IF("${PANDABOARD}" STREQUAL "YES")
#ADD_CUSTOM_TARGET(push2meili-1-CaroloCup-2015
##    COMMAND rsync -ave ssh /opt/carolocup-ARM/ meili@192.168.0.100:/opt/carolocup/
#    COMMAND rsync -ave ssh /opt/carolocup-ARM/ meili@192.168.0.100:/opt/msv/bin/carolocup/
#    DEPENDS cc-data sensorboard lanedetector driver proxy
#)

#ADD_CUSTOM_TARGET(push2meili-2-CaroloCup-2015
##    COMMAND rsync -ave ssh /opt/carolocup-ARM/ meili@192.168.0.101:/opt/carolocup/
#    COMMAND rsync -ave ssh /opt/carolocup-ARM/ meili@192.168.0.101:/opt/msv/bin/carolocup/
#    DEPENDS cc-data sensorboard lanedetector driver proxy
#)
#ENDIF("${PANDABOARD}" STREQUAL "YES")

