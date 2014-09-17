#
# CaroloCup 2013.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

# Export library name.
SET (CCDATA_LIBS cc-data)

# Add subdirectories.

# Library that contains data to be interchanged between OpenDaVINCI and hesperia, and the embedded software on the Discovery board.
ADD_SUBDIRECTORY (cc-data)

# Application for receiving and processing data from the Discovery board.
ADD_SUBDIRECTORY (sensorboard)
ADD_SUBDIRECTORY (testsensorboardsender)

# Application for sending driving commands.
ADD_SUBDIRECTORY (driver)

# Application for connecting to UDP_Server.
ADD_SUBDIRECTORY (proxy)
# carolocup2013 needs this proxy version only to be able to run on the meili car. since we will run carolocup2013 only on the simulation environment, we don't need the proxy.

# Application for sending driving commands.
ADD_SUBDIRECTORY (extradriver)

# Application for sending parking commands.
ADD_SUBDIRECTORY (parkingdriver)

# Disable QT and OpenCV apps on platforms missing those libraries.
IF((NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin") AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "FreeBSD") AND (NOT WIN32) AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "NetBSD") AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "OpenBSD"))
    ADD_SUBDIRECTORY (UDP_Server)

    # Application for recording the onboard video data.
    ADD_SUBDIRECTORY (VCR)

    # Application for receiving data from hesperia or a USB camera.
    ADD_SUBDIRECTORY (lanedetector)
ENDIF()

# Special target to simplify the rsync command.
IF("${PANDABOARD}" STREQUAL "YES")
ADD_CUSTOM_TARGET(push2meili-1-CaroloCup-2013
#    COMMAND rsync -ave ssh /opt/carolocup-ARM/ meili@192.168.0.100:/opt/carolocup/
    COMMAND rsync -ave ssh /opt/carolocup-ARM/ meili@192.168.0.100:/opt/msv/bin/carolocup/
    DEPENDS cc-data sensorboard lanedetector driver proxy
)

ADD_CUSTOM_TARGET(push2meili-2-CaroloCup-2013
#    COMMAND rsync -ave ssh /opt/carolocup-ARM/ meili@192.168.0.101:/opt/carolocup/
    COMMAND rsync -ave ssh /opt/carolocup-ARM/ meili@192.168.0.101:/opt/msv/bin/carolocup/
    DEPENDS cc-data sensorboard lanedetector driver proxy
)
ENDIF("${PANDABOARD}" STREQUAL "YES")

