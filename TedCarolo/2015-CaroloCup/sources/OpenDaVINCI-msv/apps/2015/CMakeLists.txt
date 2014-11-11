#
# Mini-Smart-Vehicles.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

# Export library name.
SET (MSVDATA_LIBS msv-data)
SET (HESPERIA_LIBS hesperia)
SET (VEHICLECONTEXT_LIBS vehiclecontext)

# Add subdirectories.

# Library that contains data to be interchanged between OpenDaVINCI and hesperia, and the embedded software on the Discovery board.
ADD_SUBDIRECTORY (msv-data)

# Application for sending driving commands.
ADD_SUBDIRECTORY (driver)

# Application for connecting to UDP_Server.
#ADD_SUBDIRECTORY (proxy-meili)

# Application for generating csv files of lane detection.
ADD_SUBDIRECTORY (csvexporter)

# Disable QT and OpenCV apps on platforms missing those libraries.
IF((NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin") AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "FreeBSD") AND (NOT WIN32) AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "NetBSD") AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "OpenBSD"))
    # Application for connecting to STM32F4 discovery board.
  #  ADD_SUBDIRECTORY (proxy-STM32F4) # Requires POSIXSerialPortFactory

    # Application for receiving data from hesperia or a USB camera.
    ADD_SUBDIRECTORY (lanedetector)

    # Application for connecting to a serial embedded software/hardware board (STM32F4 Discovery Board or Arduino).
    ADD_SUBDIRECTORY (proxy)

    # Application to record live streams.
    ADD_SUBDIRECTORY (VCR)
ENDIF()

# Special target to simplify the rsync command.
IF("${PANDABOARD}" STREQUAL "YES")
ADD_CUSTOM_TARGET(push2meili-1
#    COMMAND rsync -ave ssh /opt/msv-ARM/ meili@192.168.0.100:/opt/msv/
    COMMAND rsync -ave ssh /opt/msv-ARM/ meili@192.168.0.100:/opt/msv/bin/msv
    DEPENDS cc-data sensorboard lanedetector driver proxy
)

ADD_CUSTOM_TARGET(push2meili-2
#    COMMAND rsync -ave ssh /opt/msv-ARM/ meili@192.168.0.101:/opt/msv/
    COMMAND rsync -ave ssh /opt/msv-ARM/ meili@192.168.0.101:/opt/msv/bin/msv
    DEPENDS cc-data sensorboard lanedetector driver proxy
)
ENDIF("${PANDABOARD}" STREQUAL "YES")


