#
# Mini-Smart-Vehicles.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

# Export library name.
SET (MSVDATA_LIBS msv-data)

# Add subdirectories.

# Library that contains data to be interchanged between OpenDaVINCI and hesperia, and the embedded software on the Discovery board.
ADD_SUBDIRECTORY (msv-data)

# Application for receiving data from hesperia or a USB camera.
ADD_SUBDIRECTORY (lanedetector)

# Application for sending driving commands.
ADD_SUBDIRECTORY (driver)

# Application for connecting to UDP_Server.
ADD_SUBDIRECTORY (proxy)

# Application for connecting to STM32F4 discovery board.
ADD_SUBDIRECTORY (proxy-STM32F4)

# Application to wrap the motor controller.
ADD_SUBDIRECTORY (UDP_Server)

# Application to record live streams.
ADD_SUBDIRECTORY (VCR)

# Special target to simplify the rsync command.
IF("${PANDABOARD}" STREQUAL "YES")
ADD_CUSTOM_TARGET(push2meili-1
    COMMAND rsync -ave ssh /opt/msv-ARM/ meili@192.168.0.100:/opt/msv/
    DEPENDS cc-data sensorboard lanedetector driver proxy
)

ADD_CUSTOM_TARGET(push2meili-2
    COMMAND rsync -ave ssh /opt/msv-ARM/ meili@192.168.0.101:/opt/msv/
    DEPENDS cc-data sensorboard lanedetector driver proxy
)
ENDIF("${PANDABOARD}" STREQUAL "YES")


