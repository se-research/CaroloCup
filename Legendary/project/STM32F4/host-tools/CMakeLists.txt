#
# STM32F4 Discovery board host-tools.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

CMAKE_MINIMUM_REQUIRED (VERSION 2.6)

PROJECT (STM32F4-host-tools)

# Set the search path for .cmake files.
SET (CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/_buildtools" ${CMAKE_MODULE_PATH})

# Add subdirectories.
ADD_SUBDIRECTORY (qstlink2)

