#
# STM32F4 Discovery board embedded projects.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

CMAKE_MINIMUM_REQUIRED (VERSION 2.6)

PROJECT (STM32F4-embedded)

# Add subdirectories.
IF("${STM32F4}" STREQUAL "YES")
    ADD_SUBDIRECTORY (STM32F4-lib)
    ADD_SUBDIRECTORY (examples)
ENDIF("${STM32F4}" STREQUAL "YES")

