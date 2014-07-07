#
# Example to light up the two LEDs around the MEMS sensor of the STM32F4 Discovery board.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (LED2)

# Enable assembler to build object files from .s files.
ENABLE_LANGUAGE(ASM)

SET(CMAKE_C_FLAGS "-std=gnu99 -g -O2 -Wall -T${libstm32f4_SOURCE_DIR}/stm32_flash.ld -mlittle-endian -mthumb -mthumb-interwork -nostartfiles -mcpu=cortex-m4 -msoft-float")

# Add the STM32F4 peripherals include directories.
INCLUDE_DIRECTORIES (${libstm32f4_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (${libstm32f4_SOURCE_DIR}/include/core)
INCLUDE_DIRECTORIES (${libstm32f4_SOURCE_DIR}/include/peripherials)

# Add the STM32F4 peripherals library.
SET (STM32F4_LIBS stm32f4)

# Recipe for building "LED2".
SET(LED2_SOURCES src/main.c )

ADD_EXECUTABLE (LED2.elf ${LED2_SOURCES} ${libstm32f4_SOURCE_DIR}/startup_stm32f4xx.s)
TARGET_LINK_LIBRARIES (LED2.elf ${STM32F4_LIBS}) 

ADD_CUSTOM_TARGET(LED2.hex ALL DEPENDS LED2.elf COMMAND ${CMAKE_OBJCOPY} -Oihex LED2.elf LED2.hex)
ADD_CUSTOM_TARGET(LED2.bin ALL DEPENDS LED2.elf COMMAND ${CMAKE_OBJCOPY} -Obinary LED2.elf LED2.bin)

# Recipe for installing "LED2".
INSTALL_FILES(/bin FILES LED2.bin)

