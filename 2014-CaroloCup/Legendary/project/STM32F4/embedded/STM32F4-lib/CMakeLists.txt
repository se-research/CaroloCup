#
# STM32F4 Discovery board toolchain file.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (libstm32f4)

SET (CMAKE_C_FLAGS "-g -O2 -Wall -mlittle-endian -mthumb -mthumb-interwork -mcpu=cortex-m4 -msoft-float -ffreestanding -nostdlib")

INCLUDE_DIRECTORIES (include)
INCLUDE_DIRECTORIES (include/core)
INCLUDE_DIRECTORIES (include/peripherals)

SET (STM32F4-lib_SOURCES src/peripherals/misc.c src/peripherals/stm32f4xx_dma.c src/peripherals/stm32f4xx_rcc.c src/peripherals/stm32f4xx_adc.c src/peripherals/stm32f4xx_exti.c src/peripherals/stm32f4xx_rng.c src/peripherals/stm32f4xx_can.c src/peripherals/stm32f4xx_flash.c src/peripherals/stm32f4xx_rtc.c src/peripherals/stm32f4xx_crc.c src/peripherals/stm32f4xx_fsmc.c src/peripherals/stm32f4xx_sdio.c src/peripherals/stm32f4xx_cryp_aes.c src/peripherals/stm32f4xx_gpio.c src/peripherals/stm32f4xx_spi.c src/peripherals/stm32f4xx_cryp.c src/peripherals/stm32f4xx_hash.c src/peripherals/stm32f4xx_syscfg.c src/peripherals/stm32f4xx_cryp_des.c src/peripherals/stm32f4xx_hash_md5.c src/peripherals/stm32f4xx_tim.c src/peripherals/stm32f4xx_cryp_tdes.c src/peripherals/stm32f4xx_hash_sha1.c src/peripherals/stm32f4xx_usart.c src/peripherals/stm32f4xx_dac.c src/peripherals/stm32f4xx_i2c.c src/peripherals/stm32f4xx_wwdg.c src/peripherals/stm32f4xx_dbgmcu.c src/peripherals/stm32f4xx_iwdg.c src/peripherals/stm32f4xx_dcmi.c src/peripherals/stm32f4xx_pwr.c)

ADD_LIBRARY (stm32f4 STATIC ${STM32F4-lib_SOURCES})
INSTALL (TARGETS stm32f4 DESTINATION lib)

