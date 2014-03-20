################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../TC_driver.c \
../clksys_driver.c \
../crc.c \
../dead_reckoning.c \
../ir_atlas.c \
../ir_jvc.c \
../ir_nec.c \
../ir_rc5.c \
../main.c \
../mc_driver.c \
../nine_dof.c \
../sensors.c \
../servo.c \
../uart_bldc.c \
../uart_bt.c \
../uart_bus.c \
../uart_ft.c 

OBJS += \
./TC_driver.o \
./clksys_driver.o \
./crc.o \
./dead_reckoning.o \
./ir_atlas.o \
./ir_jvc.o \
./ir_nec.o \
./ir_rc5.o \
./main.o \
./mc_driver.o \
./nine_dof.o \
./sensors.o \
./servo.o \
./uart_bldc.o \
./uart_bt.o \
./uart_bus.o \
./uart_ft.o 

C_DEPS += \
./TC_driver.d \
./clksys_driver.d \
./crc.d \
./dead_reckoning.d \
./ir_atlas.d \
./ir_jvc.d \
./ir_nec.d \
./ir_rc5.d \
./main.d \
./mc_driver.d \
./nine_dof.d \
./sensors.d \
./servo.d \
./uart_bldc.d \
./uart_bt.d \
./uart_bus.d \
./uart_ft.d 


# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.c
	@echo 'Building file: $<'
	@echo 'Invoking: AVR Compiler'
	avr-gcc -I/usr/lib/avr/include -Wall -Os -fpack-struct -fshort-enums -std=gnu99 -funsigned-char -funsigned-bitfields -mmcu=atxmega128a3 -DF_CPU=32000000UL -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


