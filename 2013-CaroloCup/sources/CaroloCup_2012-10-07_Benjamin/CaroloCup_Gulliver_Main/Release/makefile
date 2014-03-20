################################################################################
# Automatically-generated file. Do not edit!
################################################################################

-include ../makefile.init

RM := rm -rf

# All of the sources participating in the build are defined here
-include sources.mk
-include subdir.mk
-include objects.mk

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(strip $(C_DEPS)),)
-include $(C_DEPS)
endif
ifneq ($(strip $(ASM_DEPS)),)
-include $(ASM_DEPS)
endif
ifneq ($(strip $(S_DEPS)),)
-include $(S_DEPS)
endif
ifneq ($(strip $(S_UPPER_DEPS)),)
-include $(S_UPPER_DEPS)
endif
endif

-include ../makefile.defs

# Add inputs and outputs from these tool invocations to the build variables 
LSS += \
CaroloCup_Gulliver_Main.lss \

FLASH_IMAGE += \
CaroloCup_Gulliver_Main.hex \

EEPROM_IMAGE += \
CaroloCup_Gulliver_Main.eep \

SIZEDUMMY += \
sizedummy \


# All Target
all: CaroloCup_Gulliver_Main.elf secondary-outputs

# Tool invocations
CaroloCup_Gulliver_Main.elf: $(OBJS) $(USER_OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: AVR C Linker'
	avr-gcc -Wl,-Map,CaroloCup_Gulliver_Main.map -Wl,-u,vfprintf -mmcu=atxmega128a3 -o "CaroloCup_Gulliver_Main.elf" $(OBJS) $(USER_OBJS) $(LIBS)
	@echo 'Finished building target: $@'
	@echo ' '
	$(MAKE) --no-print-directory post-build

CaroloCup_Gulliver_Main.lss: CaroloCup_Gulliver_Main.elf
	@echo 'Invoking: AVR Create Extended Listing'
	-avr-objdump -h -S CaroloCup_Gulliver_Main.elf  >"CaroloCup_Gulliver_Main.lss"
	@echo 'Finished building: $@'
	@echo ' '

CaroloCup_Gulliver_Main.hex: CaroloCup_Gulliver_Main.elf
	@echo 'Create Flash image (ihex format)'
	-avr-objcopy -R .eeprom -O ihex CaroloCup_Gulliver_Main.elf  "CaroloCup_Gulliver_Main.hex"
	@echo 'Finished building: $@'
	@echo ' '

CaroloCup_Gulliver_Main.eep: CaroloCup_Gulliver_Main.elf
	@echo 'Create eeprom image (ihex format)'
	-avr-objcopy -j .eeprom --no-change-warnings --change-section-lma .eeprom=0 -O ihex CaroloCup_Gulliver_Main.elf  "CaroloCup_Gulliver_Main.eep"
	@echo 'Finished building: $@'
	@echo ' '

sizedummy: CaroloCup_Gulliver_Main.elf
	@echo 'Invoking: Print Size'
	-avr-size --format=avr --mcu=atxmega128a3 CaroloCup_Gulliver_Main.elf
	@echo 'Finished building: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS)$(C_DEPS)$(ASM_DEPS)$(EEPROM_IMAGE)$(FLASH_IMAGE)$(ELFS)$(LSS)$(S_DEPS)$(SIZEDUMMY)$(S_UPPER_DEPS) CaroloCup_Gulliver_Main.elf
	-@echo ' '

post-build:
	-avr-objcopy -R .eeprom -O binary CaroloCup_Gulliver_Main.elf  CaroloCup_Gulliver_Main.bin
	-@echo ' '

secondary-outputs: $(LSS) $(FLASH_IMAGE) $(EEPROM_IMAGE) $(SIZEDUMMY)

.PHONY: all clean dependents
.SECONDARY: post-build

-include ../makefile.targets
