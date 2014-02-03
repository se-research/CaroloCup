#ifndef GPIO_H
#define GPIO_H

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <pthread.h>

#define EXYNOS4_PA_GPIO1                0x11400000
#define EXYNOS_5410                	0x13400000	// base address for GPIO registers
#define EXYNOS EXYNOS4_PA_GPIO1
#define GPIO_GPCONREG		4		// subtract 4 from DATA register base to get CON reg
#define GPIO_UPDOWN			4		// add 4 to DATA register base to get UPD register
#define GPIO_DRIVESTR		8		// add 8 to DATA register base to get drive str control reg

#define PULLDS 0					// disable pullup/down
#define PULLUP 1					// enable pullup
#define PULLDN 2					// enable pulldown

#define MAP_SIZE 4096UL
#define MAP_MASK (MAP_SIZE - 1)

#define PIN31Channel              0x0184
#define PIN31Bit       0
#define PIN29Channel              0x0184
#define PIN29Bit       2
#define PIN27Channel              0x0184
#define PIN27Bit       1

#define INPUT             0

int read_gpio_pin(int channel, int bit);
void setup_gpiopin(int channel, int bit, int value, int pullval);
void* loop_retrieving(void *arg);
int map_pins();
int *get_gpio_data();
int pins_state(int pin_data[]);
int calculate_movement(int before, int after);
int get_movement_data();
void initialize_pin_reading();

#endif
