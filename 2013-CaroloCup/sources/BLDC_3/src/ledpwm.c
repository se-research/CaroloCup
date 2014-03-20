/*
	Copyright 2012 Benjamin Vedder	benjamin@vedder.se

	This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

/*
 * ledpwm.c
 *
 *  Created on: 3 nov 2012
 *      Author: benjamin
 */

#include "ledpwm.h"
#include "stm32f4xx_conf.h"
#include <string.h>
#include <math.h>

// Macros
#define LED1_ON()		(GPIOC->BSRRL = GPIO_Pin_8)
#define LED1_OFF()		(GPIOC->BSRRH = GPIO_Pin_8)
#define LED2_ON()		(GPIOC->BSRRL = GPIO_Pin_9)
#define LED2_OFF()		(GPIOC->BSRRH = GPIO_Pin_9)

// Private variables
volatile static int led_values[LEDPWM_LED_NUM];

void ledpwm_init() {
	GPIO_InitTypeDef  GPIO_InitStructure;

	memset((int*)led_values, 0, sizeof(led_values));

	// GPIOD Periph clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);

	// LEDs
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_8 | GPIO_Pin_9;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_OUT;
	GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
	GPIO_Init(GPIOC, &GPIO_InitStructure);
}

/*
 * Set the intensity for one led. The intensity value is mapped to a PWM value
 * according to human luminance perception.
 *
 * Intensity range is 0.0 to 1.0
 */
void ledpwm_set_intensity(unsigned int led, float intensity) {
	if (led > LEDPWM_LED_NUM) {
		return;
	}

	if (intensity < 0.0) {
		intensity = 0.0;
	}

	if (intensity > 1.0) {
		intensity = 1.0;
	}

	led_values[led] = (int)roundf(powf(intensity, 2.5) * ((float)LEDPWM_CNT_TOP - 1.0));
}

/*
 * Call this function as fast as possible, with a deterministic rate.
 */
void ledpwm_update_pwm() {
	static int cnt = 0;
	cnt++;
	if (cnt == LEDPWM_CNT_TOP) {
		cnt = 0;
	}

	if (cnt >= led_values[0]) {
		LED1_OFF();
	} else {
		LED1_ON();
	}

	if (cnt >= led_values[1]) {
		LED2_OFF();
	} else {
		LED2_ON();
	}
}
