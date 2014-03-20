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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <string.h>

#include "stm32f4xx_conf.h"
#include "main.h"

#include "usbd_cdc_core.h"
#include "usbd_usr.h"
#include "usbd_desc.h"
#include "usbd_cdc_vcp.h"
#include "mcpwm.h"
#include "ledpwm.h"
#include "servo.h"
#include "servo_dec.h"
#include "comm.h"
#include "drive.h"
#include "autopilot.h"

// Settings
#define USE_SERVO_INPUT			1

// Macros
#define IS_FAULT()				((GPIOC->IDR & GPIO_Pin_12) ? 0 : 1)
#define ADC_EXT_PIN_ON()		(GPIOC->BSRRL = GPIO_Pin_5)
#define ADC_EXT_PIN_OFF()		(GPIOC->BSRRH = GPIO_Pin_5)

// Private variables
volatile static uint32_t time_var1, time_var2, time_var3;
__ALIGN_BEGIN USB_OTG_CORE_HANDLE  USB_OTG_dev __ALIGN_END;

#define ADC_SAMPLE_MAX_LEN		4000
volatile static uint16_t curr0_samples[ADC_SAMPLE_MAX_LEN];
volatile static uint16_t curr1_samples[ADC_SAMPLE_MAX_LEN];
volatile static uint16_t ph1_samples[ADC_SAMPLE_MAX_LEN];
volatile static uint16_t ph2_samples[ADC_SAMPLE_MAX_LEN];
volatile static uint16_t ph3_samples[ADC_SAMPLE_MAX_LEN];
volatile static uint16_t vzero_samples[ADC_SAMPLE_MAX_LEN];
volatile static uint8_t status_samples[ADC_SAMPLE_MAX_LEN];
volatile static int16_t curr_fir_samples[ADC_SAMPLE_MAX_LEN];

volatile static int sample_len = 1000;
volatile static int sample_int = 1;
volatile static int sample_ready = 1;
volatile static int sample_now = 0;
volatile static int print = 0;
volatile static int is_limited = 1;

// Private functions
static void run_periodic_tasks();
static void init();

int main(void) {
	init();

	/*
	 * Disable STDOUT buffering. Otherwise nothing will be printed
	 * before a newline character or when the buffer is flushed.
	 */
	setbuf(stdout, NULL);

//	mcpwm_set_duty(0.3);
//	mcpwm_use_pid(1);
//	mcpwm_set_pid_speed(4000.0);

	for(;;) {
		if (mcpwm_get_state() == MC_STATE_RUNNING) {
			ledpwm_set_intensity(0, 1.0);
		} else {
			ledpwm_set_intensity(0, 0.2);
		}

		// Keep loop at 100Hz and run periodic tasks while waiting
		while(time_var2 < 10) {
			run_periodic_tasks();

			if (sample_ready && print) {
				print = 0;

				for (int i = 0;i < sample_len;i++) {
					VCP_put_char(curr0_samples[i] >> 8);
					VCP_put_char(curr0_samples[i]);
					VCP_put_char(curr1_samples[i] >> 8);
					VCP_put_char(curr1_samples[i]);
					VCP_put_char(ph1_samples[i] >> 8);
					VCP_put_char(ph1_samples[i]);
					VCP_put_char(ph2_samples[i] >> 8);
					VCP_put_char(ph2_samples[i]);
					VCP_put_char(ph3_samples[i] >> 8);
					VCP_put_char(ph3_samples[i]);
					VCP_put_char(vzero_samples[i] >> 8);
					VCP_put_char(vzero_samples[i]);
					VCP_put_char(status_samples[i]);
					VCP_put_char(curr_fir_samples[i] >> 8);
					VCP_put_char(curr_fir_samples[i]);

					if (i % 20 == 0) {
						time_var3 = 10;
						while (time_var3) {
							run_periodic_tasks();
						}
					}
				}
			}
		}

		time_var2 -= 10;
	}

	return 0;
}

void main_set_limited(int limited) {
	is_limited = limited;
}

/*
 * This function is called while the main loop is idle. Make sure
 * that the calls in here don't block the thread for too long.
 */
static void run_periodic_tasks() {
	servo_run_periodic_tasks();
	comm_run_tasks();
	ap_run();

#if !USE_SERVO_INPUT
	if (IS_FAULT()) {
		ledpwm_set_intensity(1, 0.5);
	} else {
		ledpwm_set_intensity(1, 0.0);
	}
#else
	static int last_override = 0;
	if (servodec_get_time_since_update() < 100) {
		// Turn on led and ext pin
		ledpwm_set_intensity(1, 1.0);
		ADC_EXT_PIN_OFF();

		// Make sure that the communication module cannot control the motor or servo
		comm_override_power_steering(1);

		// Set steering servo value
		servos[0].pos = -servodec_get_servo(SERVODEC_IND_STEERING);

		// Set motor power
		float duty = servodec_get_servo(SERVODEC_IND_THROTTLE);
		duty /= 800.0;
		if (fabsf(duty) < 0.05) {
			duty = 0;
		}

		if (fabsf(drive_get_current_speed()) < 0.3 || !is_limited) {
			mcpwm_set_duty(duty);
		} else {
			mcpwm_set_duty(0);
		}

		last_override = 1;
	} else {
		// Turn off led and ext pin
		ledpwm_set_intensity(1, 0.0);
		ADC_EXT_PIN_ON();

		// Remove communication module override
		comm_override_power_steering(0);

		if (last_override) {
			last_override = 0;
			mcpwm_set_duty(0);
			servos[0].pos = 0;
		}
	}
#endif
}

void main_process_packet(unsigned char *data, unsigned char len) {
	if (!len) {
		return;
	}

	int16_t value;

	switch (data[0]) {
	case 1:
		// Sample and print data
		value = (int)data[1] << 8 | (int)data[2];
		if (value > ADC_SAMPLE_MAX_LEN) {
			value = ADC_SAMPLE_MAX_LEN;
		}
		sample_len = value;
		sample_int = data[3];
		sample_now = 0;
		sample_ready = 0;
		print = 1;
		break;

	case 2:
		// Duty Control
		value = (int)data[1] << 8 | (int)data[2];
		mcpwm_use_pid(0);
		mcpwm_set_duty((float)value / 1000.0);
		break;

	case 3:
		// PID Control
		value = (int)data[1] << 8 | (int)data[2];
		mcpwm_use_pid(1);
		mcpwm_set_pid_speed((float)value);
		break;

	case 4:
		// Detect
		mcpwm_set_detect();
		break;

	default:
		break;
	}
}

static void init() {
	GPIO_InitTypeDef  GPIO_InitStructure;
	USART_InitTypeDef USART_InitStructure;

	// ---------- SysTick timer -------- //
	if (SysTick_Config(SystemCoreClock / 1000)) {
		// Capture error
		while (1){};
	}

	// ------------- USB -------------- //
	USBD_Init(&USB_OTG_dev,
	            USB_OTG_FS_CORE_ID,
	            &USR_desc,
	            &USBD_CDC_cb,
	            &USR_cb);

	// MCPWM
	mcpwm_init();

	// LEDs
	ledpwm_init();

	// Servo driver
	servo_init();

	// Servo decoder
#if USE_SERVO_INPUT
	servodec_init();
#endif

	/*
	 * UART
	 */
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_USART6, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);

	// IO
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_6 | GPIO_Pin_7;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AF;
	GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_UP;
	GPIO_Init(GPIOC, &GPIO_InitStructure);

	GPIO_SetBits(GPIOC, GPIO_Pin_6 | GPIO_Pin_7);

	GPIO_PinAFConfig(GPIOC, GPIO_PinSource6, GPIO_AF_USART6);
	GPIO_PinAFConfig(GPIOC, GPIO_PinSource7, GPIO_AF_USART6);

	// Conf
	USART_InitStructure.USART_BaudRate = 125000;
	USART_InitStructure.USART_WordLength = USART_WordLength_9b;
	USART_InitStructure.USART_StopBits = USART_StopBits_1;
	USART_InitStructure.USART_Parity = USART_Parity_No;
	USART_InitStructure.USART_HardwareFlowControl = USART_HardwareFlowControl_None;
	USART_InitStructure.USART_Mode = USART_Mode_Tx | USART_Mode_Rx;
	USART_Init(USART6, &USART_InitStructure);

	// Enable
	USART_Cmd(USART6, ENABLE);

	// Use ADC_EXT as output. TODO: Maybe change it back to an adc??
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);

	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_5;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_OUT;
	GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
	GPIO_Init(GPIOC, &GPIO_InitStructure);
}

/*
 * Called every time new ADC values are available. Note that
 * the ADC is initialized from mcpwm.c
 */
void dma_adc_handler() {
	ledpwm_update_pwm();

	static int a = 0;
	if (!sample_ready) {
		a++;
		if (a >= sample_int) {
			a = 0;
			curr0_samples[sample_now] = ADC_curr_norm_value[0];
			curr1_samples[sample_now] = ADC_curr_norm_value[1];
			ph1_samples[sample_now] = ADC_V_L1;
			ph2_samples[sample_now] = ADC_V_L2;
			ph3_samples[sample_now] = ADC_V_L3;
			vzero_samples[sample_now] = ADC_V_ZERO * MCPWM_VZERO_FACT;

			if (mcpwm_get_state() == MC_STATE_DETECTING && 0) {
				status_samples[sample_now] = mcpwm_get_detect_top();
			} else {
				status_samples[sample_now] = mcpwm_get_comm_step() | (mcpwm_read_hall_phase() << 3);
			}

			//curr_fir_samples[sample_now] = (int16_t)(mcpwm_get_tot_current_filtered() * 100);

			sample_now++;
			if (sample_now == sample_len) {
				sample_ready = 1;
				sample_now = 0;
			}
		}
	}
}

/*
 * Called from systick handler
 */
void timing_handler() {
	if (time_var1) {
		time_var1--;
	}

	if (time_var3) {
		time_var3--;
	}

	time_var2++;
}

/*
 * Delay a number of systick cycles (1ms)
 */
void Delay(volatile uint32_t nCount) {
	time_var1 = nCount;
	while(time_var1){};
}

/*
 * Dummy function to avoid compiler error
 */
void _init() {

}
