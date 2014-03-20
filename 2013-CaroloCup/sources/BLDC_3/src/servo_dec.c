/*
 * servo_dec.c
 *
 *  Created on: 20 jan 2013
 *      Author: benjamin
 */

#include "servo_dec.h"
#include "stm32f4xx_conf.h"

/*
 * Settings
 */
#define SERVO_NUM				3
#define TIMER_FREQ				1000000
#define INTERRUPT_TRESHOLD		4

volatile static uint32_t interrupt_time = 0;
volatile static int8_t servo_pos[SERVO_NUM];
volatile static uint32_t time_since_update;

void servodec_init() {
	// Initialize variables
	time_since_update = 0;
	interrupt_time = 0;

	GPIO_InitTypeDef  GPIO_InitStructure;
	NVIC_InitTypeDef   NVIC_InitStructure;
	EXTI_InitTypeDef   EXTI_InitStructure;
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	uint16_t PrescalerValue = 0;

	// ------------- EXTI -------------- //
	// Clocks
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_SYSCFG, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);

	// Configure GPIO input floating
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_13 | GPIO_Pin_14;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_IN;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_DOWN;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_Init(GPIOA, &GPIO_InitStructure);

	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_3;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_IN;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_DOWN;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_Init(GPIOB, &GPIO_InitStructure);

	// Connect EXTI Lines
	SYSCFG_EXTILineConfig(EXTI_PortSourceGPIOA, EXTI_PinSource13);
	SYSCFG_EXTILineConfig(EXTI_PortSourceGPIOA, EXTI_PinSource14);
	SYSCFG_EXTILineConfig(EXTI_PortSourceGPIOB, EXTI_PinSource3);

	// Configure EXTI Lines
	EXTI_InitStructure.EXTI_Line = EXTI_Line3 | EXTI_Line13 | EXTI_Line14;
	EXTI_InitStructure.EXTI_Mode = EXTI_Mode_Interrupt;
	EXTI_InitStructure.EXTI_Trigger = EXTI_Trigger_Rising_Falling;
	EXTI_InitStructure.EXTI_LineCmd = ENABLE;
	EXTI_Init(&EXTI_InitStructure);

	// Enable and set EXTI Line Interrupts
	NVIC_InitStructure.NVIC_IRQChannel = EXTI3_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0x00;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0x01;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

	NVIC_InitStructure.NVIC_IRQChannel = EXTI15_10_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0x00;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0x01;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

	// ------------- Timer3 ------------- //
	/* Compute the prescaler value */
	/* TIM3 clock enable */
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE);

	PrescalerValue = (uint16_t) ((SystemCoreClock / 2) / TIMER_FREQ) - 1;

	/* Time base configuration */
	TIM_TimeBaseStructure.TIM_Period = 65535;
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;

	TIM_TimeBaseInit(TIM3, &TIM_TimeBaseStructure);

	/* Prescaler configuration */
	TIM_PrescalerConfig(TIM3, PrescalerValue, TIM_PSCReloadMode_Immediate);

	/* TIM3 enable counter */
	TIM_Cmd(TIM3, ENABLE);
}

void servodec_timerfunc() {
	interrupt_time++;
	time_since_update++;
}

void servodec_int_handler() {
	static int curr_index = 0;

	// Long time since last interrupt means that a new cycle has started
	if (interrupt_time >= INTERRUPT_TRESHOLD) {
		curr_index = 0;
		interrupt_time = 0;
		TIM3->CNT = 0;
		return;
	}

	if (curr_index < SERVO_NUM) {
		// Use floating point because we can :)
		float time_ms = (float)(TIM3->CNT);
		time_ms = (time_ms * 1000.0) / (float)TIMER_FREQ;

		if (time_ms < 0.4) {
			return;
		}

		TIM3->CNT = 0;

		// Check if pulse is within valid range
		if (time_ms > 0.8 && time_ms < 2.2) {

			// Truncate (just in case)
			if (time_ms > 2.0) {
				time_ms = 2.0;
			}

			if (time_ms < 1.0) {
				time_ms = 1.0;
			}

			// Update position
			servo_pos[curr_index] = (int8_t)((time_ms - 1.5)  * 255.0);
		}
	}

	curr_index++;

	if (curr_index == SERVO_NUM) {
		time_since_update = 0;
	}

	interrupt_time = 0;
}

int8_t servodec_get_servo(int servo_num) {
	if (servo_num < SERVO_NUM) {
		return servo_pos[servo_num];
	} else {
		return 0;
	}
}

/*
 * Get the amount of milliseconds that has passed since
 * the last time servo positions were received
 */
uint32_t servodec_get_time_since_update() {
	return time_since_update;
}
