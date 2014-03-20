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
 * mcpwm.c
 *
 *  Created on: 13 okt 2012
 *      Author: benjamin
 */

#include <stdlib.h>
#include <math.h>
#include "main.h"
#include "mcpwm.h"
#include "stm32f4xx_conf.h"
#include "digital_filter.h"

// Private variables
volatile static int comm_step;
volatile static int direction;
volatile static int last_comm_time;
volatile static float dutycycle_set;
volatile static float dutycycle_now;
volatile static float rpm_now;
volatile static int is_using_pid;
volatile static float pid_set_rpm;
volatile static float cycle_integrator;
volatile static int tachometer;
volatile static int pwm_adc_cycles;
volatile static int curr0_sum;
volatile static int curr1_sum;
volatile static int curr_start_samples;
volatile static int curr0_offset;
volatile static int curr1_offset;
volatile static mc_state state;
volatile static float detect_currents[6];
volatile static int detect_steps;
volatile static int detect_now;
volatile static int detect_inc;
volatile static int detect_do_step;
volatile static mc_pwm_mode pwm_mode;

#if MCPWM_IS_SENSORLESS
volatile static int start_pulses;
volatile static int closed_cycles;
#endif

/*
 * KV_Filter
 * F_SAMP = 100Hz
 * CUT = 5Hz
 * taps = 100
 */
#define KV_TAPS		100
const static float fir_coeffs[] = { 0.00015589, 0.00030737, 0.00044811,
		0.00056976, 0.0006599, 0.00070204, 0.0006771, 0.00056632, 0.00035537,
		3.9036e-05, -0.00037435, -0.00085954, -0.0013731, -0.0018547,
		-0.0022317, -0.0024264, -0.0023665, -0.0019957, -0.0012862, -0.00024787,
		0.0010645, 0.002549, 0.0040606, 0.0054207, 0.0064332, 0.0069049,
		0.0066697, 0.0056131, 0.0036946, 0.00096543, -0.0024214, -0.0062108,
		-0.010055, -0.013537, -0.016198, -0.017578, -0.017259, -0.014904,
		-0.010304, -0.003399, 0.0056927, 0.016675, 0.029084, 0.042311, 0.055644,
		0.068318, 0.079572, 0.088709, 0.095149, 0.098477, 0.098477, 0.095149,
		0.088709, 0.079572, 0.068318, 0.055644, 0.042311, 0.029084, 0.016675,
		0.0056927, -0.003399, -0.010304, -0.014904, -0.017259, -0.017578,
		-0.016198, -0.013537, -0.010055, -0.0062108, -0.0024214, 0.00096543,
		0.0036946, 0.0056131, 0.0066697, 0.0069049, 0.0064332, 0.0054207,
		0.0040606, 0.002549, 0.0010645, -0.00024787, -0.0012862, -0.0019957,
		-0.0023665, -0.0024264, -0.0022317, -0.0018547, -0.0013731, -0.00085954,
		-0.00037435, 3.9036e-05, 0.00035537, 0.00056632, 0.0006771, 0.00070204,
		0.0006599, 0.00056976, 0.00044811, 0.00030737, 0.00015589, };

volatile static float kv_samples[KV_TAPS];
volatile static int kv_index = 0;

// Current FIR filter
#define CURR_FIR_TAPS_BITS		7
#define CURR_FIR_LEN			(1 << CURR_FIR_TAPS_BITS)
#define CURR_FIR_FCUT			0.02
volatile static float current_fir_coeffs[CURR_FIR_LEN];
volatile static float current_fir_samples[CURR_FIR_LEN];
volatile static int current_fir_index = 0;

// Hall sensor shift table
const unsigned int mc_shift_table[] = {
	// 0
	0b000,	// 000
	0b001,	// 001
	0b010,	// 010
	0b011,	// 011
	0b100,	// 100
	0b101,	// 101
	0b110,	// 110
	0b111,	// 111

	// 1
	0b000,	// 000
	0b001,	// 001
	0b100,	// 010
	0b101,	// 011
	0b010,	// 100
	0b011,	// 101
	0b110,	// 110
	0b111,	// 111

	// 2
	0b000,	// 000
	0b010,	// 001
	0b001,	// 010
	0b011,	// 011
	0b100,	// 100
	0b110,	// 101
	0b101,	// 110
	0b111,	// 111

	// 3
	0b000,	// 000
	0b100,	// 001
	0b010,	// 010
	0b110,	// 011
	0b001,	// 100
	0b101,	// 101
	0b011,	// 110
	0b111,	// 111

	// 4
	0b000,	// 000
	0b010,	// 001
	0b100,	// 010
	0b110,	// 011
	0b001,	// 100
	0b011,	// 101
	0b101,	// 110
	0b111,	// 111

	// 5
	0b000,	// 000
	0b100,	// 001
	0b001,	// 010
	0b101,	// 011
	0b010,	// 100
	0b110,	// 101
	0b011,	// 110
	0b111	// 111
};
volatile static unsigned int hall_sensor_order;

// Global variables
volatile uint16_t ADC_Value[MCPWM_ADC_CHANNELS];
volatile int ADC_curr_norm_value[3];

// Private functions
static void set_duty_cycle(float dutyCycle);
static void stopPwm();
static void run_pid_controller();
static void set_next_comm_step(int next_step);
void update_rpm_tacho();

#if MCPWM_IS_SENSORLESS
static void setOpenLoop();
static int integrate_cycle(float v_diff);
#endif

// Defines
#define ADC_CDR_ADDRESS			((uint32_t)0x40012308)
#define ENABLE_GATE()			(GPIOC->BSRRL = GPIO_Pin_10)
#define DISABLE_GATE()			(GPIOC->BSRRH = GPIO_Pin_10)
#define DCCAL_ON()				(GPIOB->BSRRL = GPIO_Pin_12)
#define DCCAL_OFF()				(GPIOB->BSRRH = GPIO_Pin_12)
#define IS_FAULT()				((GPIOC->IDR & GPIO_Pin_12) ? 0 : 1)
#define READ_HALL1()			((GPIOB->IDR & GPIO_Pin_6) ? 1 : 0)
#define READ_HALL2()			((GPIOB->IDR & GPIO_Pin_7) ? 1 : 0)
#define READ_HALL3()			((GPIOB->IDR & GPIO_Pin_8) ? 1 : 0)

void mcpwm_init() {
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	TIM_BDTRInitTypeDef TIM_BDTRInitStructure;
	GPIO_InitTypeDef GPIO_InitStructure;
	NVIC_InitTypeDef NVIC_InitStructure;

	// Initialize variables
	comm_step = 1;
	direction = 1;
	rpm_now = 0;
	last_comm_time = 0;
	dutycycle_set = 0;
	dutycycle_now = 0;
	is_using_pid = 0;
	pid_set_rpm = 0.0;
	cycle_integrator = 0.0;
	tachometer = 0;
	pwm_adc_cycles = 0;
	state = MC_STATE_OFF;
	detect_steps = 0;
	detect_now = 0;
	detect_inc = 0;
	detect_do_step = 0;
	hall_sensor_order = MCPWM_HALL_SENSOR_ORDER;
	pwm_mode = MCPWM_PWM_MODE;

#if MCPWM_IS_SENSORLESS
	start_pulses = 0;
	closed_cycles = 0;
#endif

	// Create current FIR filter
	filter_create_fir((float*)current_fir_coeffs, CURR_FIR_FCUT, CURR_FIR_TAPS_BITS, 1);

	// GPIO clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);

	// GPIOC (ENABLE_GATE)
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_10;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_OUT;
	GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
	GPIO_Init(GPIOC, &GPIO_InitStructure);

	DISABLE_GATE();

	// GPIOB (DCCAL)
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_12;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_OUT;
	GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
	GPIO_Init(GPIOB, &GPIO_InitStructure);

	// GPIOB (hall sensors)
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_6 | GPIO_Pin_7 | GPIO_Pin_8;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_IN;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
	GPIO_Init(GPIOB, &GPIO_InitStructure);

	// Fault pin
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_12;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_IN;
	GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_UP;
	GPIO_Init(GPIOC, &GPIO_InitStructure);

	// TIM1 clock enable
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM1, ENABLE);

	// GPIOA Configuration: Channel 1 to 3 as alternate function push-pull
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_8 | GPIO_Pin_9 | GPIO_Pin_10;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AF;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
	GPIO_Init(GPIOA, &GPIO_InitStructure);

	// GPIOB Configuration: Channel 1 to 3 N as alternate function push-pull
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_13 | GPIO_Pin_14 | GPIO_Pin_15;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AF;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
	GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
	GPIO_Init(GPIOB, &GPIO_InitStructure);

	// Connect TIM OC pins to AF1
	GPIO_PinAFConfig(GPIOA, GPIO_PinSource8, GPIO_AF_TIM1);
	GPIO_PinAFConfig(GPIOA, GPIO_PinSource9, GPIO_AF_TIM1);
	GPIO_PinAFConfig(GPIOA, GPIO_PinSource10, GPIO_AF_TIM1);
	GPIO_PinAFConfig(GPIOB, GPIO_PinSource13, GPIO_AF_TIM1);
	GPIO_PinAFConfig(GPIOB, GPIO_PinSource14, GPIO_AF_TIM1);
	GPIO_PinAFConfig(GPIOB, GPIO_PinSource15, GPIO_AF_TIM1);

	// Enable the TIM1 Trigger and commutation interrupt
	NVIC_InitStructure.NVIC_IRQChannel = TIM1_TRG_COM_TIM11_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 3;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 1;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = SystemCoreClock / MCPWM_SWITCH_FREQUENCY;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;

	TIM_TimeBaseInit(TIM1, &TIM_TimeBaseStructure);

	// Channel 1, 2 and 3 Configuration in PWM mode
	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_OutputNState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_Pulse = 0;
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;
	TIM_OCInitStructure.TIM_OCNPolarity = TIM_OCNPolarity_High;
	TIM_OCInitStructure.TIM_OCIdleState = TIM_OCIdleState_Set;
	TIM_OCInitStructure.TIM_OCNIdleState = TIM_OCNIdleState_Set;

	TIM_OC1Init(TIM1, &TIM_OCInitStructure);
	TIM_OC2Init(TIM1, &TIM_OCInitStructure);
	TIM_OC3Init(TIM1, &TIM_OCInitStructure);
	TIM_OC4Init(TIM1, &TIM_OCInitStructure);

	// Automatic Output enable, Break, dead time and lock configuration
	TIM_BDTRInitStructure.TIM_OSSRState = TIM_OSSRState_Enable;
	TIM_BDTRInitStructure.TIM_OSSIState = TIM_OSSRState_Enable;
	TIM_BDTRInitStructure.TIM_LOCKLevel = TIM_LOCKLevel_OFF;
	TIM_BDTRInitStructure.TIM_DeadTime = MCPWM_DEAD_TIME_CYCLES;
	TIM_BDTRInitStructure.TIM_Break = TIM_Break_Disable;
	TIM_BDTRInitStructure.TIM_BreakPolarity = TIM_BreakPolarity_High;
	TIM_BDTRInitStructure.TIM_AutomaticOutput = TIM_AutomaticOutput_Enable;

	TIM_BDTRConfig(TIM1, &TIM_BDTRInitStructure);
	TIM_CCPreloadControl(TIM1, ENABLE);
	TIM_ITConfig(TIM1, TIM_IT_COM, ENABLE);

	/*
	 * ADC!
	 */
	ADC_CommonInitTypeDef ADC_CommonInitStructure;
	DMA_InitTypeDef DMA_InitStructure;
	ADC_InitTypeDef ADC_InitStructure;

	// Clock
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_DMA2 | RCC_AHB1Periph_GPIOA | RCC_AHB1Periph_GPIOC, ENABLE);
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_ADC1 | RCC_APB2Periph_ADC2 | RCC_APB2Periph_ADC3, ENABLE);

	// DMA
	DMA_InitStructure.DMA_Channel = DMA_Channel_0;
	DMA_InitStructure.DMA_Memory0BaseAddr = (uint32_t)&ADC_Value;
	DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)ADC_CDR_ADDRESS;
	DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralToMemory;
	DMA_InitStructure.DMA_BufferSize = MCPWM_ADC_CHANNELS;
	DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
	DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;
	DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;
	DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;
	DMA_InitStructure.DMA_Mode = DMA_Mode_Circular;
	DMA_InitStructure.DMA_Priority = DMA_Priority_High;
	DMA_InitStructure.DMA_FIFOMode = DMA_FIFOMode_Enable;
	DMA_InitStructure.DMA_FIFOThreshold = DMA_FIFOThreshold_1QuarterFull;
	DMA_InitStructure.DMA_MemoryBurst = DMA_MemoryBurst_Single;
	DMA_InitStructure.DMA_PeripheralBurst = DMA_PeripheralBurst_Single;
	DMA_Init(DMA2_Stream0, &DMA_InitStructure);

	// DMA2_Stream0 enable
	DMA_Cmd(DMA2_Stream0, ENABLE);

	// Enable transfer complete interrupt
	DMA_ITConfig(DMA2_Stream0, DMA_IT_TC, ENABLE);
	NVIC_InitStructure.NVIC_IRQChannel = DMA2_Stream0_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_0 | GPIO_Pin_1 | GPIO_Pin_2 | GPIO_Pin_3 | GPIO_Pin_4 | GPIO_Pin_5 | GPIO_Pin_6;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AN;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL ;
	GPIO_Init(GPIOA, &GPIO_InitStructure);

	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_0 | GPIO_Pin_1;
	GPIO_Init(GPIOB, &GPIO_InitStructure);

	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_0 | GPIO_Pin_1 | GPIO_Pin_2 | GPIO_Pin_3 | GPIO_Pin_4 | GPIO_Pin_5;
	GPIO_Init(GPIOC, &GPIO_InitStructure);

	// ADC Common Init
	ADC_CommonInitStructure.ADC_Mode = ADC_TripleMode_RegSimult;
	ADC_CommonInitStructure.ADC_Prescaler = ADC_Prescaler_Div2;
	ADC_CommonInitStructure.ADC_DMAAccessMode = ADC_DMAAccessMode_1;
	ADC_CommonInitStructure.ADC_TwoSamplingDelay = ADC_TwoSamplingDelay_5Cycles;
	ADC_CommonInit(&ADC_CommonInitStructure);

	// Channel-specific settings
	ADC_InitStructure.ADC_Resolution = ADC_Resolution_12b;
	ADC_InitStructure.ADC_ScanConvMode = ENABLE;
	ADC_InitStructure.ADC_ContinuousConvMode = DISABLE;
	ADC_InitStructure.ADC_ExternalTrigConvEdge = ADC_ExternalTrigConvEdge_Falling;
	ADC_InitStructure.ADC_ExternalTrigConv = ADC_ExternalTrigConv_T8_CC1;
	ADC_InitStructure.ADC_DataAlign = ADC_DataAlign_Right;
	ADC_InitStructure.ADC_NbrOfConversion = 4;

	ADC_Init(ADC1, &ADC_InitStructure);
	ADC_InitStructure.ADC_ExternalTrigConvEdge = ADC_ExternalTrigConvEdge_None;
	ADC_InitStructure.ADC_ExternalTrigConv = 0;
	ADC_Init(ADC2, &ADC_InitStructure);
	ADC_Init(ADC3, &ADC_InitStructure);

	// ADC1 regular channels 0, 5, 10, 13
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 1, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5, 2, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 3, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_13, 4, ADC_SampleTime_3Cycles);

	// ADC2 regular channels 1, 6, 11, 15
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 1, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 2, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 3, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 4, ADC_SampleTime_3Cycles);

	// ADC3 regular channels 2, 3, 12, 3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 1, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 2, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 3, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 4, ADC_SampleTime_3Cycles);

	// Enable DMA request after last transfer (Multi-ADC mode)
	ADC_MultiModeDMARequestAfterLastTransferCmd(ENABLE);

	// Injected channels for current measurement at end of cycle
	ADC_ExternalTrigInjectedConvConfig(ADC1, ADC_ExternalTrigInjecConv_T1_CC4);
	ADC_ExternalTrigInjectedConvConfig(ADC2, ADC_ExternalTrigInjecConv_T8_CC2);
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC1, ADC_ExternalTrigInjecConvEdge_Falling);
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC2, ADC_ExternalTrigInjecConvEdge_Falling);
	ADC_InjectedSequencerLengthConfig(ADC1, 3);
	ADC_InjectedSequencerLengthConfig(ADC2, 3);
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_5, 1, ADC_SampleTime_3Cycles);
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_5, 2, ADC_SampleTime_3Cycles);
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_5, 3, ADC_SampleTime_3Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_6, 1, ADC_SampleTime_3Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_6, 2, ADC_SampleTime_3Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_6, 3, ADC_SampleTime_3Cycles);
	// Interrupt
	ADC_ITConfig(ADC1, ADC_IT_JEOC, ENABLE);
	NVIC_InitStructure.NVIC_IRQChannel = ADC_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

	// Enable ADC1
	ADC_Cmd(ADC1, ENABLE);

	// Enable ADC2
	ADC_Cmd(ADC2, ENABLE);

	// Enable ADC3
	ADC_Cmd(ADC3, ENABLE);

	// ------------- Timer8 for ADC sampling ------------- //
	// Time Base configuration
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM8, ENABLE);

	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = SystemCoreClock / MCPWM_SWITCH_FREQUENCY;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(TIM8, &TIM_TimeBaseStructure);

	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_Pulse = 100;
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;
	TIM_OCInitStructure.TIM_OCNPolarity = TIM_OCNPolarity_High;
	TIM_OCInitStructure.TIM_OCIdleState = TIM_OCIdleState_Set;
	TIM_OCInitStructure.TIM_OCNIdleState = TIM_OCNIdleState_Set;
	TIM_OC1Init(TIM8, &TIM_OCInitStructure);
	TIM_OC1PreloadConfig(TIM8, TIM_OCPreload_Disable);
	TIM_OC2Init(TIM8, &TIM_OCInitStructure);
	TIM_OC2PreloadConfig(TIM8, TIM_OCPreload_Disable);

	// PWM outputs have to be enabled in order to trigger ADC on CCx
	TIM_CtrlPWMOutputs(TIM8, ENABLE);

	// TIM1 Master and TIM8 slave
	TIM_SelectOutputTrigger(TIM1, TIM_TRGOSource_Enable);
	TIM_SelectMasterSlaveMode(TIM1, TIM_MasterSlaveMode_Enable);
	TIM_SelectMasterSlaveMode(TIM8, TIM_MasterSlaveMode_Enable);
	TIM_SelectInputTrigger(TIM8, TIM_TS_ITR0);
	TIM_SelectSlaveMode(TIM8, TIM_SlaveMode_Gated);

	// Enable TIM8 first to make sure timers are in sync
	TIM_Cmd(TIM8, ENABLE);
	TIM_Cmd(TIM1, ENABLE);

	// Main Output Enable
	TIM_CtrlPWMOutputs(TIM1, ENABLE);

	// 32-bit timer for RPM measurement
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM2, ENABLE);
	uint16_t PrescalerValue = (uint16_t) ((SystemCoreClock / 2) / 1000000) - 1;

	// Time base configuration
	TIM_TimeBaseStructure.TIM_Period = 0xFFFFFFFF;
	TIM_TimeBaseStructure.TIM_Prescaler = PrescalerValue;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM2, &TIM_TimeBaseStructure);

	// TIM2 enable counter
	TIM_Cmd(TIM2, ENABLE);

	// Sample injected channels at end of PWM cycle
	TIM1->CCR4 = TIM1->ARR - 300;
	TIM8->CCR2 = TIM1->ARR - 300;

	// Calibrate current offset
	ENABLE_GATE();
	Delay(100);
	DCCAL_ON();
	curr0_sum = 0;
	curr1_sum = 0;
	curr_start_samples = 0;
	while(curr_start_samples < 5000) {};
	curr0_offset = curr0_sum / curr_start_samples;
	curr1_offset = curr1_sum / curr_start_samples;
	DCCAL_OFF();
}

void mcpwm_set_duty(float dutyCycle) {
	if (dutyCycle >= 0) {
		direction = 1;
	} else {
		dutyCycle = -dutyCycle;
		direction = 0;
	}

	if (dutyCycle < MCPWM_MIN_DUTY_CYCLE) {
		state = MC_STATE_OFF;
		dutycycle_set = dutyCycle;
		stopPwm();
		return;
	} else if (dutyCycle > MCPWM_MAX_DUTY_CYCLE) {
		dutyCycle = MCPWM_MAX_DUTY_CYCLE;
	}

#if MCPWM_IS_SENSORLESS
	if (state != MC_STATE_RUNNING && state != MC_STATE_STARTING) {
		setOpenLoop();
	}
#else
	if (state != MC_STATE_RUNNING) {
		state = MC_STATE_RUNNING;
		set_next_comm_step(mcpwm_read_hall_phase());
		TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
	}
#endif

	dutycycle_set = dutyCycle;
}

void mcpwm_use_pid(int use_pid) {
	is_using_pid = use_pid;
}

void mcpwm_set_pid_speed(float rpm) {
	pid_set_rpm = rpm;
}

int mcpwm_get_comm_step() {
	return comm_step;
}

float mcpwm_get_duty_cycle() {
	return dutycycle_set;
}

float mcpwm_get_rpm() {
	return direction ? rpm_now : -rpm_now;
}

float mcpwm_get_kv() {
	return rpm_now / (GET_INPUT_VOLTAGE * mcpwm_get_duty_cycle());
}

mc_state mcpwm_get_state() {
	return state;
}

float mcpwm_get_kv_filtered() {
	float result = 0;

	for (int i = 0;i < KV_TAPS;i++) {
		int sample_index = kv_index + i + 1;
		if (sample_index >= KV_TAPS) {
			sample_index -= KV_TAPS;
		}

		result += kv_samples[sample_index] * fir_coeffs[i];
	}

	return result;
}

float mcpwm_get_tot_current_filtered() {
	float value = filter_run_fir_iteration((float*)current_fir_samples,
			(float*)current_fir_coeffs, CURR_FIR_TAPS_BITS, current_fir_index);

	value *= (3.3 / 4095.0) / (0.0005 * 50.0);
	return value;
}

int mcpwm_get_tachometer_value(int reset) {
	int val = tachometer;

	if (reset) {
		tachometer = 0;
	}

	return val;
}

void mcpwm_set_detect() {
	detect_steps = 0;
	is_using_pid = 0;

	for(int i = 0;i < 6;i++) {
		detect_currents[i] = 0;
	}

	state = MC_STATE_DETECTING;
}

int mcpwm_get_detect_top() {
	float max = 0;
	int pos = 1;
	for(int i = 0;i < 6;i++) {
		if (detect_currents[i] > max) {
			max = detect_currents[i];
			pos = i + 1;
		}
	}

	return pos;
}

static void stopPwm() {
	TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

	TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

	TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
}

#if MCPWM_IS_SENSORLESS
static void setOpenLoop() {
	start_pulses = 0;
	state = MC_STATE_STARTING;
	closed_cycles = 0;
	cycle_integrator = 0.0;
}
#endif

static void set_duty_cycle(float dutyCycle) {
	if (dutyCycle < MCPWM_MIN_DUTY_CYCLE) {
		dutyCycle = MCPWM_MIN_DUTY_CYCLE;
	} else if (dutyCycle > MCPWM_MAX_DUTY_CYCLE) {
		dutyCycle = MCPWM_MAX_DUTY_CYCLE;
	}

	dutycycle_now = dutyCycle;

	uint16_t period;
	if (pwm_mode == PWM_MODE_BIPOLAR) {
		period = (uint16_t)(((float)TIM1->ARR / 2.0) * dutyCycle + ((float)TIM1->ARR / 2.0));
	} else {
		period = (uint16_t)((float)TIM1->ARR * dutyCycle);
	}

	TIM1->CCR1 = period;
	TIM1->CCR2 = period;
	TIM1->CCR3 = period;

	// Sample ADC at appropriate time during the pwm cycle
	if (pwm_mode == PWM_MODE_BIPOLAR) {
		TIM8->CCR1 = 50;
//		TIM8->CCR1 = period / 2;
	} else {
		TIM8->CCR1 = period / 2;
		// Current samples
		TIM1->CCR4 = (TIM1->ARR - period) / 2 + period;
		TIM8->CCR2 = (TIM1->ARR - period) / 2 + period;
	}
}

static void run_pid_controller() {
	static float i_term = 0;
	static float prev_error = 0;
	float p_term;
	float d_term;

	// PID is off. Return.
	if (!is_using_pid) {
		i_term = 0;
		prev_error = 0;
		return;
	}

	// Too low RPM set. Stop and return.
	if (fabsf(pid_set_rpm) < MCPWM_PID_MIN_RPM) {
		i_term = 0;
		prev_error = 0;
		mcpwm_set_duty(0.0);
		return;
	}

#if MCPWM_IS_SENSORLESS
	// Start sequence running. Return.
	if (state == MC_STATE_STARTING || closed_cycles < MCPWM_CLOSED_STARTPWM_COMMS) {
		i_term = 0;
		prev_error = 0;
		mcpwm_set_duty(pid_set_rpm > 0 ? MCPWM_START_DUTY_CYCLE : -MCPWM_START_DUTY_CYCLE);
		return;
	}
#endif

	// Compensation for supply voltage variations
	float scale = 1.0 / GET_INPUT_VOLTAGE;

	// Compute error
	float error = pid_set_rpm - mcpwm_get_rpm();

	// Compute parameters
	p_term = error * MCPWM_PID_KP * scale;
	i_term += error * (MCPWM_PID_KI * MCPWM_PID_TIME_K) * scale;
	d_term = (error - prev_error) * (MCPWM_PID_KD / MCPWM_PID_TIME_K) * scale;

	// I-term wind-up protection
	if (i_term > 1.0) {
		i_term = 1.0;
	} else if (i_term < -1.0) {
		i_term = -1.0;
	}

	// Store previous error
	prev_error = error;

	// Calculate output
	float output = p_term + i_term + d_term;

	// Make sure that at least minimum output is used
	if (fabsf(output) < MCPWM_MIN_DUTY_CYCLE) {
		if (output > 0.0) {
			output = MCPWM_MIN_DUTY_CYCLE;
		} else {
			output = -MCPWM_MIN_DUTY_CYCLE;
		}
	}

	// Do not output in reverse direction to oppose too high rpm
	if (pid_set_rpm > 0.0 && output < 0.0) {
		output = MCPWM_MIN_DUTY_CYCLE;
		i_term = 0.0;
	} else if (pid_set_rpm < 0.0 && output > 0.0) {
		output = -MCPWM_MIN_DUTY_CYCLE;
		i_term = 0.0;
	}

	mcpwm_set_duty(output);
}

void mcpwm_time_int_handler() {
	// Update RPM in case it has slowed down
	uint32_t tim_val = TIM2->CNT;
	uint32_t tim_diff = tim_val - last_comm_time;

	if (tim_diff > 0) {
		float rpm_tmp = ((float)MCPWM_AVG_COM_RPM * 1000000.0 * 60.0) /
					((float)tim_diff *  (float)MCPWM_NUM_POLES * 3.0);

		// Re-calculate RPM between commutations
		// This will end up being used when slowing down
		if (rpm_tmp < rpm_now) {
			rpm_now = rpm_tmp;
		}

#if MCPWM_IS_SENSORLESS
		if (rpm_now < MCPWM_MIN_CLOSED_RPM && state == MC_STATE_RUNNING) {
			setOpenLoop();
		}
#endif
	}

#if MCPWM_IS_SENSORLESS
	// Duty-cycle, detect and startup
	static int start_time = 0;
#endif

	switch (state) {
	case MC_STATE_OFF:
		stopPwm();
		break;

	case MC_STATE_DETECTING:
		detect_do_step = 1;
		break;

#if MCPWM_IS_SENSORLESS
	case MC_STATE_STARTING:
		set_duty_cycle(MCPWM_START_DUTY_CYCLE);
		start_time++;

		if (start_time >= MCPWM_START_COMM_TIME_MS) {
			start_time = 0;
			start_pulses++;
			TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
		}
		break;
#endif

	case MC_STATE_RUNNING:
		// TODO: Ramping??
#if MCPWM_IS_SENSORLESS
		if (closed_cycles < MCPWM_CLOSED_STARTPWM_COMMS) {
			set_duty_cycle(MCPWM_START_DUTY_CYCLE);
		} else {
			set_duty_cycle(dutycycle_set);
		}
		start_time = 0;
#else
		set_duty_cycle(dutycycle_set);
#endif
		break;

	default:
		break;
	}

	run_pid_controller();

	// Fill KV filter vector at 100Hz
	static float cnt_tmp = 0;
	cnt_tmp++;
	if (cnt_tmp >= 10) {
		cnt_tmp = 0;
		if (state == MC_STATE_RUNNING) {
			kv_samples[kv_index] = mcpwm_get_kv();
			kv_index++;
			if (kv_index >= KV_TAPS) {
				kv_index = 0;
			}
		}
	}
}

void mcpwm_adc_inj_int_handler() {
	int curr0 = ADC_GetInjectedConversionValue(ADC1, ADC_InjectedChannel_1);
	curr0 += ADC_GetInjectedConversionValue(ADC1, ADC_InjectedChannel_2);
	curr0 += ADC_GetInjectedConversionValue(ADC1, ADC_InjectedChannel_3);
	curr0 /= 3;

	int curr1 = ADC_GetInjectedConversionValue(ADC2, ADC_InjectedChannel_1);
	curr1 += ADC_GetInjectedConversionValue(ADC2, ADC_InjectedChannel_2);
	curr1 += ADC_GetInjectedConversionValue(ADC2, ADC_InjectedChannel_3);
	curr1 /= 3;

	curr0_sum += curr0;
	curr1_sum += curr1;
	curr_start_samples++;

	ADC_curr_norm_value[0] = curr0 - curr0_offset;
	ADC_curr_norm_value[1] = curr1 - curr1_offset;
	ADC_curr_norm_value[2] = -(ADC_curr_norm_value[0] + ADC_curr_norm_value[1]);

	// Run current FIR filter
	float curr_tot_sample = 0;

	switch (comm_step) {
	case 1:
	case 6:
		if (direction) {
			curr_tot_sample = (float)ADC_curr_norm_value[2];
		} else {
			curr_tot_sample = (float)ADC_curr_norm_value[1];
		}
		break;

	case 2:
	case 3:
		curr_tot_sample = (float)ADC_curr_norm_value[0];
		break;

	case 4:
	case 5:
		if (direction) {
			curr_tot_sample = (float)ADC_curr_norm_value[1];
		} else {
			curr_tot_sample = (float)ADC_curr_norm_value[2];
		}
		break;
	}

	if (detect_now == 1) {
		float a = fabsf(ADC_curr_norm_value[0]);
		float b = fabsf(ADC_curr_norm_value[1]);

		if (a > b) {
			detect_currents[comm_step] = a;
		} else {
			detect_currents[comm_step] = b;
		}

		stopPwm();
		detect_steps++;
	}

	if (detect_now) {
		detect_now--;
	}

	if (detect_do_step) {
		detect_steps++;
		detect_now = 2;

		set_duty_cycle(0.5);

		direction = 1;
		comm_step++;
		if (comm_step > 6) {
			comm_step = 1;
		}

		set_next_comm_step(comm_step);
		TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
		detect_do_step = 0;
	}

	current_fir_samples[current_fir_index] = curr_tot_sample;
	current_fir_index++;
	if (current_fir_index == CURR_FIR_LEN) {
		current_fir_index = 0;
	}
}

/*
 * New ADC samples ready. Do commutation!
 */
void mcpwm_adc_int_handler() {
#if MCPWM_IS_SENSORLESS
	// See if current RPM is large enough to consider it updated,
	// otherwise use low enough RPM value
	float div_rpm;
	float rpm_factor = 0.3;

	if (state == MC_STATE_STARTING) {
		div_rpm = (1 / (float)MCPWM_START_COMM_TIME_MS) * 1000.0 * 60.0;
		rpm_factor = 0.6;
	} else {
		if (rpm_now < (float)MCPWM_MIN_CLOSED_RPM) {
			div_rpm = (float)MCPWM_MIN_CLOSED_RPM;
			rpm_factor = 0.1;
		} else {
			div_rpm = rpm_now;
		}
	}

	// Compute the theoretical commutation time at the current RPM
	float comm_time = ((float)MCPWM_SWITCH_FREQUENCY) /
			((div_rpm / 60.0) * (float)MCPWM_NUM_POLES * 3.0);

	if (pwm_adc_cycles >= (int)(comm_time * (1.0 / rpm_factor))) {
		TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
		if (state == MC_STATE_RUNNING) {
			closed_cycles++;
		}
		cycle_integrator = 0;
	}

	// If enough time has elapsed to make a commutation plausible
	if (pwm_adc_cycles >= (int)(comm_time * rpm_factor)) {
		int inc_step = 0;
		int curr_tres = ADC_V_ZERO * MCPWM_VZERO_FACT;
		int ph1, ph2, ph3;
		int v_diff;

		if (direction) {
			ph1 = ADC_V_L1;
			ph2 = ADC_V_L2;
			ph3 = ADC_V_L3;
		} else {
			ph1 = ADC_V_L1;
			ph2 = ADC_V_L3;
			ph3 = ADC_V_L2;
		}

		switch (comm_step) {
		case 1:
			if (ph1 > curr_tres) {
				inc_step = 1;
			}
			v_diff = abs(ph1 - curr_tres);
			break;

		case 2:
			if (ph2 < curr_tres) {
				inc_step = 1;
			}
			v_diff = abs(ph2 - curr_tres);
			break;

		case 3:
			if (ph3 > curr_tres) {
				inc_step = 1;
			}
			v_diff = abs(ph3 - curr_tres);
			break;

		case 4:
			if (ph1 < curr_tres) {
				inc_step = 1;
			}
			v_diff = abs(ph1 - curr_tres);
			break;

		case 5:
			if (ph2 > curr_tres) {
				inc_step = 1;
			}
			v_diff = abs(ph2 - curr_tres);
			break;

		case 6:
			if (ph3 < curr_tres) {
				inc_step = 1;
			}
			v_diff = abs(ph3 - curr_tres);
			break;

		default:
			break;
		}

		if (inc_step) {
			int ratio = (100 * v_diff) / curr_tres;

			if (state == MC_STATE_STARTING && ratio < MCPWM_MAX_COMM_START_DIFF
					&& start_pulses > MCPWM_MIN_START_STEPS) {
				// We think we are running in closed loop. Stop start sequence!
				state = MC_STATE_RUNNING;
				cycle_integrator = 0;
			} else if (state == MC_STATE_RUNNING) {
				integrate_cycle((float)v_diff);
			}
		}
	}
	pwm_adc_cycles++;
#else
	int hall_phase = mcpwm_read_hall_phase();
	if (comm_step != hall_phase) {
		comm_step = hall_phase;

		update_rpm_tacho();

		if (state == MC_STATE_RUNNING) {
			set_next_comm_step(hall_phase);
			TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
		}
	}
#endif
}

#if MCPWM_IS_SENSORLESS
static int integrate_cycle(float v_diff) {
	cycle_integrator += v_diff;

	const float limit = (MCPWM_CYCLE_INT_LIMIT * 800000.0) / (float)MCPWM_SWITCH_FREQUENCY;
	if (cycle_integrator >= limit) {
		TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
		closed_cycles++;
		cycle_integrator = 0;
		return 1;
	}

	return 0;
}
#endif

/**
 * Read the current phase of the motor using hall effect sensors
 * @return
 * The phase read.
 */
signed int mcpwm_read_hall_phase() {
	int hall = READ_HALL1() | (READ_HALL2() << 1) | (READ_HALL3() << 2);

	signed int tmp_phase = -1;
	int shift = mc_shift_table[hall + (hall_sensor_order << 3)];

	switch (shift) {
	case 0b101:
		tmp_phase = 1;
		break;

	case 0b001:
		tmp_phase = 2;
		break;

	case 0b011:
		tmp_phase = 3;
		break;

	case 0b010:
		tmp_phase = 4;
		break;

	case 0b110:
		tmp_phase = 5;
		break;

	case 0b100:
		tmp_phase = 6;
		break;
	case 0b000:
	case 0b111:
		tmp_phase = -1;
		break;
	}

	// This is NOT a proper way to solve this...
	if (!direction && tmp_phase >= 0) {
		signed int p_tmp = tmp_phase;
		p_tmp += 4;
		if (p_tmp < 0) {
			p_tmp += 6;
		} else if (p_tmp > 5) {
			p_tmp -= 6;
		}
		tmp_phase = 6 - p_tmp;
	}

	return tmp_phase;
}

/*
 * Commutation Steps FORWARDS
 * STEP		BR1		BR2		BR3
 * 1		0		+		-
 * 2		+		0		-
 * 3		+		-		0
 * 4		0		-		+
 * 5		-		0		+
 * 6		-		+		0
 *
 * Commutation Steps REVERSE (switch phase 2 and 3)
 * STEP		BR1		BR2		BR3
 * 1		0		-		+
 * 2		+		-		0
 * 3		+		0		-
 * 4		0		+		-
 * 5		-		+		0
 * 6		-		0		+
 */

void update_rpm_tacho() {
	pwm_adc_cycles = 0;

	static uint32_t comm_counter = 0;
	comm_counter++;

	if (comm_counter == MCPWM_AVG_COM_RPM) {
		comm_counter = 0;
		uint32_t tim_val = TIM2->CNT;
		uint32_t tim_diff = tim_val - last_comm_time;
		last_comm_time = tim_val;

		if (tim_diff > 0) {
			rpm_now = ((float)MCPWM_AVG_COM_RPM * 1000000.0 * 60.0) /
					((float)tim_diff *  (float)MCPWM_NUM_POLES * 3.0);

#if MCPWM_IS_SENSORLESS
			if (rpm_now > ((float)MCPWM_MAX_CLOSED_RPM * dutycycle_now)) {
				setOpenLoop();
			}
#endif
		}
	}

	static int last_step = 0;
	int tacho_diff = 0;

	if (comm_step == 1 && last_step == 6) {
		tacho_diff++;
	} else if (comm_step == 6 && last_step == 1) {
		tacho_diff--;
	} else {
		tacho_diff += comm_step - last_step;
	}

	last_step = comm_step;

	// Tachometer
	if (direction) {
		tachometer += tacho_diff;
	} else {
		tachometer -= tacho_diff;
	}
}

void mcpwm_comm_int_handler() {
#if MCPWM_IS_SENSORLESS
	// PWM commutation in advance for next step.

	if (!(state == MC_STATE_STARTING || state == MC_STATE_RUNNING)) {
		return;
	}

	update_rpm_tacho();

	comm_step++;
	if (comm_step > 6) {
		comm_step = 1;
	}

	int next_step = comm_step + 1;
	if (next_step > 6) {
		next_step = 1;
	}

	set_next_comm_step(next_step);
#endif

}

static void set_next_comm_step(int next_step) {
	uint16_t positive_oc_mode = TIM_OCMode_PWM1;
	uint16_t negative_oc_mode = TIM_OCMode_Inactive;

	uint16_t positive_highside = TIM_CCx_Enable;
	uint16_t positive_lowside = TIM_CCxN_Enable;

	uint16_t negative_highside = TIM_CCx_Enable;
	uint16_t negative_lowside = TIM_CCxN_Enable;

	switch (pwm_mode) {
	case PWM_MODE_NONSYNCHRONOUS_LOSW:
		positive_oc_mode = TIM_OCMode_Active;
		negative_oc_mode = TIM_OCMode_PWM2;
		negative_highside = TIM_CCx_Disable;
		break;

	case PWM_MODE_NONSYNCHRONOUS_HISW:
		positive_lowside = TIM_CCxN_Disable;
		break;

	case PWM_MODE_SYNCHRONOUS:
		break;

	case PWM_MODE_BIPOLAR:
		negative_oc_mode = TIM_OCMode_PWM2;
		break;
	}

	if (next_step == 1) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_2, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_3, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_3, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_2, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, negative_lowside);
		}
	} else if (next_step == 2) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_1, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_3, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_1, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_2, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, negative_lowside);
		}
	} else if (next_step == 3) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_1, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_2, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_1, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_3, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, negative_lowside);
		}
	} else if (next_step == 4) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_3, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_2, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_2, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_3, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, negative_lowside);
		}
	} else if (next_step == 5) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_3, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_1, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_2, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_1, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, negative_lowside);
		}
	} else if (next_step == 6) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_2, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_1, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_3, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_1, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, negative_lowside);
		}
	} else {
		// Invalid phase.. stop PWM!
		TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_ForcedAction_InActive);
		TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

		TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_ForcedAction_InActive);
		TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

		TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_ForcedAction_InActive);
		TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);
	}
}
