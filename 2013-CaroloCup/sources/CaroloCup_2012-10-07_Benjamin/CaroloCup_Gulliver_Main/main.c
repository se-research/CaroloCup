/*
 * main.c
 *
 *  Created on: 8 mar 2011
 *      Author: benjamin
 */

#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/interrupt.h>
#include <util/delay.h>
#include <stdlib.h>
#include <stdio.h>
#include <avr/pgmspace.h>
#include <avr/eeprom.h>
#include <math.h>

#include "clksys_driver.h"
#include "ir.h"
#include "servo.h"
#include "uart_bt.h"
#include "uart_bldc.h"
#include "uart_ft.h"
#include "TC_driver.h"
#include "mc_driver.h"
#include "uart_bus.h"
#include "nine_dof.h"
#include "sensors.h"
#include "nine_dof.h"

#include "main.h"
#include "dead_reckoning.h"

/*
 * Timers used:
 *
 * TCD0: 1ms interrupts
 * TCD1: Servo
 * TCE0: Timeout uart
* TCC1: ADC
 * TCC0: IR
 *
 */

/*seededCrc16
 * defines
 */
#define PWM_MAX		3600L

/*
 * Variables
 */
volatile unsigned int gADC_CH0_ZeroOffset = 0;
volatile signed int mv_batt = 0;
volatile unsigned int mc_timeout = 0;
volatile unsigned char g_time_0;
volatile double dr_x_pos = MAIN_DEFAULT_X;
volatile double dr_y_pos = MAIN_DEFAULT_Y;
volatile double dr_sigma = 10;					// Can not be initialised as 0
volatile double dr_angle = 0;
volatile unsigned char car_address = 0;
volatile unsigned long clk = 0;

/*
 * Functions
 */
uint8_t ReadCalibrationByte(uint8_t index);

void init() {
	/*  Enable internal 32 MHz calibrated oscillator and check for
	 *  it to be stable. Set the 32 MHz oscillator as the main
	 *  clock source.
	 */
//	CLKSYS_Enable(OSC_RC32MEN_bm);
//	while (CLKSYS_IsReady(OSC_RC32MRDY_bm) == 0)
//		;
//	CLKSYS_Main_ClockSource_Select(CLK_SCLKSEL_RC32M_gc);
//	CLKSYS_Disable(OSC_RC2MEN_bm);

	/*
	 * External crystal oscillator
	 */
	CLKSYS_XOSC_Config(OSC_FRQRANGE_12TO16_gc, false, OSC_XOSCSEL_XTAL_1KCLK_gc);
	CLKSYS_Enable( OSC_XOSCEN_bm );
	CLKSYS_PLL_Config(OSC_PLLSRC_XOSC_gc, 8);
	CLKSYS_Enable( OSC_PLLEN_bm );
	CLKSYS_Prescalers_Config(CLK_PSADIV_4_gc, CLK_PSBCDIV_1_1_gc);
	do {
	} while (CLKSYS_IsReady( OSC_PLLRDY_bm ) == 0);
	CLKSYS_Main_ClockSource_Select(CLK_SCLKSEL_PLL_gc);
	CLKSYS_Disable(OSC_RC2MEN_bm);

	// LEDs
	PORTC_DIRSET = _BV(0) | _BV(1);

	// Pull-ups in switches
	PORTB_PIN0CTRL = PORT_OPC_WIREDANDPULL_gc;
	PORTB_PIN1CTRL = PORT_OPC_WIREDANDPULL_gc;
	PORTB_PIN2CTRL = PORT_OPC_WIREDANDPULL_gc;
	PORTB_PIN3CTRL = PORT_OPC_WIREDANDPULL_gc;
	PORTB_PIN4CTRL = PORT_OPC_WIREDANDPULL_gc;
	PORTB_PIN5CTRL = PORT_OPC_WIREDANDPULL_gc;
	PORTB_PIN6CTRL = PORT_OPC_WIREDANDPULL_gc;
	PORTB_PIN7CTRL = PORT_OPC_WIREDANDPULL_gc;
	// ADC Offset
	PORTA_DIRSET = _BV(5);
	PORTA_OUTCLR = _BV(5);

	/*
	 * ADC
	 */
	// Setup heartbeat trigger for ADC conversion
	TCC1_PER = 5000;
	TCC1_CTRLA = TC_CLKSEL_DIV8_gc;
	TCC1_CTRLB = TC0_CCAEN_bm;

	// setup event0 and event1 when TCC1 overflows
	EVSYS.CH0MUX = 0xC8; // TCC0 Overflow

	// read low ADCA calibration byte from NVM signature row into register
	ADCA.CALL = ReadCalibrationByte(offsetof(NVM_PROD_SIGNATURES_t, ADCACAL0));
	// read high ADCA calibration byte from NVM signature row into register
	ADCA.CALH = ReadCalibrationByte(offsetof(NVM_PROD_SIGNATURES_t, ADCACAL1));

	// setup adc for single ended, unsigned sampling on PORTA:5 to calibrate ADC offset
	ADCA.CTRLA |= 0x1; // enable adc
	ADCA.CTRLB = ADC_RESOLUTION_12BIT_gc; // 12 bit conversion
	ADCA.REFCTRL = ADC_REFSEL_INT1V_gc | 0x02; // internal 1V bandgap reference
	ADCA.PRESCALER = ADC_PRESCALER_DIV64_gc; // peripheral clk/64
	ADCA.CH0.CTRL = ADC_CH_INPUTMODE_SINGLEENDED_gc; // single ended
	ADCA.CH0.MUXCTRL = ADC_CH_MUXPOS_PIN5_gc; // PORTA:5

	unsigned char i;
	unsigned long sum0 = 0;
	unsigned int max0 = 0, min0 = 8000, tmp;


	for (i = 0; i < 66; i++) {
		// trigger single conversion on ch0
		ADCA.CTRLA |= _BV(2);
		// wait for result
		while (!ADCA.CH0.INTFLAGS)
			; // wait for conmain_old_dr_corr_rcmversion complete flag
		ADCA.CH0.INTFLAGS = ADC_CH_CHIF_bm; // clear int flags (cleared by writing 1)

		tmp = ADCA_CH0RES;
		sum0 += tmp;
		if (tmp > max0) {
			max0 = tmp;
		}
		if (tmp < min0) {
			min0 = tmp;
		}
	}
	gADC_CH0_ZeroOffset = (sum0 - max0 - min0) >> 6;

	// setup adc to read from PORTA:0 and interrupt on conversion complete
	ADCA.CH0.MUXCTRL = ADC_CH_MUXPOS_PIN0_gc; // PORTA:0
	ADCA.CH0.INTCTRL = ADC_CH_INTLVL_MED_gc; // medium level interrupt
	// Trigger ch0 conversion on event0 and ch1 conversion on event1
	ADCA.EVCTRL = ADC_EVSEL_0123_gc | ADC_EVACT_CH01_gc;

	// Timer TCD0: 1000Hz interrupts
	TCD0_CTRLA = 0x05; // Prescaler: clk/64
	TCD0_PER = 500; // 1000Hz
	TCD0_INTCTRLA = 0x03; // Timer overflow is a high level interrupt

//	ir_init();
//	init_servo();
	uart_bt_init();

	uart_bldc_init();
	uart_ft_init();
	uart_bus_init();

	// Enable all level interrupts
	PMIC_CTRL = PMIC_LOLVLEN_bm | PMIC_MEDLVLEN_bm | PMIC_HILVLEN_bm;
	sei();

	uart_ft_print_string("Reset done!\n");
}

void set_mc_timeout(unsigned int ms) {
	mc_timeout = ms;
}

void set_servo_position(unsigned char pos, unsigned char speed) {
	unsigned char buffer[4];

	buffer[0] = 'S';
	buffer[1] = 0;
	buffer[2] = pos;
	buffer[3] = speed;

	uart_bldc_putc_isr('s');
	uart_bldc_put_buffer_isr(buffer, 4);
}

void updateClk(unsigned long newclock)
{
	cli();
	clk = newclock;
	sei();
}

ISR(TCD0_OVF_vect) {
	clk += 1;
	uart_ft_timer_func();

	if (g_time_0) {
		g_time_0--;
	}

	if (mc_timeout) {
		mc_timeout--;
	} else {
//		mc_set_motor_power(4000, 2, 0);
	}

	static unsigned char last_addr = 1;
	car_address = ~(PORTB_IN | _BV(7));
	if (car_address != last_addr) {
		last_addr = car_address;
	}
}

#define R1_1	330L
#define R2_1	22L

ISR(ADCA_CH0_vect) {
	static unsigned long tot = 0;
	static unsigned char i = 0;
	static unsigned int max, min;

	unsigned int tmp_ad = ADCA_CH0RES;
	tot += tmp_ad;
	if (tmp_ad < min) {
		min = tmp_ad;
	}
	if (tmp_ad > max) {
		max = tmp_ad;
	}
	i++;

	if (i == 66) {
		signed long adv = ((tot - max - min) >> 6) - gADC_CH0_ZeroOffset;
		mv_batt = (signed int) ((adv * (R1_1 + R2_1) * 1000) / (R2_1 * 4096));

		max = 0;
		min = 8000;
		tot = 0;
		i = 0;
	}
}

uint8_t ReadCalibrationByte(uint8_t index) {
	uint8_t result;

	/* Load the NVM Command register to read the calibration row. */
	NVM_CMD = NVM_CMD_READ_CALIB_ROW_gc;
	result = pgm_read_byte(index);

	/* Clean up NVM Command register. */
	NVM_CMD = NVM_CMD_NO_OPERATION_gc;

	return (result);
}

int main() {
	init();

	dr_compass_reset_offset();
	mc_set_servo_position(MC_SERVO_CENTER, 0);

	for (;;) {
		g_time_0 = 10;
		dr_update_position_angle((double*) &dr_x_pos, (double*) &dr_y_pos, (double*) &dr_angle);

		LED1_TGL;
		while(g_time_0){};
	}

	return 0;
}

