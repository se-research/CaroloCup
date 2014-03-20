/*
 * comm.c
 *
 *  Created on: 8 mar 2011
 *      Author: benjamin
 */
#include "uart_bt.h"
#include "uart_bldc.h"
#include "clksys_driver.h"

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/eeprom.h>
#include <util/delay.h>
#include <math.h>

/*
 * Variables
 */
volatile unsigned char uart_bt_tx_buffer[UART_BT_TX_BUFF_SIZE];
volatile unsigned char uart_bt_tx_buff_read = 0;
volatile unsigned char uart_bt_tx_buff_write = 0;
volatile unsigned char uart_bt_is_transmitting = 0;

void uart_bt_init() {
	/*
	 * UARTD1
	 */
	PORTD_PIN6CTRL = PORT_OPC_WIREDANDPULL_gc;	// Pull up
	PORTD_OUTSET = _BV(7);						// TX High
	PORTD_DIRSET = _BV(7);						// TX output
	uart_bt_set_baudrate();						// Baudrate
	USARTD1_CTRLC |= 0b011;						// 8-bit character
	USARTD1_CTRLB |= _BV(3) | _BV(4);			// Enable transmitter and receiver
	USARTD1_CTRLA |= 0b00010000;				// RXC medium level interrupt

	/*
	 * Avoid delay in bootloader at next boot
	 */
	eeprom_write_byte(0, 0x10);
}

/*
 * This function calculates the optimal baudrate based on
 * the baudsetting in the .h-file. The compiler will optimize this
 * function so that it disappears almost completely
 */
void uart_bt_set_baudrate() {
	double bsel;
	signed char bscale=-7;
	double fbaudTemp=0;
	double error=255;
	unsigned int bselBest=1;
	signed char bscaleBest=1;

	bsel=((double)F_CPU/(16.0*(double)UART_BT_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = -6;
	bsel=((double)F_CPU/(16.0*(double)UART_BT_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = -5;
	bsel=((double)F_CPU/(16.0*(double)UART_BT_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}


	bscale = -4;
	bsel=((double)F_CPU/(16.0*(double)UART_BT_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = -3;
	bsel=((double)F_CPU/(16.0*(double)UART_BT_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = -2;
	bsel=((double)F_CPU/(16.0*(double)UART_BT_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = -1;
	bsel=((double)F_CPU/(16.0*(double)UART_BT_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = 0;
	bsel=(double)F_CPU/(16.0*(double)UART_BT_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = 1;
	bsel=(double)F_CPU/(16.0*(double)UART_BT_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = 2;
	bsel=(double)F_CPU/(16.0*(double)UART_BT_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = 3;
	bsel=(double)F_CPU/(16.0*(double)UART_BT_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = 4;
	bsel=(double)F_CPU/(16.0*(double)UART_BT_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = 5;
	bsel=(double)F_CPU/(16.0*(double)UART_BT_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = 6;
	bsel=(double)F_CPU/(16.0*(double)UART_BT_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	bscale = 7;
	bsel=(double)F_CPU/(16.0*(double)UART_BT_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BT_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BT_BAUDRATE);
	}

	//Here, the baud rate is set
	USARTD1_BAUDCTRLA=bselBest&0xFF;
	USARTD1_BAUDCTRLB=((bselBest>>8) & 0x0F) | (bscaleBest << 4);
}

void uart_bt_putc(unsigned char c) {
	while (!(USARTD1_STATUS & _BV(5)))	// Wait for data register empty flag
		;
	USARTD1_DATA = c;
}

void uart_bt_putc_isr(unsigned char c) {
	uart_bt_tx_buffer[uart_bt_tx_buff_write] = c;
	uart_bt_tx_buff_write++;

	if (uart_bt_tx_buff_write == UART_BT_TX_BUFF_SIZE) {
		uart_bt_tx_buff_write = 0;
	}

	if (!uart_bt_is_transmitting) {
		uart_bt_is_transmitting = 1;
		uart_bt_putc(uart_bt_tx_buffer[uart_bt_tx_buff_read]);
		uart_bt_tx_buff_read++;
		if (uart_bt_tx_buff_read == UART_BT_TX_BUFF_SIZE) {
			uart_bt_tx_buff_read = 0;
		}
	}
}

void uart_bt_put_buffer(unsigned char* buffer, unsigned int length) {
	unsigned int i;
	for (i = 0; i < length; i++) {
		uart_bt_putc(buffer[i]);
	}
}

void uart_bt_put_buffer_isr(unsigned char* buffer, unsigned int length) {
	unsigned int i;
	for (i = 0; i < length; i++) {
		uart_bt_putc_isr(buffer[i]);
	}
}

void uart_bt_puts(char* s) {
	int i;
	for (i = 0;i < 400;i++) {
		uart_bt_putc(s[i]);
		if (s[i] == '\0') {
			return;
		}
	}
}

void uart_bt_puts_isr(char* s) {
	int i;
	for (i = 0;i < 400;i++) {
		uart_bt_putc_isr(s[i]);
		if (s[i] == '\0') {
			return;
		}
	}
}

unsigned char uart_bt_wait_for_bytes(unsigned int num, unsigned int timeout, unsigned char* buffer) {
	int i;
	unsigned char to = 0, byte = 0;

	TCD1_CTRLA = 0b0111; // Prescale 1024
	TCD1_CNT = 0;

	for (i = 0; i < num; i++) {
		while (TCD1_CNT < timeout) {
			if (USARTD1_STATUS & _BV(7)) {
				TCD1_CNT = 0;
				byte = USARTD1_DATA;
				to = 0;
				break;
			}
			to = 1;
		}
		if (to) {
			TCD1_CTRLA = 0;
			TCD1_CNT = 0;
			return 0;
		} else {
			buffer[i] = byte;
		}
	}

	TCD1_CTRLA = 0;
	TCD1_CNT = 0;
	return 1;
}

unsigned char uart_bt_wait_for_string(unsigned int timeout, char* buffer, unsigned int max_len) {
	int i;
	unsigned char to = 0, byte = 0;

	TCD1_CTRLA = 0b0111; // Prescale 1024
	TCD1_CNT = 0;

	for (i = 0; i < max_len; i++) {
		while (TCD1_CNT < timeout) {
			if (USARTD1_STATUS & _BV(7)) {
				TCD1_CNT = 0;
				byte = USARTD1_DATA;
				to = 0;
				break;
			}
			to = 1;
		}
		if (to) {
			TCD1_CTRLA = 0;
			TCD1_CNT = 0;
			return 0;
		} else {
			buffer[i] = byte;
			if (byte == '\0') {
				return 1;
			}
		}
	}

	TCD1_CTRLA = 0;
	TCD1_CNT = 0;
	return 1;
}

void uart_bt_command() {
	unsigned char buffer[12];

	if (uart_bt_wait_for_bytes(1, 30000, buffer)) {
		switch (buffer[0]) {
		case 0:
			if (uart_bt_wait_for_bytes(4, 30000, buffer)) {
				uart_bldc_putc_isr('s');
				uart_bldc_put_buffer_isr(buffer, 4);
				set_mc_timeout(1000);
			}
			break;

		case 1:
			if (uart_bt_wait_for_bytes(1, 30000, buffer)) {
//				servos[0].pos = buffer[0];
//				set_servo_timeout(1000);
			}
			break;

		case 2:
			// Toggle front light
			PORTC_OUTTGL = _BV(6);
			break;

		default:
			break;
		}
	}
}

void uart_bt_value() {
	unsigned char buffer[16];

	if (uart_bt_wait_for_bytes(1, 30000, buffer)) {
		switch (buffer[0]) {
		case 'B':
			uart_bt_putc(mv_batt >> 8);
			uart_bt_putc(mv_batt);
			break;

		case 'v':
			uart_bldc_putc('v');
			if (uart_bldc_wait_for_bytes(8, 30000, buffer)) {
				uart_bt_put_buffer(buffer, 8);
			}
			break;

			default:
				break;
		}
	}
}

/*
 * RX ISR
 */
ISR(USARTD1_RXC_vect) {
	unsigned char data = USARTD1_DATA;

	switch (data) {
	case 'A':
		uart_bt_puts("Hello!\n");
		break;

	case 'B':
		eeprom_write_byte(0, 0x00);
		_delay_ms(2);
		CCPWrite(&RST.CTRL, RST_SWRST_bm);
		break;

	case 'r': // Reset this controller
		CCPWrite(&RST.CTRL, RST_SWRST_bm);
		break;

	case 'C':
		uart_bt_putc('A');
		uart_bt_command();
		break;

	case 'V':
		uart_bt_putc('A');
		uart_bt_value();
		break;

	default:
		break;
	}
}

/*
 * TX ISR
 */
ISR(USARTD1_TXC_vect) {
	if (uart_bt_tx_buff_read == uart_bt_tx_buff_write) {
		uart_bt_is_transmitting = 0;
		return;
	}

	uart_bt_putc(uart_bt_tx_buffer[uart_bt_tx_buff_read]);
	uart_bt_tx_buff_read++;
	if (uart_bt_tx_buff_read == UART_BT_TX_BUFF_SIZE) {
		uart_bt_tx_buff_read = 0;
	}
}












