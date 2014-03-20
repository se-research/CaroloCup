/*
 * uartd0.c
 *
 *  Created on: 6 aug 2011
 *      Author: benjamin
 */

#include "uart_bldc.h"

#include <avr/io.h>
#include <avr/interrupt.h>
#include <math.h>

/*
 * Variables
 */
unsigned char uart_bldc_buffer[UART_BLDC_BUFF_SIZE];
volatile unsigned char uart_bldc_buff_read = 0;
volatile unsigned char uart_bldc_buff_write = 0;
volatile unsigned char uart_bldc_is_transmitting = 0;

void uart_bldc_init() {
	/*
	 * UARTE0
	 */
	PORTE_PIN2CTRL = PORT_OPC_WIREDANDPULL_gc;	// Pull up
	PORTE_OUTSET = _BV(3);						// TX High
	PORTE_DIRSET = _BV(3);						// TX output
	uart_bldc_set_baudrate();					// Baudrate
	USARTE0_CTRLC |= 0b011;						// 8-bit character
	USARTE0_CTRLB |= _BV(3) | _BV(4);			// Enable transmitter and receiver
	USARTE0_CTRLA |= _BV(2);					// TX low level interrupt
}

/*
 * This function calculates the optimal baudrate based on
 * the baudsetting in the .h-file. The compiler will optimize this
 * function so that it disappears almost completely
 */
void uart_bldc_set_baudrate() {
	double bsel;
	signed char bscale=-7;
	double fbaudTemp=0;
	double error=255;
	unsigned int bselBest=1;
	signed char bscaleBest=1;

	bsel=((double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = -6;
	bsel=((double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = -5;
	bsel=((double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}


	bscale = -4;
	bsel=((double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = -3;
	bsel=((double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = -2;
	bsel=((double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = -1;
	bsel=((double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = 0;
	bsel=(double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = 1;
	bsel=(double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = 2;
	bsel=(double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = 3;
	bsel=(double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = 4;
	bsel=(double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = 5;
	bsel=(double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = 6;
	bsel=(double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	bscale = 7;
	bsel=(double)F_CPU/(16.0*(double)UART_BLDC_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BLDC_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BLDC_BAUDRATE);
	}

	//Here, the baud rate is set
	USARTE0_BAUDCTRLA=bselBest&0xFF;
	USARTE0_BAUDCTRLB=((bselBest>>8) & 0x0F) | (bscaleBest << 4);
}

void uart_bldc_putc(unsigned char c) {
	while (!(USARTE0_STATUS & _BV(5)))	// Wait for data register empty flag
		;
	USARTE0_DATA = c;
}

void uart_bldc_puts_isr(char* s) {
	int i;
	for (i = 0;i < 400;i++) {
		uart_bldc_putc_isr(s[i]);
		if (s[i] == '\0') {
			return;
		}
	}
}

void uart_bldc_put_buffer_isr(unsigned char* buffer, unsigned int length) {
	unsigned int i;
	for (i = 0; i < length; i++) {
		uart_bldc_putc_isr(buffer[i]);
	}
}

void uart_bldc_put_buffer(unsigned char* buffer, unsigned int length) {
	unsigned int i;
	for (i = 0; i < length; i++) {
		uart_bldc_putc(buffer[i]);
	}
}

unsigned char uart_bldc_wait_for_bytes(unsigned int num,
		unsigned int timeout, unsigned char* buffer) {
	int i;
	unsigned char to = 0, byte = 0;

	TCE0_CTRLA = 0b0111; // Prescale 1024
	TCE0_CNT = 0;

	for (i = 0; i < num; i++) {
		while (TCE0_CNT < timeout) {
			if (USARTE0_STATUS & _BV(7)) {
				TCE0_CNT = 0;
				byte = USARTE0_DATA;
				to = 0;
				break;
			}
			to = 1;
		}
		if (to) {
			TCE0_CTRLA = 0;
			TCE0_CNT = 0;
			return 0;
		} else {
			buffer[i] = byte;
		}
	}

	TCE0_CTRLA = 0;
	TCE0_CNT = 0;
	return 1;
}

void uart_bldc_putc_isr(unsigned char c) {
	uart_bldc_buffer[uart_bldc_buff_write] = c;
	uart_bldc_buff_write++;

	if (uart_bldc_buff_write == UART_BLDC_BUFF_SIZE) {
		uart_bldc_buff_write = 0;
	}

	if (!uart_bldc_is_transmitting) {
		uart_bldc_is_transmitting = 1;
		uart_bldc_putc(uart_bldc_buffer[uart_bldc_buff_read]);
		uart_bldc_buff_read++;
		if (uart_bldc_buff_read == UART_BLDC_BUFF_SIZE) {
			uart_bldc_buff_read = 0;
		}
	}
}

void uart_bldc_flush_rx() {
	volatile unsigned char tmp;

	while(USARTE0_STATUS & _BV(7)) {
		tmp = USARTE0_DATA;
	}
}

/*
 * TX ISR
 */
ISR(USARTE0_TXC_vect) {
	if (uart_bldc_buff_read == uart_bldc_buff_write) {
		uart_bldc_is_transmitting = 0;
		return;
	}

	uart_bldc_putc(uart_bldc_buffer[uart_bldc_buff_read]);
	uart_bldc_buff_read++;
	if (uart_bldc_buff_read == UART_BLDC_BUFF_SIZE) {
		uart_bldc_buff_read = 0;
	}
}
