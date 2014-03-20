/*
 * uartd0.c
 *
 *  Created on: 6 aug 2011
 *      Author: benjamin
 */

#include "uart_bus.h"

#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>
#include <math.h>

/*
 * Variables
 */
unsigned char uart_bus_buffer[UART_BUS_BUFF_SIZE];
volatile unsigned char uart_bus_buff_read = 0;
volatile unsigned char uart_bus_buff_write = 0;
volatile unsigned char uart_bus_is_transmitting = 0;

void uart_bus_init() {
	/*
	 * UARTC0
	 */
	PORTC_PIN2CTRL = PORT_OPC_WIREDANDPULL_gc;	// RX pullup
	PORTC_OUTSET = _BV(3);						// TX High
	PORTC_DIRSET = _BV(3);						// TX output
	uart_bus_set_baudrate();					// Baudrate
	USARTC0_CTRLC = 0b111;						// 9-bit character
	USARTC0_CTRLB = _BV(4) | _BV(3);			// Enable receiver and transmitter
	USARTC0_CTRLA = 0b00000100;					// TXC low low interrupt
}

/*
 * This function calculates the optimal baudrate based on
 * the baudsetting in the .h-file. The compiler will optimize this
 * function so that it disappears almost completely
 */
void uart_bus_set_baudrate() {
	double bsel;
	signed char bscale=-7;
	double fbaudTemp=0;
	double error=255;
	unsigned int bselBest=1;
	signed char bscaleBest=1;

	bsel=((double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = -6;
	bsel=((double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = -5;
	bsel=((double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}


	bscale = -4;
	bsel=((double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = -3;
	bsel=((double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = -2;
	bsel=((double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = -1;
	bsel=((double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE)-1.0)*(1.0/pow(2.0,(double)bscale));
	fbaudTemp=(double)F_CPU/(16.0*(bsel*pow(2.0,(double)bscale)+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = 0;
	bsel=(double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = 1;
	bsel=(double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = 2;
	bsel=(double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = 3;
	bsel=(double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = 4;
	bsel=(double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = 5;
	bsel=(double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = 6;
	bsel=(double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	bscale = 7;
	bsel=(double)F_CPU/(16.0*(double)UART_BUS_BAUDRATE*pow(2.0,(double)bscale)-1.0);
	fbaudTemp=(double)F_CPU/(16.0*pow(2.0,(double)bscale)*(bsel+1.0));
	if(bsel < 4095 && (fabs(fbaudTemp-UART_BUS_BAUDRATE)<error)) {
		bscaleBest=bscale;
		bselBest=(unsigned int)bsel;
		error=fabs(fbaudTemp-UART_BUS_BAUDRATE);
	}

	//Here, the baud rate is set
	USARTC0_BAUDCTRLA=bselBest&0xFF;
	USARTC0_BAUDCTRLB=((bselBest>>8) & 0x0F) | (bscaleBest << 4);
}

/*
 * Choose which controller to talk with.
 */
void uart_bus_set_address(unsigned char address) {
	loop_until_bit_is_set(USARTC0_STATUS, 5);	// Wait for data register empty flag
	USARTC0_CTRLB |= _BV(0);					// Set ninth bit
	USARTC0_DATA = address;
}

void uart_bus_putc(unsigned char c) {
	loop_until_bit_is_set(USARTC0_STATUS, 5);	// Wait for data register empty flag
	USARTC0_CTRLB &= ~_BV(0);					// Clear ninth bit
	USARTC0_DATA = c;
}

void uart_bus_flush_rx() {
	volatile unsigned char tmp;

	while(USARTC0_STATUS & _BV(7)) {
		tmp = USARTC0_DATA;
	}
}

void uart_bus_puts_isr(char* s) {
	int i;
	for (i = 0;i < 400;i++) {
		uart_bus_putc_isr(s[i]);
		if (s[i] == '\0') {
			return;
		}
	}
}

void uart_bus_put_buffer_isr(unsigned char* buffer, unsigned int length) {
	unsigned int i;
	for (i = 0; i < length; i++) {
		uart_bus_putc_isr(buffer[i]);
	}
}

void uart_bus_put_buffer(unsigned char* buffer, unsigned int length) {
	unsigned int i;
	for (i = 0; i < length; i++) {
		uart_bus_putc(buffer[i]);
	}
}

unsigned char uart_bus_wait_for_bytes(unsigned int num,
		unsigned int timeout, unsigned char* buffer) {
	int i;
	unsigned char to = 0, byte = 0;

	TCF0_CTRLA = 0b0111; // Prescale 1024
	TCF0_CNT = 0;

	for (i = 0; i < num; i++) {
		while (TCF0_CNT < timeout) {
			if (USARTC0_STATUS & _BV(7)) {
				TCF0_CNT = 0;
				byte = USARTC0_DATA;
				to = 0;
				break;
			}
			to = 1;
		}
		if (to) {
			TCF0_CTRLA = 0;
			TCF0_CNT = 0;
			return 0;
		} else {
			buffer[i] = byte;
		}
	}

	TCF0_CTRLA = 0;
	TCF0_CNT = 0;
	return 1;
}

void uart_bus_putc_isr(unsigned char c) {
	uart_bus_buffer[uart_bus_buff_write] = c;
	uart_bus_buff_write++;

	if (uart_bus_buff_write == UART_BUS_BUFF_SIZE) {
		uart_bus_buff_write = 0;
	}

	if (!uart_bus_is_transmitting) {
		uart_bus_is_transmitting = 1;
		uart_bus_putc(uart_bus_buffer[uart_bus_buff_read]);
		uart_bus_buff_read++;
		if (uart_bus_buff_read == UART_BUS_BUFF_SIZE) {
			uart_bus_buff_read = 0;
		}
	}
}

/*
 * TX ISR
 */
ISR(USARTC0_TXC_vect) {
	if (uart_bus_buff_read == uart_bus_buff_write) {
		uart_bus_is_transmitting = 0;
		return;
	}

	uart_bus_putc(uart_bus_buffer[uart_bus_buff_read]);
	uart_bus_buff_read++;
	if (uart_bus_buff_read == UART_BUS_BUFF_SIZE) {
		uart_bus_buff_read = 0;
	}
}
