/*
 * uartd0.h
 *
 *  Created on: 6 aug 2011
 *      Author: benjamin
 */

#ifndef UART_BLDC_H_
#define UART_BLDC_H_

#include "main.h"

/*
 * Settings
 */
#define UART_BLDC_BUFF_SIZE		40
#define UART_BLDC_BAUDRATE		115200.0

/*
 * Functions
 */
void uart_bldc_init();
void uart_bldc_set_baudrate();
void uart_bldc_putc(unsigned char c);
void uart_bldc_puts_isr(char* s);
void uart_bldc_put_buffer_isr(unsigned char* buffer, unsigned int length);
void uart_bldc_put_buffer(unsigned char* buffer, unsigned int length);
unsigned char uart_bldc_wait_for_bytes(unsigned int num,
		unsigned int timeout, unsigned char* buffer);
void uart_bldc_putc_isr(unsigned char c);
void uart_bldc_flush_rx();

#endif /* UARTD0_H_ */
