/*
 * uartd0.h
 *
 *  Created on: 6 aug 2011
 *      Author: benjamin
 */

#ifndef UART_BUS_H_
#define UART_BUS_H_

#include "main.h"

/*
 * Settings
 */
#define UART_BUS_BUFF_SIZE		40
#define UART_BUS_BAUDRATE		125000.0

/*
 * Addresses
 */
#define UART_BUS_NINEDOF_ADDR	4
#define UART_BUS_SENSORS_ADDR	7
#define UART_BUS_MICAZ_ADDR		11

/*
 * Functions
 */
void uart_bus_init();
void uart_bus_set_baudrate();
void uart_bus_set_address(unsigned char address);
void uart_bus_putc(unsigned char c);
void uart_bus_puts_isr(char* s);
void uart_bus_put_buffer_isr(unsigned char* buffer, unsigned int length);
void uart_bus_put_buffer(unsigned char* buffer, unsigned int length);
unsigned char uart_bus_wait_for_bytes(unsigned int num,
		unsigned int timeout, unsigned char* buffer);
void uart_bus_putc_isr(unsigned char c);
void uart_bus_flush_rx();

#endif /* UARTD0_BUS_ */
