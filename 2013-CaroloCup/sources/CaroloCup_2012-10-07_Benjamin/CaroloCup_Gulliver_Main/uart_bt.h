/*
 * uart_bt_.h
 *
 *  Created on: 8 mar 2011
 *      Author: benjamin
 */

#ifndef UART_BT_H_
#define UART_BT_H_

#include "main.h"

/*
 * Settings
 */
#define UART_BT_TX_BUFF_SIZE	40
#define UART_BT_BAUDRATE		115200.0

/*
 * Functions
 */
void uart_bt_init();
void uart_bt_set_baudrate();
void uart_bt_putc(unsigned char c);
void uart_bt_putc_isr(unsigned char c);
void uart_bt_puts(char* s);
void uart_bt_puts_isr(char* s);
void uart_bt_put_buffer(unsigned char* buffer, unsigned int length);
void uart_bt_put_buffer_isr(unsigned char* buffer, unsigned int length);

#endif /* COMM_H_ */
