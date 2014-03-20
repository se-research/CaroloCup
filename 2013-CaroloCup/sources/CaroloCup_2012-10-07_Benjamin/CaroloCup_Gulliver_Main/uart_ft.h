/*
 * uart_bt_.h
 *
 *  Created on: 8 mar 2011
 *      Author: benjamin
 */

#ifndef UART_FT_H_
#define UART_FT_H_

#include "main.h"

/*
 * Settings
 */
#define UART_FT_TX_BUFF_SIZE		135
#define UART_FT_BAUDRATE			115200.0

// Packets that expect response
typedef enum {
	CAR_PACKET_READ_VALUES = 0,
	CAR_PACKET_READ_POS,
	CAR_PACKET_READ_SENS_ULTRA,
	CAR_PACKET_READ_SENS_IR,
	CAR_PACKET_PING
} CAR_RES_PACKET_ID;

// Packets that only expect ack
typedef enum {
	CAR_PACKET_SET_POWER_SERVO = 0,
	CAR_PACKET_WRITE_POS
} CAR_NORES_PACKET_ID;

#define PACKET_NORES_ACK	254
#define PACKET_OTHER		255

/*
 * Functions
 */
void uart_ft_init();
void uart_ft_set_baudrate();
void uart_ft_putc(unsigned char c);
void uart_ft_puts(char* s);
void uart_ft_puts_isr(char* s);
void uart_ft_put_buffer(unsigned char* buffer, unsigned int length);
void uart_ft_put_buffer_isr(unsigned char* buffer, unsigned int length);
void uart_ft_timer_func();
void uart_ft_send_packet(unsigned char *data, unsigned char len);
void uart_ft_print_string(char *buffer);

#endif /* COMM_H_ */
