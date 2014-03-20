/*
 * comm.c
 *
 *  Created on: 8 mar 2011
 *      Author: benjamin
 */
#include "uart_ft.h"
#include "clksys_driver.h"
#include "mc_driver.h"
#include "dead_reckoning.h"
#include "crc.h"
#include "sensors.h"

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/eeprom.h>
#include <avr/pgmspace.h>
#include <stdio.h>
#include <util/delay.h>
#include <math.h>

#define PUTS 0

/*
 * Variables
 */
volatile unsigned char uart_ft_tx_buffer[UART_FT_TX_BUFF_SIZE];
volatile unsigned char uart_ft_tx_buff_read = 0;
volatile unsigned char uart_ft_tx_buff_write = 0;
volatile unsigned char uart_ft_is_transmitting = 0;

// Packet variables
volatile unsigned char uart_ft_rx_state = 0;
volatile unsigned char uart_ft_rx_timeout = 0;

// Private functions
static void send_ack();
static void send_ping();

void uart_ft_init() {
	/*
	 * UARTF0
	 */
	PORTF_PIN2CTRL = PORT_OPC_WIREDANDPULL_gc;	// Pull up
	PORTF_OUTSET = _BV(3);						// TX High
	PORTF_DIRSET = _BV(3);						// TX output
	uart_ft_set_baudrate();						// Baudrate
	USARTF0_CTRLC |= 0b011;						// 8-bit character
	USARTF0_CTRLB |= _BV(3) | _BV(4);		// Enable transmitter and receiver
	USARTF0_CTRLA |= 0b00010100;			// RX and tx medium level interrupt

	/*
	 * Avoid delay in bootloader at next boot
	 */
	eeprom_write_byte(0, 0x10);
}

void uart_ft_set_baudrate() {
	double bsel;
	signed char bscale = -7;
	double fbaudTemp = 0;
	double error = 255;
	unsigned int bselBest = 1;
	signed char bscaleBest = 1;

	bsel = ((double) F_CPU / (16.0 * (double) UART_FT_BAUDRATE) - 1.0)
			* (1.0 / pow(2.0, (double) bscale));
	fbaudTemp = (double) F_CPU
			/ (16.0 * (bsel * pow(2.0, (double) bscale) + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = -6;
	bsel = ((double) F_CPU / (16.0 * (double) UART_FT_BAUDRATE) - 1.0)
			* (1.0 / pow(2.0, (double) bscale));
	fbaudTemp = (double) F_CPU
			/ (16.0 * (bsel * pow(2.0, (double) bscale) + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = -5;
	bsel = ((double) F_CPU / (16.0 * (double) UART_FT_BAUDRATE) - 1.0)
			* (1.0 / pow(2.0, (double) bscale));
	fbaudTemp = (double) F_CPU
			/ (16.0 * (bsel * pow(2.0, (double) bscale) + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = -4;
	bsel = ((double) F_CPU / (16.0 * (double) UART_FT_BAUDRATE) - 1.0)
			* (1.0 / pow(2.0, (double) bscale));
	fbaudTemp = (double) F_CPU
			/ (16.0 * (bsel * pow(2.0, (double) bscale) + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = -3;
	bsel = ((double) F_CPU / (16.0 * (double) UART_FT_BAUDRATE) - 1.0)
			* (1.0 / pow(2.0, (double) bscale));
	fbaudTemp = (double) F_CPU
			/ (16.0 * (bsel * pow(2.0, (double) bscale) + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = -2;
	bsel = ((double) F_CPU / (16.0 * (double) UART_FT_BAUDRATE) - 1.0)
			* (1.0 / pow(2.0, (double) bscale));
	fbaudTemp = (double) F_CPU
			/ (16.0 * (bsel * pow(2.0, (double) bscale) + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = -1;
	bsel = ((double) F_CPU / (16.0 * (double) UART_FT_BAUDRATE) - 1.0)
			* (1.0 / pow(2.0, (double) bscale));
	fbaudTemp = (double) F_CPU
			/ (16.0 * (bsel * pow(2.0, (double) bscale) + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = 0;
	bsel = (double) F_CPU
			/ (16.0 * (double) UART_FT_BAUDRATE * pow(2.0, (double) bscale)
					- 1.0);
	fbaudTemp = (double) F_CPU
			/ (16.0 * pow(2.0, (double) bscale) * (bsel + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = 1;
	bsel = (double) F_CPU
			/ (16.0 * (double) UART_FT_BAUDRATE * pow(2.0, (double) bscale)
					- 1.0);
	fbaudTemp = (double) F_CPU
			/ (16.0 * pow(2.0, (double) bscale) * (bsel + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = 2;
	bsel = (double) F_CPU
			/ (16.0 * (double) UART_FT_BAUDRATE * pow(2.0, (double) bscale)
					- 1.0);
	fbaudTemp = (double) F_CPU
			/ (16.0 * pow(2.0, (double) bscale) * (bsel + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = 3;
	bsel = (double) F_CPU
			/ (16.0 * (double) UART_FT_BAUDRATE * pow(2.0, (double) bscale)
					- 1.0);
	fbaudTemp = (double) F_CPU
			/ (16.0 * pow(2.0, (double) bscale) * (bsel + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = 4;
	bsel = (double) F_CPU
			/ (16.0 * (double) UART_FT_BAUDRATE * pow(2.0, (double) bscale)
					- 1.0);
	fbaudTemp = (double) F_CPU
			/ (16.0 * pow(2.0, (double) bscale) * (bsel + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = 5;
	bsel = (double) F_CPU
			/ (16.0 * (double) UART_FT_BAUDRATE * pow(2.0, (double) bscale)
					- 1.0);
	fbaudTemp = (double) F_CPU
			/ (16.0 * pow(2.0, (double) bscale) * (bsel + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = 6;
	bsel = (double) F_CPU
			/ (16.0 * (double) UART_FT_BAUDRATE * pow(2.0, (double) bscale)
					- 1.0);
	fbaudTemp = (double) F_CPU
			/ (16.0 * pow(2.0, (double) bscale) * (bsel + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	bscale = 7;
	bsel = (double) F_CPU
			/ (16.0 * (double) UART_FT_BAUDRATE * pow(2.0, (double) bscale)
					- 1.0);
	fbaudTemp = (double) F_CPU
			/ (16.0 * pow(2.0, (double) bscale) * (bsel + 1.0));
	if (bsel < 4095 && (fabs(fbaudTemp - UART_FT_BAUDRATE) < error)) {
		bscaleBest = bscale;
		bselBest = (unsigned int) bsel;
		error = fabs(fbaudTemp - UART_FT_BAUDRATE);
	}

	//Here, the baud rate is set
	USARTF0_BAUDCTRLA = bselBest & 0xFF;
	USARTF0_BAUDCTRLB = ((bselBest >> 8) & 0x0F) | (bscaleBest << 4);
}

void uart_ft_putc(unsigned char c) {
	while (!(USARTF0_STATUS & _BV(5)))
		// Wait for data register empty flag
		;
	USARTF0_DATA = c;
}

void uart_ft_putc_isr(unsigned char c) {
	uart_ft_tx_buffer[uart_ft_tx_buff_write] = c;
	uart_ft_tx_buff_write++;

	if (uart_ft_tx_buff_write == UART_FT_TX_BUFF_SIZE) {
		uart_ft_tx_buff_write = 0;
	}

	if (!uart_ft_is_transmitting) {
		uart_ft_is_transmitting = 1;
		uart_ft_putc(uart_ft_tx_buffer[uart_ft_tx_buff_read]);
		uart_ft_tx_buff_read++;
		if (uart_ft_tx_buff_read == UART_FT_TX_BUFF_SIZE) {
			uart_ft_tx_buff_read = 0;
		}
	}
}

void uart_ft_puts(char* s) {
	int i;
	for (i = 0; i < 400; i++) {
		uart_ft_putc(s[i]);
		if (s[i] == '\0') {
			return;
		}
	}
}

void uart_ft_puts_isr(char* s) {
	int i;
	for (i = 0; i < 400; i++) {
		uart_ft_putc_isr(s[i]);
		if (s[i] == '\0') {
			return;
		}
	}
}

void uart_ft_put_buffer_isr(unsigned char* buffer, unsigned int length) {
	unsigned int i;
	for (i = 0; i < length; i++) {
		uart_ft_putc_isr(buffer[i]);
	}
}

void uart_ft_put_buffer(unsigned char* buffer, unsigned int length) {
	unsigned int i;
	for (i = 0; i < length; i++) {
		uart_ft_putc(buffer[i]);
	}
}

unsigned char uart_ft_wait_for_bytes(unsigned int num, unsigned int timeout,
		unsigned char* buffer) {
	int i;
	unsigned char to = 0, byte = 0;

	TCE0_CTRLA = 0b0111; // Prescale 1024
	TCE0_CNT = 0;

	for (i = 0; i < num; i++) {
		while (TCE0_CNT < timeout) {
			if (USARTF0_STATUS & _BV(7)) {
				TCE0_CNT = 0;
				byte = USARTF0_DATA;
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

unsigned char uart_ft_wait_for_string(unsigned int timeout, char* buffer,
		unsigned int max_len) {
	int i;
	unsigned char to = 0, byte = 0;

	TCE0_CTRLA = 0b0111; // Prescale 1024
	TCE0_CNT = 0;

	for (i = 0; i < max_len; i++) {
		while (TCE0_CNT < timeout) {
			if (USARTF0_STATUS & _BV(7)) {
				TCE0_CNT = 0;
				byte = USARTF0_DATA;
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
			if (byte == '\0') {
				return 1;
			}
		}
	}

	TCE0_CTRLA = 0;
	TCE0_CNT = 0;
	return 1;
}

/*
 * Call this function at 1000 Hz.
 */
void uart_ft_timer_func() {
	if (uart_ft_rx_timeout) {
		uart_ft_rx_timeout--;
	} else {
		uart_ft_rx_state = 0;
	}
}

void uart_ft_print_string(char *buffer) {
#if PUTS
	uart_ft_puts(buffer);
	return;
#endif

	unsigned char packet[129];
	unsigned char i = 0;
	packet[0] = PACKET_OTHER; // Other packet
	packet[1] = 0; // 0 Means print to terminal

	for (i = 0; i < 128; i++) {
		packet[i + 2] = buffer[i];
		if (buffer[i] == '\0') {
			break;
		}
	}

	uart_ft_send_packet(packet, i + 2);
}

void uart_ft_send_packet(unsigned char *data, unsigned char len) {
	uart_ft_putc_isr(2);
	uart_ft_putc_isr(len);
	uart_ft_put_buffer_isr(data, len);
	unsigned short crc = crc16(data, len);
	uart_ft_putc_isr(crc >> 8);
	uart_ft_putc_isr(crc);
	uart_ft_putc_isr(3);
}

void uart_ft_process_packet(unsigned char *data, unsigned char len) {
	CAR_RES_PACKET_ID car_res_packet;
	CAR_NORES_PACKET_ID car_nores_packet;

	unsigned char buffer2[100];
	unsigned int alpha;
	unsigned char index;
	unsigned int temp;
	signed int temp_signed;
	signed long px, py;
	double tmp_d;
	static MC_VALUES mc_val;
	static SENSOR_ULTRA ultra_sensors[6];

	switch (data[0]) {
	case 0:
		/*
		 * Enter bootloader
		 */
		eeprom_write_byte(0, 0x00);
		_delay_ms(2);
		CCPWrite(&RST .CTRL, RST_SWRST_bm);
		break;

	case 1:
		/*
		 * Packet that expects response
		 */
		car_res_packet = data[1];
		data += 2;
		switch (car_res_packet) {
		case CAR_PACKET_READ_VALUES:
			mc_get_values_struct(&mc_val);
			index = 0;
			temp = (unsigned int)((mc_val.v_batt - 0.2) * 1000.0);
			buffer2[index++] = CAR_PACKET_READ_VALUES;
			buffer2[index++] = temp >> 8;
			buffer2[index++] = temp;
			temp = (unsigned int)(mc_val.v_log * 1000.0);
			buffer2[index++] = temp >> 8;
			buffer2[index++] = temp;
			temp_signed = (signed int)(mc_val.temp * 100.0);
			buffer2[index++] = temp_signed >> 8;
			buffer2[index++] = temp_signed;
			temp_signed = (signed int)(mc_val.speed * 100.0);
			buffer2[index++] = temp_signed >> 8;
			buffer2[index++] = temp_signed;
			uart_ft_send_packet(buffer2, index);
			break;

		case CAR_PACKET_READ_POS:
			/*
			 * Read dead reckoning position.
			 */
			px = (signed long) dr_x_pos;
			py = (signed long) dr_y_pos;
			alpha = (unsigned int) (dr_angle * 10000.0);
			index = 0;

			buffer2[index++] = CAR_PACKET_READ_POS;
			buffer2[index++] = px >> 24;
			buffer2[index++] = px >> 16;
			buffer2[index++] = px >> 8;
			buffer2[index++] = px;
			buffer2[index++] = py >> 24;
			buffer2[index++] = py >> 16;
			buffer2[index++] = py >> 8;
			buffer2[index++] = py;
			buffer2[index++] = alpha >> 8;
			buffer2[index++] = alpha;

			uart_ft_send_packet(buffer2, index);
			break;

		case CAR_PACKET_READ_SENS_ULTRA:
			index = 0;
			sensors_read_ultra(ultra_sensors);

			buffer2[index++] = CAR_PACKET_READ_SENS_ULTRA;
			buffer2[index++] = ultra_sensors[0].address;
			buffer2[index++] = ultra_sensors[1].address;
			buffer2[index++] = ultra_sensors[2].address;
			buffer2[index++] = ultra_sensors[3].address;
			buffer2[index++] = ultra_sensors[4].address;
			buffer2[index++] = ultra_sensors[5].address;
			buffer2[index++] = ultra_sensors[0].value >> 8;
			buffer2[index++] = ultra_sensors[0].value;
			buffer2[index++] = ultra_sensors[1].value >> 8;
			buffer2[index++] = ultra_sensors[1].value;
			buffer2[index++] = ultra_sensors[2].value >> 8;
			buffer2[index++] = ultra_sensors[2].value;
			buffer2[index++] = ultra_sensors[3].value >> 8;
			buffer2[index++] = ultra_sensors[3].value;
			buffer2[index++] = ultra_sensors[4].value >> 8;
			buffer2[index++] = ultra_sensors[4].value;
			buffer2[index++] = ultra_sensors[5].value >> 8;
			buffer2[index++] = ultra_sensors[5].value;
			uart_ft_send_packet(buffer2, index);
			break;

		case CAR_PACKET_READ_SENS_IR:
			break;

		case CAR_PACKET_PING:
			send_ping();
			break;

		default:
			break;
		}
		break;

	case 2:
		/*
		 * Packet that expects no response
		 */
		car_nores_packet = data[1];
		data += 2;
		switch (car_nores_packet) {
		case CAR_PACKET_SET_POWER_SERVO:
			tmp_d = (double)((signed int)data[0] << 8 | (signed int)data[1]) / 100;
			if (tmp_d > MC_MAX_SPEED) {
				tmp_d = MC_MAX_SPEED;
			} else if (tmp_d < -MC_MAX_SPEED) {
				tmp_d = -MC_MAX_SPEED;
			}
			mc_set_speed(tmp_d);
			mc_set_servo_position(data[2], 0);
			break;

		case CAR_PACKET_WRITE_POS:
			index = 0;
			px = ((signed long)data[index] << 24) |
					((signed long)data[index + 1] << 16) |
					((signed long)data[index + 2] << 8) |
					((signed long)data[index + 3]);
			index += 4;

			py = ((signed long)data[index] << 24) |
					((signed long)data[index + 1] << 16) |
					((signed long)data[index + 2] << 8) |
					((signed long)data[index + 3]);
			index += 4;

			alpha = ((unsigned int)data[index] << 8) |
					((unsigned int)data[index + 1]);
			index += 2;

			dr_x_pos = (double)px;
			dr_y_pos = (double)py;
			dr_angle = ((double)alpha) / 10000.0;
			break;

		default:
			break;
		}

		send_ack();
		break;

	case 'A': //65
		/*
		 * Test string
		 */
		uart_ft_print_string("Hello!\n");
		break;

	case 'p':
		sprintf((char*) buffer2, "X: %.0f Y: %.0f STD: %.0f\n", dr_x_pos,
				dr_y_pos, dr_sigma);
		uart_ft_print_string((char*) buffer2);
		break;

	default:
		break;
	}
}

/*
 * State machine for receiving packets.
 *
 * Packet format:
 * 126
 * len
 * data
 * crc_high
 * crc_low
 * 126
 */ISR(USARTF0_RXC_vect) {
	static unsigned char payload_length = 0;
	static unsigned char rx_buffer[140];
	static unsigned char rx_data_ptr = 0;
	static unsigned char crc_low = 0;
	static unsigned char crc_high = 0;
	unsigned char rx_data;

#define UART_FT_RX_TIMEOUT	2;

	// Continue while there is data in the receive data
	while (USARTF0_STATUS & _BV(7)) {
		rx_data = USARTF0_DATA;

		switch (uart_ft_rx_state) {
		case 0:
			if (rx_data == 2) {
				uart_ft_rx_state++;
				uart_ft_rx_timeout = UART_FT_RX_TIMEOUT
				;
				rx_data_ptr = 0;
			} else {
				uart_ft_rx_state = 0;
			}
			break;

		case 1:
			payload_length = rx_data;
			uart_ft_rx_state++;
			uart_ft_rx_timeout = UART_FT_RX_TIMEOUT
			;
			break;

		case 2:
			rx_buffer[rx_data_ptr++] = rx_data;
			if (rx_data_ptr == payload_length) {
				uart_ft_rx_state++;
			}
			uart_ft_rx_timeout = UART_FT_RX_TIMEOUT
			;
			break;

		case 3:
			crc_high = rx_data;
			uart_ft_rx_state++;
			uart_ft_rx_timeout = UART_FT_RX_TIMEOUT
			;
			break;

		case 4:
			crc_low = rx_data;
			uart_ft_rx_state++;
			uart_ft_rx_timeout = UART_FT_RX_TIMEOUT
			;
			break;

		case 5:
			if (rx_data == 3) {
				if (crc16(rx_buffer, payload_length)
						== ((unsigned short) crc_high << 8
								| (unsigned short) crc_low)) {
					// Packet received!
					uart_ft_process_packet(rx_buffer, payload_length);
				}
			}

			uart_ft_rx_state = 0;
			break;

		default:
			uart_ft_rx_state = 0;
			break;
		}
	}
}

/*
 * TX ISR
 */ISR(USARTF0_TXC_vect) {
	if (uart_ft_tx_buff_read == uart_ft_tx_buff_write) {
		uart_ft_is_transmitting = 0;
		return;
	}

	uart_ft_putc(uart_ft_tx_buffer[uart_ft_tx_buff_read]);
	uart_ft_tx_buff_read++;
	if (uart_ft_tx_buff_read == UART_FT_TX_BUFF_SIZE) {
		uart_ft_tx_buff_read = 0;
	}
}

static void send_ack() {
	unsigned char byte = PACKET_NORES_ACK;
	uart_ft_send_packet(&byte, 1);
}

static void send_ping() {
	unsigned char byte = CAR_PACKET_PING;
	uart_ft_send_packet(&byte, 1);
}
