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
 * comm.c
 *
 *  Created on: 22 nov 2012
 *      Author: benjamin
 */

#include "comm.h"
#include "main.h"
#include "stm32f4xx_conf.h"
#include "usbd_cdc_vcp.h"
#include "dead_reckoning.h"
#include "drive.h"
#include "autopilot.h"
#include <math.h>
#include <string.h>

// Private functions
static void send_ack();
static void send_ping();
static int usart_wait_for_bytes(int byte_num, uint8_t *buffer, int timeout_ms);
static void usart_clear_pending_bytes();
static void delay_us(int us);
static void handle_res_packet(unsigned char *data, unsigned char len);
static void handle_nores_packet(unsigned char *data, unsigned char len);

// Variables
static volatile int override_power_steering = 0;
static volatile unsigned char res_packet_buffer[256];
static volatile unsigned char nores_packet_buffer[256];
static volatile unsigned char res_packet_len = 0;
static volatile unsigned char nores_packet_len = 0;

// Macros
#define SET_UART_BUS_ADDRESS(address)	USART_SendData(USART6, (1 << 8) | address);

void comm_handle_nores_packet(unsigned char *data, unsigned char len) {
	if (!nores_packet_len) {
		memcpy((unsigned int*)nores_packet_buffer, data, len);
		nores_packet_len = len;
	}
}

void comm_handle_res_packet(unsigned char *data, unsigned char len) {
	if (!res_packet_len) {
		memcpy((unsigned int*)res_packet_buffer, data, len);
		res_packet_len = len;
	}
}

void comm_override_power_steering(int is_override) {
	override_power_steering = is_override;
}

static int usart_wait_for_bytes(int byte_num, uint8_t *buffer, int timeout_ms) {
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM13, ENABLE);
	uint16_t PrescalerValue = (uint16_t) ((SystemCoreClock / 2) / 1000) - 1;

	TIM_Cmd(TIM13, DISABLE);

	// Time base configuration
	TIM_TimeBaseStructure.TIM_Period = 0xFFFF;
	TIM_TimeBaseStructure.TIM_Prescaler = PrescalerValue;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM13, &TIM_TimeBaseStructure);

	// TIM13 enable counter
	TIM_Cmd(TIM13, ENABLE);

	TIM13->CNT = 0;

	int index = 0;

	while (byte_num) {
		if (USART_GetFlagStatus(USART6, USART_FLAG_RXNE) != RESET) {
			uint8_t recByte = USART_ReceiveData(USART6) & 0xFF;
			buffer[index++] = recByte;
			byte_num--;
		}

		if (TIM13->CNT >= timeout_ms) {
			return -1;
		}
	}

	return 0;
}

static void usart_clear_pending_bytes() {
	while (USART_GetFlagStatus(USART6, USART_FLAG_RXNE) != RESET) {
				USART_ReceiveData(USART6);
	}
}

static void delay_us(int us) {
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM13, ENABLE);
	uint16_t PrescalerValue = (uint16_t) ((SystemCoreClock / 2) / 1000000) - 1;

	TIM_Cmd(TIM13, DISABLE);

	// Time base configuration
	TIM_TimeBaseStructure.TIM_Period = 0xFFFF;
	TIM_TimeBaseStructure.TIM_Prescaler = PrescalerValue;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM13, &TIM_TimeBaseStructure);

	// TIM13 enable counter
	TIM_Cmd(TIM13, ENABLE);

	TIM13->CNT = 0;
	while(TIM13->CNT < us){};

	TIM_Cmd(TIM13, DISABLE);
}

static void send_ack() {
	unsigned char byte = PACKET_NORES_ACK;
	VCP_send_packet(&byte, 1);
}

static void send_ping() {
	unsigned char byte = CAR_PACKET_PING;
	VCP_send_packet(&byte, 1);
}

static void handle_res_packet(unsigned char *data, unsigned char len) {
	CAR_RES_PACKET_ID car_res_packet;
	uint8_t buffer2[100];
	unsigned int alpha;
	unsigned int index;
	uint32_t px, py;
	int32_t travel_cnt;
	int res, i;
	uint8_t buffer3[100];

	car_res_packet = data[0];
	data++;

	switch (car_res_packet) {
	case CAR_PACKET_READ_VALUES:
		// TODO
		break;

	case CAR_PACKET_READ_POS:
		/*
		 * Read dead reckoning position.
		 */
		px = (signed long)dr_x_pos;
		py = (signed long)dr_y_pos;
		alpha = (unsigned short)(dr_angle * 10000.0);
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

		VCP_send_packet(buffer2, index);
		break;

	case CAR_PACKET_READ_SENS_ULTRA:
		usart_clear_pending_bytes();
		SET_UART_BUS_ADDRESS(7);
		delay_us(100);
		USART_SendData(USART6, 1);
		res = usart_wait_for_bytes(16, buffer3, 10);
		SET_UART_BUS_ADDRESS(0);

		if (res == 0) {
			index = 0;
			buffer2[index++] = CAR_PACKET_READ_SENS_ULTRA;

			buffer2[index++] = buffer3[1];
			buffer2[index++] = buffer3[4];
			buffer2[index++] = buffer3[7];
			buffer2[index++] = buffer3[10];
			buffer2[index++] = buffer3[13];

			buffer2[index++] = buffer3[2];
			buffer2[index++] = buffer3[3];

			buffer2[index++] = buffer3[5];
			buffer2[index++] = buffer3[6];

			buffer2[index++] = buffer3[8];
			buffer2[index++] = buffer3[9];

			buffer2[index++] = buffer3[11];
			buffer2[index++] = buffer3[12];

			buffer2[index++] = buffer3[14];
			buffer2[index++] = buffer3[15];

			VCP_send_packet(buffer2, index);
		}
		break;

	case CAR_PACKET_READ_SENS_IR:
		usart_clear_pending_bytes();
		SET_UART_BUS_ADDRESS(7);
		delay_us(100);
		USART_SendData(USART6, 2);
		res = usart_wait_for_bytes(12, buffer3, 10);
		SET_UART_BUS_ADDRESS(0);

		if (res == 0) {
			index = 0;
			buffer2[index++] = CAR_PACKET_READ_SENS_IR;

			for(i = 0;i < 12;i++) {
				buffer2[index++] = buffer3[i];
			}

			VCP_send_packet(buffer2, index);
		}
		break;

	case CAR_PACKET_GET_TRAVEL_COUNTER:
		if (data[0]) {
			travel_cnt = (int32_t)dr_get_abs_total_travel_distance();
		} else {
			travel_cnt = (int32_t)dr_get_total_travel_distance();
		}

		index = 0;
		buffer2[index++] = CAR_PACKET_GET_TRAVEL_COUNTER;
		buffer2[index++] = travel_cnt >> 24;
		buffer2[index++] = travel_cnt >> 16;
		buffer2[index++] = travel_cnt >> 8;
		buffer2[index++] = travel_cnt;
		break;

	case CAR_PACKET_PING:
		send_ping();
		break;

	default:
		break;
	}
}

static void handle_nores_packet(unsigned char *data, unsigned char len) {
	CAR_NORES_PACKET_ID car_nores_packet;
	float tmp_f, px, py, alpha;
	int ind;
	RoutePoint point;

	car_nores_packet = data[0];
	data++;

	switch (car_nores_packet) {
	case CAR_PACKET_SET_POWER_SERVO:
		ap_clear();
		if (!override_power_steering) {
			tmp_f = (float) ((int16_t) ((int16_t) data[0] << 8
					| (int16_t) data[1])) / 100.0;
			drive_go(tmp_f, ((float) data[2] - 128.0) / 128.0);
		}
		break;

	case CAR_PACKET_WRITE_POS:
		ind = 0;
		px = ((signed long)data[ind] << 24) |
				((signed long)data[ind + 1] << 16) |
				((signed long)data[ind + 2] << 8) |
				((signed long)data[ind + 3]);
		ind += 4;

		py = ((signed long)data[ind] << 24) |
				((signed long)data[ind + 1] << 16) |
				((signed long)data[ind + 2] << 8) |
				((signed long)data[ind + 3]);
		ind += 4;

		alpha = ((unsigned int)data[ind] << 8) |
				((unsigned int)data[ind + 1]);
		ind += 2;

		dr_x_pos = (float)px;
		dr_y_pos = (float)py;
		dr_angle = ((float)alpha) / 10000.0;
		break;

	case CAR_PACKET_ADD_POINT:
		ind = 0;
		point.x = ((signed long) data[ind] << 24)
				| ((signed long) data[ind + 1] << 16)
				| ((signed long) data[ind + 2] << 8)
				| ((signed long) data[ind + 3]);
		ind += 4;

		point.y = ((signed long)data[ind] << 24) |
				((signed long)data[ind + 1] << 16) |
				((signed long)data[ind + 2] << 8) |
				((signed long)data[ind + 3]);
		ind += 4;

		point.speed = (float)((int16_t) ((int16_t) data[ind] << 8
				| (int16_t) data[ind + 1])) / 100.0;

		ind += 2;
		ap_add_point(&point);
		break;

	case CAR_PACKET_AP_RUN:
		ap_set_running(data[0]);
		break;

	case CAR_PACKET_AP_CLEAR:
		ap_clear();
		break;

	case CAR_PACKET_RESET_TRAVEL_CNT:
		dr_reset_travel_counters();
		break;

	case CAR_PACKET_SET_LIMITED:
		main_set_limited(data[0]);
		break;

	default:
		break;
	}

	send_ack();
}

void comm_run_tasks() {
	if (nores_packet_len) {
		handle_nores_packet((unsigned char*)nores_packet_buffer, nores_packet_len);
		nores_packet_len = 0;
	}

	if (res_packet_len) {
		handle_res_packet((unsigned char*)res_packet_buffer, res_packet_len);
		res_packet_len = 0;
	}
}
