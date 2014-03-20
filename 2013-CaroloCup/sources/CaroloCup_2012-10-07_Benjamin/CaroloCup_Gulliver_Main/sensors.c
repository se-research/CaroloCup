/*
 * sensors.c
 *
 *  Created on: 13 jan 2012
 *      Author: benjamin
 */

#include "sensors.h"
#include "uart_bus.h"

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <util/delay.h>
#include <stdio.h>
#include <math.h>

// TODO!!! FIX
unsigned char sensors_read_ultra(SENSOR_ULTRA *sensors) {
	unsigned char buffer[40];
	uart_bus_flush_rx();
	uart_bus_set_address(UART_BUS_SENSORS_ADDR);
	_delay_ms(0.1);
	uart_bus_putc(1);
	unsigned char res = uart_bus_wait_for_bytes(7, 20, buffer);

	if (buffer[0] > DISTANCE_SENSORS_NUM) {
		return 0;
	}

	if (res) {
		for (unsigned char i = 0;i < 2;i++) {
			sensors[i].address = buffer[3 * i + 1];
			sensors[i].value = (unsigned int)buffer[3 * i + 2] << 8 | (unsigned int)buffer[3 * i + 3];
		}

		return 1;
	}

	return 0;
}

