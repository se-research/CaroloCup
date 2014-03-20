/*
 * nine_dof.c
 *
 *  Created on: 4 jan 2012
 *      Author: benjamin
 */

#include "nine_dof.h"
#include "uart_bus.h"
#include "uart_ft.h"

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <util/delay.h>
#include <stdio.h>
#include <math.h>

unsigned char nine_dof_read_compass(unsigned int *angle) {
	unsigned char buffer[2];
	uart_bus_flush_rx();
	uart_bus_set_address(UART_BUS_NINEDOF_ADDR);
	_delay_ms(0.1);
	uart_bus_putc(2);

	unsigned char res = uart_bus_wait_for_bytes(2, 20, buffer);

	if (res) {
		*angle = buffer[0] << 8 | buffer[1];
		return 1;
	}

	return 0;
}

unsigned char nine_dof_read_compass_xyz(signed int *x, signed int *y, signed int *z) {
	unsigned char buffer[6];
	uart_bus_set_address(UART_BUS_NINEDOF_ADDR);
	_delay_ms(0.1);
	uart_bus_putc(8);

//	cli();
	unsigned char res = uart_bus_wait_for_bytes(6, 20, buffer);
//	sei();

	if (res) {
		*x = buffer[0] << 8 | buffer[1];
		*y = buffer[2] << 8 | buffer[3];
		*z = buffer[4] << 8 | buffer[5];
		return 1;
	}

	return 0;
}

void nine_dof_compass_calibrate() {
	char buffer[60];
	char print = 0;
	static unsigned char no_print = 0;

	static signed int 	x = 0,
						y = 0,
						z = 0,
						x_max = -500,
						y_max = -500,
						z_max = -500,
						x_min = 500,
						y_min = 500,
						z_min = 500;

	if (nine_dof_read_compass_xyz(&x, &y, &z)) {
		if (x > x_max) {
			x_max = x;
			print = 1;
		} else if (x < x_min) {
			x_min = x;
			print = 1;
		}

		if (y > y_max) {
			y_max = y;
			print = 1;
		} else if (y < y_min) {
			y_min = y;
			print = 1;
		}

		if (z > z_max) {
			z_max = z;
			print = 1;
		} else if (z < z_min) {
			z_min = z;
			print = 1;
		}

		if (!print) {
			no_print++;
		}

		if (print || no_print >= 200) {
			no_print = 0;
			sprintf_P(buffer, PSTR(	"X:%i\n"
					"Y:%i\n"
					"Z:%i\n"), x, y, z);
			uart_ft_print_string(buffer);

			sprintf_P(buffer, PSTR(	"X[%i:%i]\n"
					"Y[%i:%i]\n"
					"Z[%i:%i]\n"), x_min, x_max, y_min, y_max, z_min, z_max);
			uart_ft_print_string(buffer);
		}
	} else {

	}
}

