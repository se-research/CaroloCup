/*
 * mc_driver.c
 *
 *  Created on: 13 dec 2011
 *      Author: benjamin
 */

#include <avr/io.h>
#include <avr/interrupt.h>

#include "mc_driver.h"
#include "uart_bldc.h"
#include "main.h"

volatile unsigned char current_servo_pos = 0;
volatile unsigned char mc_busy = 0;
volatile unsigned char mc_override = 0;
volatile double mc_last_speed = 0;

void mc_set_motor_power(unsigned int power_in, unsigned char direction, unsigned char isPid) {
	if (mc_busy || mc_override) {
		return;
	}

	unsigned char buffer[4];

	if (isPid) {
		buffer[0] = 'P';
	} else {
		buffer[0] = 'O';
		if (power_in > MC_PWM_MAX) {
			power_in = MC_PWM_MAX;
		}
	}

	buffer[1] = power_in >> 8;
	buffer[2] = power_in;
	buffer[3] = direction;

	mc_busy = 1;
	uart_bldc_putc('s');
	uart_bldc_put_buffer(buffer, 4);
	mc_busy = 0;

	set_mc_timeout(1000);
}

void mc_set_override(unsigned char override) {
	mc_override = override;
}

/*
 * Set speed in meters/second and store last set speed
 */
void mc_set_speed(double speed) {
	mc_last_speed = speed;
	const double rpm = (speed * 60.0) / (MC_METER_TACHO_PULSE * 6);

	if (rpm > 0) {
		mc_set_motor_power((unsigned int)rpm, 0, 1);
	} else {
		mc_set_motor_power((unsigned int)(-rpm), 1, 1);
	}
}

/*
 * Set speed in meters/second and don't store last set speed
 */
void mc_set_speed_no_store(double speed) {
	const double rpm = (speed * 60.0) / (MC_METER_TACHO_PULSE * 6);

	if (rpm > 0) {
		mc_set_motor_power((unsigned int)rpm, 0, 1);
	} else {
		mc_set_motor_power((unsigned int)(-rpm), 1, 1);
	}
}

double mc_get_last_speed() {
	return mc_last_speed;
}

void mc_set_servo_position(unsigned char pos, unsigned char speed) {
	if (mc_busy) {
		return;
	}

	unsigned char buffer[4];

	if (pos > MC_SERVO_LEFT) {
		pos = MC_SERVO_LEFT;
	}

	if (pos < MC_SERVO_RIGHT) {
		pos = MC_SERVO_RIGHT;
	}

	current_servo_pos = pos;

	pos += MC_SERVO_OFFSET;

	buffer[0] = 'S';
	buffer[1] = 0;
	buffer[2] = pos;
	buffer[3] = speed;

	mc_busy = 1;
	uart_bldc_putc('s');
	uart_bldc_put_buffer(buffer, 4);
	mc_busy = 0;
}

unsigned char mc_get_current_servo_pos() {
	return current_servo_pos;
}

unsigned char mc_is_busy() {
	return mc_busy;
}

unsigned char mc_get_values(unsigned char* buffer) {
	unsigned char res = 0;

	if (mc_busy) {
		return res;
	}

	mc_busy = 1;
	uart_bldc_flush_rx();
	uart_bldc_putc('v');
	res = uart_bldc_wait_for_bytes(8, 100, buffer);
	mc_busy = 0;

	return res;
}

unsigned char mc_get_values_struct(MC_VALUES *values) {
	unsigned char res = 0;
	unsigned char buffer[8];
	unsigned int mv1, mv2, mdeg, rpm;

	if (mc_get_values(buffer)) {
		mv1 = (unsigned int)buffer[0] << 8 | (unsigned int)buffer[1];
		values->v_batt = (double)mv1 / 1000.0;

		mv2 = (unsigned int)buffer[2] << 8 | (unsigned int)buffer[3];
		values->v_log = (double)mv2 / 1000.0;

		mdeg = (unsigned int)buffer[4] << 8 | (unsigned int)buffer[5];
		values->temp = (double)mdeg / 100.0;

		rpm = (unsigned int)buffer[6] << 8 | (unsigned int)buffer[7];
		values->speed = ((double)rpm * (MC_METER_TACHO_PULSE * 6.0) / 60.0);

		res = 1;
	}

	return res;
}

unsigned char mc_get_rpm(double *rpm) {
	unsigned char res = 0;
	unsigned char buffer[2];

	if (mc_busy) {
		return res;
	}

	mc_busy = 1;
	uart_bldc_flush_rx();
	uart_bldc_putc('r');
	res = uart_bldc_wait_for_bytes(2, 100, buffer);
	mc_busy = 0;

	if (res) {
		signed int tmp = (signed int)buffer[0] << 8 | (signed int)buffer[1];
		*rpm = ((double)tmp) * 10.0;
	}

	return res;
}

unsigned char mc_get_speed(double *speed) {
	double rpm;
	unsigned char res = 0;

	res = mc_get_rpm(&rpm);

	if (res) {
		*speed = rpm * (MC_METER_TACHO_PULSE * 6.0) / 60.0;
	}

	return res;
}

signed long mc_get_tacho(unsigned char reset_tacho) {
	unsigned char buffer[4];
	signed long tacho = 0;
	unsigned char res;

	if (mc_busy) {
		return tacho;
	}

	mc_busy = 1;
	uart_bldc_flush_rx();
	if (reset_tacho) {
		uart_bldc_putc('T');
	} else {
		uart_bldc_putc('t');
	}

	res = uart_bldc_wait_for_bytes(4, 100, buffer);
	mc_busy = 0;

	if (res) {
		tacho = (signed long)buffer[0] << 24 |
				(signed long)buffer[1] << 16 |
				(signed long)buffer[2] << 8 |
				(signed long)buffer[3];
	}

	return tacho;
}
