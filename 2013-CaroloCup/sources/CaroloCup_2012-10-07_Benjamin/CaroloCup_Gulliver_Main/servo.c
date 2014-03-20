/*
	Copyright 2009-2010 Benjamin Vedder	vedder87@gmail.com
	
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
 * servo.c
 *
 *  Created on: 2009-apr-25
 *      Author: Benjamin Vedder
 *      Driver for controlling servos
 *      (Write-only c code...)
 */
#include "servo.h"

volatile SERVO servos[SERVOS_NUM];

#if USE_COMMANDS
volatile SERVO_CMD commands[SERVOS_NUM];
volatile unsigned int cmd_counter = 0, cmd_time_to_run = 0;
#endif

#if TEST_CYCLE_TIME
	volatile unsigned int restart_cnt = 0;
	volatile unsigned int interrupt_cnt = 0;
#endif

SERVO sorted_servos[SERVOS_NUM];

signed char int_index = 0, servo_int_index = 0, masks_ports_index = 0;
volatile unsigned char driver_active = 0;

volatile unsigned int delays[SERVOS_NUM + 1];
volatile unsigned char same_pos[SERVOS_NUM + 1];
volatile unsigned char same_bits[SERVOS_NUM + 1];
volatile unsigned char masks[SERVOS_NUM];
volatile unsigned char *ports[SERVOS_NUM];
volatile unsigned char length;

static size_t servo_struct_size = sizeof(SERVO);
static size_t sorted_servos_len = sizeof(sorted_servos) / sizeof(SERVO);

/*
 * Private functions
 */
void servo_get_copy(SERVO *a);
void servo_restart_pwm(void);
void servo_init_timer();
void servo_start_pulse(void);

/*
 * HW-specific START
 */
#define SERVO_CNT			TCD1_CNT
#define SERVO_CNT_TOP		TCD1_PER
#define SERVO_ISR			ISR (TCD1_OVF_vect)

void servo_init_timer() {
	// Timer overflow is a high level interrupt
	TCD1_INTCTRLA = 0x03;
	TCD1_CTRLA = TC_CLKSEL_DIV8_gc;
}

void servo_init(void) {
	servos[0].port = &PORTA_OUT;
	servos[0].dir = &PORTA_DIR;
	servos[0].pin = 6;
	servos[0].pos = 128;

	servos[1].port = &PORTC_OUT;
	servos[1].dir = &PORTC_DIR;
	servos[1].pin = 7;
	servos[1].pos = 128;

	same_pos[SERVOS_NUM] = 0;

	unsigned char i;
	for (i = 0; i < SERVOS_NUM; i++) {

#if USE_COMMANDS
		commands[i].active = 0;
		commands[i].last = 0;
#endif

		// Pin as output
		*servos[i].dir |= _BV(servos[i].pin);
	}

	servo_init_timer();
	servo_restart_pwm(); // Start servo control
	driver_active = 1;
}

void servo_start_pulse(void) {
	PORTA_OUTSET = 0b11000000;
}

/*
 * Stop servo driver
 */
void servo_stop_driver(void) {
	unsigned char i;

	TCD1_CTRLA = 0;
	TCD1_CTRLB = 0;
	TCD1_CTRLC = 0;
	TCD1_CTRLD = 0;
	TCD1_INTCTRLA = 0;
	TCD1_PER = 0;


	for (i = 0; i < SERVOS_NUM; i++) {
		// Pin as intput
		*servos[i].dir &= ~_BV(servos[i].pin);
		*servos[i].port &= ~_BV(servos[i].pin);
	}

	driver_active = 0;
}
/*
 * HW-specific END
 */

SERVO_ISR {
	if (int_index < 0) {
		// Start pulse
		servo_start_pulse();

		SERVO_CNT_TOP = SERVO_START_OFFSET + delays[0];

		int_index = 0;
		return;
	}

	if (int_index == length) {
		servo_restart_pwm();
		return;
	}

	// End pulse
	while (same_pos[int_index]) {

		*ports[masks_ports_index] &= ~masks[masks_ports_index];

		servo_int_index += same_bits[masks_ports_index];
		masks_ports_index++;
		same_pos[int_index]--;
	}

	int_index++;

	// If the positions are close handle more servos in this interrupt.
	if (servo_int_index < SERVOS_NUM && delays[int_index] < 50) {
		do {
			SERVO_CNT = 0;
			while (SERVO_CNT < delays[int_index]);

			while (same_pos[int_index]) {

				*ports[masks_ports_index] &= ~masks[masks_ports_index];

				servo_int_index += same_bits[masks_ports_index];
				masks_ports_index++;
				same_pos[int_index]--;
			}

			int_index++;

		} while (servo_int_index < SERVOS_NUM && delays[int_index] < 60);
	}

	SERVO_CNT_TOP = delays[int_index];

#if TEST_CYCLE_TIME
	if (int_index < length && int_index > 0) {
		interrupt_cnt = SERVO_CNT;
	}
#endif

}

int servo_cmp_by_pos(const void *a, const void *b) {

	SERVO *ia = (SERVO *) a;
	SERVO *ib = (SERVO *) b;

	if (ia->pos == ib->pos)
		return 0;
	else if (ia->pos < ib->pos)
		return -1;
	else
		return 1;
}

void servo_restart_pwm(void) {

#if TEST_CYCLE_TIME
	unsigned int pwm_start = SERVO_CNT;
#endif

	//	CNT = 0;
	int_index = -1;
	servo_int_index = 0;
	masks_ports_index = 0;

	// Start Cooldown
	SERVO_CNT_TOP = SERVO_COOLDOWN_FACTOR;

	servo_get_copy(sorted_servos);
	qsort(sorted_servos, sorted_servos_len, servo_struct_size, servo_cmp_by_pos);

	delays[0] = sorted_servos[0].pos * SERVO_CPU_FACTOR;
	masks[0] = _BV(sorted_servos[0].pin);
	ports[0] = sorted_servos[0].port;

	unsigned char i = 0, j = 1, k = 0;

	for (i = 0; i <= SERVOS_NUM; i++) {
		same_pos[i] = 1;
		same_bits[i] = 1;
	}

	i = 0;

	while (1) {
		while (sorted_servos[j].pos == sorted_servos[j - 1].pos && j < SERVOS_NUM) {

			if (sorted_servos[j].port == sorted_servos[j - 1].port) {
				masks[k] |= _BV(sorted_servos[j].pin);
				same_bits[k]++;
			} else {
				same_pos[i]++;
				ports[++k] = sorted_servos[j].port;
				masks[k] = _BV(sorted_servos[j].pin);
			}

			j++;
		}

		i++;

		if (j < SERVOS_NUM) {
			delays[i] = (sorted_servos[j].pos - sorted_servos[j - 1].pos) * SERVO_CPU_FACTOR;
			ports[++k] = sorted_servos[j].port;
			masks[k] = _BV(sorted_servos[j].pin);
			j++;
		} else {
			break;
		}
	}

	/*                                                                            |
	 * Add a few extra cycles here to make sure the interrupt is able to finish. \|/
	 */
	delays[i] = (sorted_servos[SERVOS_NUM - 1].pos) * SERVO_CPU_FACTOR 		+ 	 120;

	length = i;

#if USE_COMMANDS
	/*
	 * Run the commands for the servos here.
	 */
	for (i = 0;i < SERVOS_NUM;i++) {
		if (commands[i].active) {
			unsigned char p = commands[i].pos, ps = servos[i].pos;

			if (p == ps) {
				commands[i].active = 0;
				commands[i].last = 0;
				continue;
			}

			commands[i].last += commands[i].speed;
			unsigned char delta = commands[i].last >> 5;

			if (p < ps) {
				servos[i].pos -= delta;
				if (delta > (ps - p)) {
					servos[i].pos = p;
				}
			} else {
				servos[i].pos += delta;
				if (delta > (p - ps)) {
					servos[i].pos = p;
				}
			}

			if (delta > 0) {
				commands[i].last = 0;
			}
		}
	}

	cmd_counter++;

	if (cmd_counter == cmd_time_to_run && cmd_seq_running) {
		unsigned char tmp1 = 0, tmp2 = 0, tmp3 = 0;

		while (cmd_seq_running) {
			switch (pgm_read_byte(&cmd_seq[cmd_ptr++])) {
			case CMD_MOVE_SERVO:
				tmp1 = pgm_read_byte(&cmd_seq[cmd_ptr++]);
				tmp2 = pgm_read_byte(&cmd_seq[cmd_ptr++]);
				tmp3 = pgm_read_byte(&cmd_seq[cmd_ptr++]);

				if (tmp3 == 0) {
					commands[tmp1].active = 0;
					servos[tmp1].pos = tmp2;
				} else {
					commands[tmp1].speed = tmp3;
					commands[tmp1].pos = tmp2;
					commands[tmp1].active = 1;
				}
				break;

			case CMD_WAIT:
				cmd_counter = 0;
				cmd_time_to_run = pgm_read_byte(&cmd_seq[cmd_ptr++]) * CMD_WAIT_FACTOR;
				goto end_cmds;
				break;

			case CMD_WAIT_SERVO:
				tmp1 = pgm_read_byte(&cmd_seq[cmd_ptr++]);
				if (commands[tmp1].active) {
					cmd_ptr -= 2;
					cmd_counter = 0;
					cmd_time_to_run = 1;
					goto end_cmds;
				}
				break;

			case CMD_WAIT_ALL_SERVOS:
				for (tmp1 = 0; tmp1 < SERVOS_NUM; tmp1++) {
					if (commands[tmp1].active) {
						cmd_ptr--;
						cmd_counter = 0;
						cmd_time_to_run = 1;
						goto end_cmds;
					}
				}
				break;

			case CMD_STOP_DRIVER:
				servo_stop_cmds();
				servo_stop_driver();
				goto end_cmds;
				break;

			case CMD_STOP_CMDS:
				servo_stop_cmds();
				goto end_cmds;
				break;

			case CMD_RESTART:
				cmd_ptr = 0;
				break;

			default:
				servo_stop_cmds();
				goto end_cmds;
				break;
			}

		}

		end_cmds: ;
	}

#endif

#if TEST_CYCLE_TIME
	restart_cnt = SERVO_CNT - pwm_start;
#endif

}

#if USE_COMMANDS
volatile signed char cmd_seq_running = 0;
volatile unsigned int cmd_ptr = 0;
volatile PGM_P cmd_seq;

void servo_move(unsigned char servo, unsigned char position, unsigned char speed) {
//	if (commands[servo].active) {
//		return;
//	}

	if (speed == 0) {
		servos[servo].pos = position;
		commands[servo].active = 0;
		return;
	}

	commands[servo].speed = speed;
	commands[servo].pos = position;
	commands[servo].active = 1;
}

void servo_run_cmds(PGM_P cmds) {
	cmd_ptr = 0;
	cmd_seq = cmds;
	cmd_seq_running = 1;
	cmd_counter = 0;
	cmd_time_to_run = 1;
}

void servo_stop_cmds(void) {
	cmd_seq_running = 0;
}
#endif

unsigned char servo_driver_is_active() {
	return driver_active;
}

void servo_get_copy(SERVO *a) {
	int i;

	for(i = 0;i < SERVOS_NUM;i++) {
		a[i].port = servos[i].port;
		a[i].dir = servos[i].dir;
		a[i].pin = servos[i].pin;
		a[i].pos = servos[i].pos;
	}
}
