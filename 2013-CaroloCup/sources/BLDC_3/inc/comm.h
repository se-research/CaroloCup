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
 * comm.h
 *
 *  Created on: 22 nov 2012
 *      Author: benjamin
 */

#ifndef COMM_H_
#define COMM_H_

// Packets that expect response
typedef enum {
	CAR_PACKET_READ_VALUES = 0,
	CAR_PACKET_READ_POS,
	CAR_PACKET_READ_SENS_ULTRA,
	CAR_PACKET_READ_SENS_IR,
	CAR_PACKET_GET_TRAVEL_COUNTER,
	CAR_PACKET_PING
} CAR_RES_PACKET_ID;

// Packets that only expect ack
typedef enum {
	CAR_PACKET_SET_POWER_SERVO = 0,
	CAR_PACKET_WRITE_POS,
	CAR_PACKET_ADD_POINT,
	CAR_PACKET_AP_RUN,
	CAR_PACKET_AP_CLEAR,
	CAR_PACKET_RESET_TRAVEL_CNT,
	CAR_PACKET_SET_LIMITED
} CAR_NORES_PACKET_ID;

#define PACKET_NORES_ACK	254
#define PACKET_OTHER		255

// Functions
void comm_handle_nores_packet(unsigned char *data, unsigned char len);
void comm_handle_res_packet(unsigned char *data, unsigned char len);
void comm_override_power_steering(int is_override);
void comm_run_tasks();

#endif /* COMM_INTERFACE_H_ */
