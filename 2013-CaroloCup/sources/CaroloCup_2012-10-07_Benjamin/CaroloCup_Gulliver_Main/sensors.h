/*
 * sensors.h
 *
 *  Created on: 13 jan 2012
 *      Author: benjamin
 */

#ifndef SENSORS_H_
#define SENSORS_H_

/*
 * UtrasoudSensor
 */
typedef struct {
	unsigned char address;
	unsigned int value;
} SENSOR_ULTRA;

/*
 * Functions
 */
unsigned char sensors_read_ultra(SENSOR_ULTRA *sensors);

#endif /* SENSORS_H_ */
