/*
 * nine_dof.h
 *
 *  Created on: 4 jan 2012
 *      Author: benjamin
 */

#ifndef NINE_DOF_H_
#define NINE_DOF_H_

/*
 * Functions
 */
unsigned char nine_dof_read_compass(unsigned int *angle);
unsigned char nine_dof_read_compass_xyz(signed int *x, signed int *y, signed int *z);
void nine_dof_compass_calibrate();

#endif /* NINE_DOF_H_ */
