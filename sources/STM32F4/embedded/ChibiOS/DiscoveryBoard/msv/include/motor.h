#ifndef MOTOR_H_INCLUDED
#define MOTOR_H_INCLUDED

void motorInit(void);
void setMotorData(int steer, int speed);
int convertMotorData(int speed);
int convertSteeringData(int steer);

#endif // MOTORINCLUDED
