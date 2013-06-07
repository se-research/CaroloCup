#ifndef ULTRASONIC_H_INCLUDED
#define ULTRASONIC_H_INCLUDED

void cmd_printDataFromUltrasonic(BaseSequentialStream *chp, int argc, char *argv[]);
void myUltrasonicInit(void);
int16_t getRange(int SRF08_num);
void getUS(int16_t *usData);
//int16_t getLightInstensity(int SRF_num);

#endif // ULTRASONIC_H_INCLUDED
