#ifndef SPI_H_INCLUDED
#define SPI_H_INCLUDED

// x,y,z contain 4 samples and in [4] the average of the last four samples.
static int8_t accel_x[5], accel_y[5], accel_z[5];
static Mutex accelMtx;

void cmd_printAccel(BaseSequentialStream *chp, int argc, char *argv[]);
void cmd_printAccelContinuously(BaseSequentialStream *chp, int argc, char *argv[]);

void readAccel(void);

void mySPIinit(void);


#endif // SPI_H_INCLUDED
