#include <stdlib.h>

#include "ch.h"
#include "hal.h"
#include "chprintf.h"

#include "devices_lib/accel/lis302dl.h"

#include "SPI/SPI.h"

/*
 * SPI1 configuration structure.
 * Speed 5.25MHz, CPHA=1, CPOL=1, 8bits frames, MSb transmitted first.
 * The slave select line is the pin GPIOE_CS_SPI on the port GPIOE.
 */
static const SPIConfig spi1cfg = {
  NULL,
  /* HW dependent part.*/
  GPIOE,
  GPIOE_CS_SPI,
  SPI_CR1_BR_0 | SPI_CR1_BR_1 | SPI_CR1_CPOL | SPI_CR1_CPHA
};

/*
 * Read accelerometer data.
 */
void readAccel(void) {
    // Keeping an history of the latest four accelerometer readings.
    unsigned i;
    for (i = 3; i > 0; i--) {
      accel_x[i] = accel_x[i - 1];
      accel_y[i] = accel_y[i - 1];
      accel_z[i] = accel_z[i - 1];
    }

    // Reading MEMS accelerometer X, Y, and Z registers.
    accel_x[0] = (int8_t)lis302dlReadRegister(&SPID1, LIS302DL_OUTX);
    accel_y[0] = (int8_t)lis302dlReadRegister(&SPID1, LIS302DL_OUTY);
    accel_z[0] = (int8_t)lis302dlReadRegister(&SPID1, LIS302DL_OUTZ);

    // Calculating average of the latest four accelerometer readings.
    chMtxLock(&accelMtx);
    accel_x[4] = ((int32_t)accel_x[0] + (int32_t)accel_x[1] +
         (int32_t)accel_x[2] + (int32_t)accel_x[3]) / 4;
    accel_y[4] = ((int32_t)accel_y[0] + (int32_t)accel_y[1] +
         (int32_t)accel_y[2] + (int32_t)accel_y[3]) / 4;
    accel_z[4] = ((int32_t)accel_z[0] + (int32_t)accel_z[1] +
         (int32_t)accel_z[2] + (int32_t)accel_z[3]) / 4;
    chMtxUnlock();
}

/*
 * Print last read accelerometer data.
 */
void cmd_printAccel(BaseSequentialStream *chp, int argc, char *argv[]) {
  (void)chp;
  (void)argc;
  (void)argv;

  chMtxLock(&accelMtx);
  chprintf(chp, "x=%d, y=%d, z=%d ", accel_x[4], accel_y[4], accel_z[4]);
  chMtxUnlock();

  chprintf(chp, "\r\n");
}

/*
 * This is a periodic thread that reads accelerometer.
 */
static WORKING_AREA(waThread1, 128);
static msg_t Thread1(void *arg) {
  (void)arg;

  // Next deadline.
  systime_t time;

  chRegSetThreadName("accelreader");

  // Reader thread loop.
  time = chTimeNow();
  while (TRUE) {
    readAccel();

    chMtxLock(&accelMtx);
    // Reprogramming the four PWM channels using the accelerometer data.
    if (accel_y[4] < 0) {
      pwmEnableChannel(&PWMD4, 0, (pwmcnt_t)-accel_y[4]);
      pwmEnableChannel(&PWMD4, 2, (pwmcnt_t)0);
    }
    else {
      pwmEnableChannel(&PWMD4, 2, (pwmcnt_t)accel_y[4]);
      pwmEnableChannel(&PWMD4, 0, (pwmcnt_t)0);
    }
    if (accel_x[4] < 0) {
      pwmEnableChannel(&PWMD4, 1, (pwmcnt_t)-accel_x[4]);
      pwmEnableChannel(&PWMD4, 3, (pwmcnt_t)0);
    }
    else {
      pwmEnableChannel(&PWMD4, 3, (pwmcnt_t)accel_x[4]);
      pwmEnableChannel(&PWMD4, 1, (pwmcnt_t)0);
    }
    chMtxUnlock();

    // Waiting until the next 250 milliseconds time interval.
    chThdSleepUntil(time += MS2ST(100));
  }

  return (msg_t)0;
}

void mySPIinit(void){
  chMtxInit(&accelMtx);

  // Initializes the SPI driver 1 in order to access the MEMS. The signals are already initialized in the board file.
  spiStart(&SPID1, &spi1cfg);

  // LIS302DL initialization.
  lis302dlWriteRegister(&SPID1, LIS302DL_CTRL_REG1, 0x43);
  lis302dlWriteRegister(&SPID1, LIS302DL_CTRL_REG2, 0x00);
  lis302dlWriteRegister(&SPID1, LIS302DL_CTRL_REG3, 0x00);

  // Start accelerator reading thread.
  chThdCreateStatic(waThread1, sizeof(waThread1),
                    NORMALPRIO + 10, Thread1, NULL);
}
