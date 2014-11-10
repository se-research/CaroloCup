/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

#include "devices_lib/accel/lis302dl.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuation.
///////////////////////////////////////////////////////////////////////////////

static Thread *ThreadOnboardAccelerometer = NULL;

// Four samples are read from the sensor.
#define MAX_ACCELEROMETER_SAMPLES 4

// x,y,z contain MAX_ACCELEROMETER_SAMPLES samples
// and in [MAX_ACCELEROMETER_SAMPLES+1] the average of the last four samples.
static int8_t accel_x[MAX_ACCELEROMETER_SAMPLES+1];
static int8_t accel_y[MAX_ACCELEROMETER_SAMPLES+1];
static int8_t accel_z[MAX_ACCELEROMETER_SAMPLES+1];

// Index variable to continuously store the values.
static unsigned int indexAccelerometer = 0;

// Configuration for SPI which is used to read data from the MEMS accelerometer sensor.
// Speed 5.25MHz, CPHA=1, CPOL=1, 8bits frames, MSb transmitted first.
// The slave select line is the pin GPIOE_CS_SPI on the port GPIOE.
static const SPIConfig spi1cfg = {
    NULL,
    GPIOE,
    GPIOE_CS_SPI,
    SPI_CR1_BR_0 | SPI_CR1_BR_1 | SPI_CR1_CPOL | SPI_CR1_CPHA
};

///////////////////////////////////////////////////////////////////////////////
// Interface methods.
///////////////////////////////////////////////////////////////////////////////

Thread * getThreadOnboardAccelerometer(void) {
    return ThreadOnboardAccelerometer;
}

void getOnboardAccelerometerData(OnboardAccelerometerDataT *data) {
    if (data != NULL) {
        data->x = accel_x[MAX_ACCELEROMETER_SAMPLES];
        data->y = accel_y[MAX_ACCELEROMETER_SAMPLES];
        data->z = accel_z[MAX_ACCELEROMETER_SAMPLES];
    }
}

void commandPrintAccelerometer(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    chprintf(chp, "Onboard accelerometer: x = %d, y = %d, z = %d ", accel_x[MAX_ACCELEROMETER_SAMPLES],
                                                                    accel_y[MAX_ACCELEROMETER_SAMPLES],
                                                                    accel_z[MAX_ACCELEROMETER_SAMPLES]);
    chprintf(chp, "\r\n");
}

///////////////////////////////////////////////////////////////////////////////
// Sensor reading methods.
///////////////////////////////////////////////////////////////////////////////

void readOnboardAccelerometer(void) {
    // Reading MEMS accelerometer X, Y, and Z registers.
    accel_x[indexAccelerometer] = (int8_t)lis302dlReadRegister(&SPID1, LIS302DL_OUTX);
    accel_y[indexAccelerometer] = (int8_t)lis302dlReadRegister(&SPID1, LIS302DL_OUTY);
    accel_z[indexAccelerometer] = (int8_t)lis302dlReadRegister(&SPID1, LIS302DL_OUTZ);

    // Increment the index counter and apply modulo to restrict its value range.
    indexAccelerometer = (indexAccelerometer+1) % MAX_ACCELEROMETER_SAMPLES;

    // Calculating average of the latest four accelerometer readings.
    accel_x[MAX_ACCELEROMETER_SAMPLES] = ((int32_t)accel_x[0] + (int32_t)accel_x[1] +
         (int32_t)accel_x[2] + (int32_t)accel_x[3]) / MAX_ACCELEROMETER_SAMPLES;
    accel_y[MAX_ACCELEROMETER_SAMPLES] = ((int32_t)accel_y[0] + (int32_t)accel_y[1] +
         (int32_t)accel_y[2] + (int32_t)accel_y[3]) / MAX_ACCELEROMETER_SAMPLES;
    accel_z[MAX_ACCELEROMETER_SAMPLES] = ((int32_t)accel_z[0] + (int32_t)accel_z[1] +
         (int32_t)accel_z[2] + (int32_t)accel_z[3]) / MAX_ACCELEROMETER_SAMPLES;
}

static WORKING_AREA(workingAreaThread_OnboardAccelerometer, 512);
static msg_t Thread_OnboardAccelerometer(void *arg) {
    (void)arg;
    chRegSetThreadName("OnboardAccelerometer");

    waitForCompletingInitialization();

    while (TRUE) {
        readOnboardAccelerometer();
        chThdSleepMilliseconds(10);
    }

    return (msg_t)0;
}

///////////////////////////////////////////////////////////////////////////////
// Initialization method.
///////////////////////////////////////////////////////////////////////////////

void initializeOnboardAccelerometer(void) {
    // Initializes the SPI driver 1 in order to access the MEMS. The signals are already initialized in the board file.
    spiStart(&SPID1, &spi1cfg);

    // LIS302DL initialization.
    lis302dlWriteRegister(&SPID1, LIS302DL_CTRL_REG1, 0x43);
    lis302dlWriteRegister(&SPID1, LIS302DL_CTRL_REG2, 0x00);
    lis302dlWriteRegister(&SPID1, LIS302DL_CTRL_REG3, 0x00);

    // Start accelerator reading thread.
    ThreadOnboardAccelerometer = chThdCreateStatic(workingAreaThread_OnboardAccelerometer,
                                                    sizeof(workingAreaThread_OnboardAccelerometer),
                                                    NORMALPRIO + 10, Thread_OnboardAccelerometer, NULL);
}

