/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuration.
///////////////////////////////////////////////////////////////////////////////

static Thread *ThreadSpeed = NULL;

#define INCREMENT_LEFT_WHEEL   0.0212
#define INCREMENT_RIGHT_WHEEL  0.0212

#define min(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a < _b ? _a : _b; })

static float dLeft = 0;
static float dRight = 0;

static float vLeft[4];
static float vRight[4];

static float signLeft = 1;
static float signRight = 1;

static float sign = 1;

icucnt_t last_width_LeftWheelFirstSensor, last_period_LeftWheelFirstSensor;
icucnt_t last_width_LeftWheelSecondSensor, last_period_LeftWheelSecondSensor;
icucnt_t last_width_RightWheelFirstSensor, last_period_RightWheelFirstSensor;
icucnt_t last_width_RightWheelSecondSensor, last_period_RightWheelSecondSensor;

systime_t timeStampLeftWheelFirstSensor, timeStampLeftWheelSecondSensor;
systime_t timeStampRightWheelFirstSensor, timeStampRightWheelSecondSensor;

static void icuwidth_LeftWheelFirstSensor(ICUDriver *icup) {
    last_width_LeftWheelFirstSensor = icuGetWidth(icup);

    timeStampLeftWheelFirstSensor = chTimeNow();
}

static void icuperiod_LeftWheelFirstSensor(ICUDriver *icup) {
    last_period_LeftWheelFirstSensor = icuGetPeriod(icup);
    
    if (timeStampLeftWheelFirstSensor < timeStampLeftWheelSecondSensor) {
        signLeft = 1;
    }
    else {
        signLeft = -1;
    }

    sign = min(signLeft, signRight);

    dLeft += (sign * INCREMENT_LEFT_WHEEL);
}

static void icuwidth_LeftWheelSecondSensor(ICUDriver *icup) {
    last_width_LeftWheelSecondSensor = icuGetWidth(icup);

    timeStampLeftWheelSecondSensor = chTimeNow();
}

static void icuperiod_LeftWheelSecondSensor(ICUDriver *icup) {
    last_period_LeftWheelSecondSensor = icuGetPeriod(icup);
}

static void icuwidth_RightWheelFirstSensor(ICUDriver *icup) {
    last_width_RightWheelFirstSensor = icuGetWidth(icup);

    timeStampRightWheelFirstSensor = chTimeNow();
}

static void icuperiod_RightWheelFirstSensor(ICUDriver *icup) {
    last_period_RightWheelFirstSensor = icuGetPeriod(icup);
    
    if (timeStampRightWheelFirstSensor > timeStampRightWheelSecondSensor) {
        signRight = 1;
    }
    else {
        signRight = -1;
    }

    sign = min(signLeft, signRight);

    dRight += (sign * INCREMENT_RIGHT_WHEEL);
}

static void icuwidth_RightWheelSecondSensor(ICUDriver *icup) {
    last_width_RightWheelSecondSensor = icuGetWidth(icup);

    timeStampRightWheelSecondSensor = chTimeNow();
}

static void icuperiod_RightWheelSecondSensor(ICUDriver *icup) {
    last_period_RightWheelSecondSensor = icuGetPeriod(icup);
}

static ICUConfig icuLeftWheelFirstSensor = {
    ICU_INPUT_ACTIVE_HIGH,
    10000, // 10kHz ICU clock frequency.
    icuwidth_LeftWheelFirstSensor,
    icuperiod_LeftWheelFirstSensor,
    NULL,
    ICU_CHANNEL_2
};

static ICUConfig icuLeftWheelSecondSensor = {
    ICU_INPUT_ACTIVE_HIGH,
    10000, // 10kHz ICU clock frequency.
    icuwidth_LeftWheelSecondSensor,
    icuperiod_LeftWheelSecondSensor,
    NULL,
    ICU_CHANNEL_1
};

static ICUConfig icuRightWheelFirstSensor = {
    ICU_INPUT_ACTIVE_HIGH,
    10000, // 10kHz ICU clock frequency.
    icuwidth_RightWheelFirstSensor,
    icuperiod_RightWheelFirstSensor,
    NULL,
    ICU_CHANNEL_2
};

static ICUConfig icuRightWheelSecondSensor = {
    ICU_INPUT_ACTIVE_HIGH,
    10000, // 10kHz ICU clock frequency.
    icuwidth_RightWheelSecondSensor,
    icuperiod_RightWheelSecondSensor,
    NULL,
    ICU_CHANNEL_1
};

///////////////////////////////////////////////////////////////////////////////
// Interface methods.
///////////////////////////////////////////////////////////////////////////////

Thread * getThreadSpeed(void) {
    return ThreadSpeed;
}

void getWheelEncoderData(WheelEncoderDataT *data) {
    if (data != NULL) {
        data->drivenDistanceLeftWheel = (int) (dLeft * 100.0);
        data->drivenDistanceRightWheel = (int) (dRight * 100.0);
        data->speedLeftWheel = (int) ((vLeft[0] + vLeft[1] + vLeft[2] + vLeft[3])*100.0/4.0);
        data->speedLeftWheel = (int) ((vRight[0] + vRight[1] + vRight[2] + vRight[3])*100.0/4.0);
    }
}

void commandResetWheelEncoder(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)chp;
    (void)argc;
    (void)argv;

    dLeft = 0;
    dRight = 0;
}

void commandPrintWheelEncoder(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    chprintf(chp, "WheelEncoder: w0 = %d, p0 = %d, w1 = %d, p1 = %d, w2 = %d, p2 = %d, w3 = %d, p3 = %d, dLeft = %f, dRight = %f,",
            last_width_LeftWheelFirstSensor, last_period_LeftWheelFirstSensor,
            last_width_LeftWheelSecondSensor, last_period_LeftWheelSecondSensor,
            last_width_RightWheelFirstSensor, last_period_RightWheelFirstSensor,
            last_width_RightWheelSecondSensor, last_period_RightWheelSecondSensor,
            dLeft, dRight);
    chprintf(chp, "\r\n");
}

///////////////////////////////////////////////////////////////////////////////
// Sensor reading methods.
///////////////////////////////////////////////////////////////////////////////

// The WheelEncoder is read in background by ChibiOS.

// The speed is calculated based on the driven path over time.
static WORKING_AREA(workingAreaThread_Speed, 512);
static msg_t Thread_Speed(void *arg) {
    (void)arg;
    chRegSetThreadName("Speed");

    waitForCompletingInitialization();

    const int FREQUENCY = 50;

    vLeft[0] = vLeft[1] = vLeft[2] = vLeft[3] = 0;
    vRight[0] = vRight[1] = vRight[2] = vRight[3] = 0;

    float dLeftOld = 0;
    float dRightOld = 0;
    float vL = 0;
    float vR = 0;
    int index = 0;
    while (TRUE) {
        // Calculate current speed.
        vL = (dLeft - dLeftOld) * (1000.0 / FREQUENCY);
        vR = (dRight - dRightOld) * (1000.0 / FREQUENCY);

        // Save speed to calculate average from last four readings.
        vLeft[index] = vL;
        vRight[index] = vR;

        // Adjust index pointer.
        index = (index+1)%4;

        // Save old driven distance.
        dLeftOld = dLeft;
        dRightOld = dRight;

        // Wait constant time.
        chThdSleepMilliseconds(FREQUENCY);
    }

    return (msg_t)0;
}

///////////////////////////////////////////////////////////////////////////////
// Initialization method.
///////////////////////////////////////////////////////////////////////////////

void initializeWheelEncoder(void) {
    // Initializes the ICU drivers in order to access the PWM values from the wheel encoder ICU sensor.
    icuStart(&ICUD4, &icuLeftWheelFirstSensor);
    icuStart(&ICUD12, &icuLeftWheelSecondSensor);

    icuStart(&ICUD5, &icuRightWheelFirstSensor);
    icuStart(&ICUD9, &icuRightWheelSecondSensor);

    // Wheel encoder ICU sensor left wheel first sensor is connected to PB7 and is monitored by timer 4/channel 2.
    palSetPadMode(GPIOB, 7, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM4));

    // Wheel encoder ICU sensor left wheel second sensor is connected to PB14 and is monitored by timer 12/channel 1.
    palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM12));

    // Wheel encoder ICU sensor right wheel first sensor is connected to PA1 and is monitored by timer 5/channel 2.
    palSetPadMode(GPIOA, 1, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM5));

    // Wheel encoder ICU sensor right wheel second sensor is connected to PE5 and is monitored by timer 14/channel 1.
    palSetPadMode(GPIOE, 5, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM9));

    // Enable ICU reading in background.
    icuEnable(&ICUD4);
    icuEnable(&ICUD12);

    icuEnable(&ICUD5);
    icuEnable(&ICUD9);

    // Start speed reading thread.
    ThreadSpeed = chThdCreateStatic(workingAreaThread_Speed,
                                    sizeof(workingAreaThread_Speed),
                                    NORMALPRIO + 15, Thread_Speed, NULL);
}

