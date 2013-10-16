/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuation.
///////////////////////////////////////////////////////////////////////////////

icucnt_t last_width_LeftWheelFirstSensor, last_period_LeftWheelFirstSensor;
icucnt_t last_width_LeftWheelSecondSensor, last_period_LeftWheelSecondSensor;
icucnt_t last_width_RightWheelFirstSensor, last_period_RightWheelFirstSensor;
icucnt_t last_width_RightWheelSecondSensor, last_period_RightWheelSecondSensor;

static void icuwidth_LeftWheelFirstSensor(ICUDriver *icup) {
    last_width_LeftWheelFirstSensor = icuGetWidth(icup);
}

static void icuperiod_LeftWheelFirstSensor(ICUDriver *icup) {
    last_period_LeftWheelFirstSensor = icuGetPeriod(icup);
}

static void icuwidth_LeftWheelSecondSensor(ICUDriver *icup) {
    last_width_LeftWheelSecondSensor = icuGetWidth(icup);
}

static void icuperiod_LeftWheelSecondSensor(ICUDriver *icup) {
    last_period_LeftWheelSecondSensor = icuGetPeriod(icup);
}

static void icuwidth_RightWheelFirstSensor(ICUDriver *icup) {
    last_width_RightWheelFirstSensor = icuGetWidth(icup);
}

static void icuperiod_RightWheelFirstSensor(ICUDriver *icup) {
    last_period_RightWheelFirstSensor = icuGetPeriod(icup);
}

static void icuwidth_RightWheelSecondSensor(ICUDriver *icup) {
    last_width_RightWheelSecondSensor = icuGetWidth(icup);
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

void getWheelEncoderData(int8_t data[4]) {
    data[0] = (int8_t)last_width_LeftWheelFirstSensor; // Check read values.
    data[1] = (int8_t)last_width_LeftWheelSecondSensor; // Check read values.
    data[2] = (int8_t)last_width_RightWheelFirstSensor; // Check read values.
    data[3] = (int8_t)last_width_RightWheelSecondSensor; // Check read values.
}

void commandPrintWheelEncoder(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    chprintf(chp, "WheelEncoder: w0 = %d, p0 = %d, w1 = %d, p1 = %d, w2 = %d, p2 = %d, w3 = %d, p3 = %d, ",
            last_width_LeftWheelFirstSensor, last_period_LeftWheelFirstSensor,
            last_width_LeftWheelSecondSensor, last_period_LeftWheelSecondSensor,
            last_width_RightWheelFirstSensor, last_period_RightWheelFirstSensor,
            last_width_RightWheelSecondSensor, last_period_RightWheelSecondSensor);
    chprintf(chp, "\r\n");
}

///////////////////////////////////////////////////////////////////////////////
// Sensor reading methods.
///////////////////////////////////////////////////////////////////////////////

// The WheelEncoder is read in background by ChibiOS.

///////////////////////////////////////////////////////////////////////////////
// Initialization method.
///////////////////////////////////////////////////////////////////////////////

void initializeWheelEncoder(void) {
    // Initializes the ICU drivers in order to access the PWM values from the wheel encoder ICU sensor.
    icuStart(&ICUD4, &icuLeftWheelFirstSensor);
    icuStart(&ICUD12, &icuLeftWheelSecondSensor);

    icuStart(&ICUD5, &icuRightWheelFirstSensor);
    icuStart(&ICUD14, &icuRightWheelSecondSensor);

    // Wheel encoder ICU sensor left wheel first sensor is connected to PB7 and is monitored by timer 4/channel 2.
    palSetPadMode(GPIOB, 7, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM4));

    // Wheel encoder ICU sensor left wheel second sensor is connected to PB14 and is monitored by timer 12/channel 1.
    palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM12));

    // Wheel encoder ICU sensor right wheel first sensor is connected to PA1 and is monitored by timer 5/channel 2.
    palSetPadMode(GPIOA, 1, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM5));

    // Wheel encoder ICU sensor right wheel second sensor is connected to PA7 and is monitored by timer 14/channel 1.
    palSetPadMode(GPIOA, 7, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM14));

    // Enable ICU reading in background.
    icuEnable(&ICUD4);
    icuEnable(&ICUD12);

    icuEnable(&ICUD5);
    icuEnable(&ICUD14);
}

