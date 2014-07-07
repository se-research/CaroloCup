/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuation.
///////////////////////////////////////////////////////////////////////////////

icucnt_t last_width_CH0, last_period_CH0;
icucnt_t last_width_CH1, last_period_CH1;
icucnt_t last_width_CH2, last_period_CH2;

static void icuwidth_CH0(ICUDriver *icup) {
    last_width_CH0 = icuGetWidth(icup);
}

static void icuperiod_CH0(ICUDriver *icup) {
    last_period_CH0 = icuGetPeriod(icup);
}

static void icuwidth_CH1(ICUDriver *icup) {
    last_width_CH1 = icuGetWidth(icup);
}

static void icuperiod_CH1(ICUDriver *icup) {
    last_period_CH1 = icuGetPeriod(icup);
}

#if !USE_ONBOARD_ACCELEROMETER
static void icuwidth_CH2(ICUDriver *icup) {
    last_width_CH2 = icuGetWidth(icup);
}

static void icuperiod_CH2(ICUDriver *icup) {
    last_period_CH2 = icuGetPeriod(icup);
}
#endif

static ICUConfig icuRCReceiverCH0 = {
    ICU_INPUT_ACTIVE_HIGH,
    10000, // 10kHz ICU clock frequency.
    icuwidth_CH0,
    icuperiod_CH0,
    NULL,
    ICU_CHANNEL_1
};

static ICUConfig icuRCReceiverCH1 = {
    ICU_INPUT_ACTIVE_HIGH,
    10000, // 10kHz ICU clock frequency.
    icuwidth_CH1,
    icuperiod_CH1,
    NULL,
    ICU_CHANNEL_1
};

#if !USE_ONBOARD_ACCELEROMETER
static ICUConfig icuRCReceiverCH2 = {
    ICU_INPUT_ACTIVE_HIGH,
    10000, // 10kHz ICU clock frequency.
    icuwidth_CH2,
    icuperiod_CH2,
    NULL,
    ICU_CHANNEL_1
};
#endif

///////////////////////////////////////////////////////////////////////////////
// Interface methods.
///////////////////////////////////////////////////////////////////////////////

void getRCReceiverData(int8_t data[3]) {
    data[0] = (int8_t)last_width_CH0; // Steering: max left = 21, center = 16, max right = 10.
    data[1] = (int8_t)last_width_CH1; // Acceleration: max acceleration forward = 21, no acceleration = 16, max acceleration backwards = 11.
    data[2] = (int8_t)last_width_CH2; // Not used.
}

void commandPrintRCReceiver(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;	
    (void)argv;

    chprintf(chp, "RC-Receiver: w0 = %d, p0 = %d, w1 = %d, p1 = %d, w2 = %d, p2 = %d", last_width_CH0, last_period_CH0, last_width_CH1, last_period_CH1, last_width_CH2, last_period_CH2);
    chprintf(chp, "\r\n");
}

///////////////////////////////////////////////////////////////////////////////
// Sensor reading methods.
///////////////////////////////////////////////////////////////////////////////

// The RC receiver is read in background by ChibiOS.

///////////////////////////////////////////////////////////////////////////////
// Initialization method.
///////////////////////////////////////////////////////////////////////////////

void initializeRCReceiver(void) {
    // Initializes the ICU drivers in order to access the PWM values from the RC-receiver ICU sensor.
    icuStart(&ICUD1, &icuRCReceiverCH0);
    icuStart(&ICUD8, &icuRCReceiverCH1);
#if !USE_ONBOARD_ACCELEROMETER
    icuStart(&ICUD14, &icuRCReceiverCH2);
#endif

    // RC-receiver ICU sensor channel 0 is connected to PA8 and is monitored by timer 1/channel 1.
    palSetPadMode(GPIOA, 8, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM1));

    // RC-receiver ICU sensor channel 1 is connected to PC6 and is monitored by timer 8/channel 1.
    palSetPadMode(GPIOC, 6, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM8));

#if !USE_ONBOARD_ACCELEROMETER
    // RC-receiver ICU sensor channel 2 is connected to PA7 and is monitored by timer 14/channel 1.
    palSetPadMode(GPIOA, 7, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM14));
#endif

    // Enable ICU reading in background.
    icuEnable(&ICUD1);
    icuEnable(&ICUD8);
#if !USE_ONBOARD_ACCELEROMETER
    icuEnable(&ICUD14);
#endif
}

