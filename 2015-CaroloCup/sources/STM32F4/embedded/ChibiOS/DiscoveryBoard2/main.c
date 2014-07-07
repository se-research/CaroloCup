/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

void initializePeripherals(void) {
    // Initialize interface to exchange data.
    initializeUSB(); // USB driver and USB-CDC link.

    // Initialize sensors.
#if USE_ONBOARD_ACCELEROMETER
    initializeOnboardAccelerometer(); // STM32F4 onboard accelerometer.
#endif

#if USE_ONBOARD_TEMPERATURE
    initializeTemperature(); // STM32F4 onboard temperature sensor (MUST NOT BE USED TOGETHER WITH INFRARED!!!).
#endif

#if !USE_ONBOARD_TEMPERATURE && USE_INFRARED
    initializeInfrared(); // Three infrared sensors, connected to PC1, PC4, and PC5.
#endif

#if USE_ULTRASONIC
    initializeUltrasonic(); // Three ultrasonic sensors, connected via I2C to PB6 (SCL) and PB9 (SDA).
#endif

#if USE_RCRECEIVER
    initializeRCReceiver(); // RC-Receiver ICU sensor, connected via ICU to PA8 (RC channel 0), PC6 (RC channel 1), PA7 (RC channel 2).
#endif

#if USE_WHEELENCODER
    initializeWheelEncoder(); // WheelEncoder ICU sensor, connected via ICU to PB7 (first sensor left wheel), PB8 (first sensor right wheel).
#endif

#if USE_RAZORBOARD
    //initializeRazor9DoFIMU(); // Razor 9DoF IMU, connected via PD8 (TX) and PD9 (RX).
#endif

#if USE_ACTORS
    // Initialize actors.
    initializeSteeringAcceleration();
#endif

    // Initialize user-defined protocol.
    initializeUserProtocol();
}

void wakeThreads(void) {
#if USE_ONBOARD_ACCELEROMETER
    chMsgSend(getThreadOnboardAccelerometer(), '1'); chThdSleepMilliseconds(2);
#endif

#if USE_ONBOARD_TEMPERATURE
    chMsgSend(getThreadTemperature(), '1'); chThdSleepMilliseconds(2);
#endif

#if !USE_ONBOARD_TEMPERATURE && USE_INFRARED
    chMsgSend(getThreadInfrared(), '1'); chThdSleepMilliseconds(2);
#endif

#if USE_ULTRASONIC
    chMsgSend(getThreadUltrasonic(), '1'); chThdSleepMilliseconds(2);
#endif

#if USE_RCRECEIVER
    // RCReceiver is not a thread!
#endif

#if USE_WHEELENCODER
    chMsgSend(getThreadSpeed(), '1'); chThdSleepMilliseconds(2);
#endif

#if USE_RAZORBOARD
    //chMsgSend(getThreadRazor9DoFIMU(), '1'); chThdSleepMilliseconds(2);
#endif

#if USE_ACTORS
    chMsgSend(getThreadAcceleration(), '1'); chThdSleepMilliseconds(2);
    chMsgSend(getThreadSteering(), '1'); chThdSleepMilliseconds(2);
#endif
}

int main(void) {
    // HAL initialization, this also initializes the configured device
    // drivers and performs the board-specific initializations.
    // Kernel initialization, the main() function becomes a thread and the RTOS is active.
    halInit();
    chSysInit();

    // Initialize all peripherals.
    initializePeripherals();

    // Initial state machine right after booting:
    // If the user presses the button within 5s, the STM32F4 Discovery Board
    // switches to interactive mode where the user can connect
    // to it using a terminal program (e.g. screen /dev/ttyACM0).
    int startShell = useShell();

    // Since all initialized threads are awaiting for the signal to start, we issue this signal here.
    wakeThreads();

    if (startShell)
        spawnShell();
    else
        runProtocol2();

    return 0;
}

