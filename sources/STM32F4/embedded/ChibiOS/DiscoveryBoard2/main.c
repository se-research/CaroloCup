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
    initializeOnboardAccelerometer(); // STM32F4 onboard accelerometer.
    //initializeTemperature(); // STM32F4 onboard temperature sensor (MUST NOT BE USED TOGETHER WITH INFRARED!!!).
    initializeInfrared(); // Three infrared sensors, connected to PC1, PC4, and PC5.
    initializeUltrasonic(); // Three ultrasonic sensors, connected via I2C to PB6 (SCL) and PB9 (SDA).
    //initializeRazor9DoFIMU(); // Razor 9DoF IMU, connected via PD8 (TX) and PD9 (RX).

    // Initialize actors.
    initializeSteeringAcceleration();
}

void wakeThreads(void) {
    chMsgSend(getThreadOnboardAccelerometer(), '1'); chThdSleepMilliseconds(2);
    //chMsgSend(getThreadTemperature(), '1'); chThdSleepMilliseconds(2);
    chMsgSend(getThreadInfrared(), '1'); chThdSleepMilliseconds(2);
    chMsgSend(getThreadUltrasonic(), '1'); chThdSleepMilliseconds(2);
    //chMsgSend(getThreadRazor9DoFIMU(), '1'); chThdSleepMilliseconds(5);

    chMsgSend(getThreadAcceleration(), '1'); chThdSleepMilliseconds(2);
    chMsgSend(getThreadSteering(), '1'); chThdSleepMilliseconds(2);
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
        runProtocol();

    return 0;
}

