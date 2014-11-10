/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef USER_SHELL_H
#define USER_SHELL_H

#include "DiscoveryBoard.h"

// Commands that are available for the shell.
static const ShellCommand commands[] = {
    {"mem", commandMem},
    {"threads", commandThreads},
    {"printAccelerometer", commandPrintAccelerometer},
    {"pa", commandPrintAccelerometer},
    {"printTemperature", commandPrintTemperature},
    {"pt", commandPrintTemperature},
    {"printInfraredDistances", commandPrintInfraredDistances},
    {"pdi", commandPrintInfraredDistances},
    {"printUltrasonicDistances", commandPrintUltrasonicDistances},
    {"pdu", commandPrintUltrasonicDistances},
    {"setUltrasonicAddress", commandSetUltrasonicAddress},
    {"sua", commandSetUltrasonicAddress},
    {"triggerSingleUltrasonicMeasurement", commandTriggerSingleUltrasonicMeasurement},
    {"tsum", commandTriggerSingleUltrasonicMeasurement},
    {"startContinuousUltrasonicMeasurements", commandStartContinuousUltrasonicMeasurements},
    {"scum", commandStartContinuousUltrasonicMeasurements},
    {"stopContinuousUltrasonicMeasurements", commandStopContinuousUltrasonicMeasurements},
    {"scumoff", commandStopContinuousUltrasonicMeasurements},
    {"controlSteeringAccelerationMotors", commandControlSteeringAccelerationMotors},
    {"csam", commandControlSteeringAccelerationMotors},
    {"printRCReceiver", commandPrintRCReceiver},
    {"prc", commandPrintRCReceiver},
    {"printWheelEncoder", commandPrintWheelEncoder},
    {"pwe", commandPrintWheelEncoder},
    {"resetWheelEncoder", commandResetWheelEncoder},
    {"rwe", commandResetWheelEncoder},
//    {"printRazor9DoFIMU", commandPrintRazor9DoFIMU},
//    {"pr", commandPrintRazor9DoFIMU},
    {"rpn", commandRPNCalculator},
    {NULL, NULL}
};

/**
 * This method spawns an interactive user shell from
 * within an infinite loop (thus, this method will
 * not return!).
 */
int spawnShell(void);

#endif // USER_SHELL_H

