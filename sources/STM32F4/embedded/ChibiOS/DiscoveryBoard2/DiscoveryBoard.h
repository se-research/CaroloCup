/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef DISCOVERYBOARD_H
#define DISCOVERYBOARD_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "ch.h"
#include "hal.h"
#include "shell.h"
#include "chprintf.h"

// Here, all the header files for interacting with sensors are collected.
#include "Sensors/Infrared/Infrared.h"
//#include "Sensors/Razor9DoFIMU/Razor9DoFIMU.h"
#include "Sensors/Ultrasonic/Ultrasonic.h"
#include "Sensors/OnboardAccelerometer/OnboardAccelerometer.h"
#include "Sensors/Temperature/Temperature.h"

// Here, all the header files for interactively communicating with the Discovery Board are collected.
#include "UI/LEDs.h"
#include "UI/UserButton.h"
#include "UI/UserShellCommands.h"
#include "UI/UserShell.h"
#include "UI/UseShell.h"

#include "Protocol/Protocol.h"
#include "Protocol/ProtocolHandler.h"

#include "USB/USB.h"

#endif // DISCOVERYBOARD_H

