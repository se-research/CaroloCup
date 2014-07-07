/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

// Configuration of the shell.
static const ShellConfig shellConfiguration = {
  (BaseSequentialStream *)&SDU1,
  commands
};

int spawnShell(void) {
    const int ONE_THOUSAND_MILLISECONDS = 1000;
    const int THREAD_WORKING_AREA_SIZE = 2048;

    Thread *shellThread = NULL;

    while (TRUE) {
        // If we don't have a shell but a living USB connection, create a new thread with the shell.
        if (!shellThread && isUSBActive()) {
            shellThread = shellCreate(&shellConfiguration, THD_WA_SIZE(THREAD_WORKING_AREA_SIZE), NORMALPRIO);
        }
        else if (chThdTerminated(shellThread)) {
            // Recover memory of the previous shell.
            chThdRelease(shellThread);

            // Trigger spawning of a new shell.
            shellThread = NULL;
        }

        chThdSleepMilliseconds(ONE_THOUSAND_MILLISECONDS);
    }
}

