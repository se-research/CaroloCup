#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ch.h"
#include "hal.h"

#include "shell.h"
#include "chprintf.h"

#include "PWM/PWM.h"
#include "ADC/ADC.h"
#include "USB/USB.h"
#include "Misc/Misc.h"
#include "SPI/SPI.h"
#include "IMU/IMU.h"




/*
 * assert Shell Commands to functions
 */

static const ShellCommand commands[] = {
  {"mem", cmd_mem},
  {"threads", cmd_threads},
  {"measure", cmd_measure},
  {"m", cmd_measure},
  {"measureAnalog", cmd_measureA},
  {"ma", cmd_measureA},
  {"vref", cmd_Vref},
  {"v", cmd_Vref},
  {"temperature", cmd_Temperature},
  {"te", cmd_Temperature},
  {"measureDirect", cmd_measureDirect},
  {"md", cmd_measureDirect},
  {"measureContinuous", cmd_measureCont},
  {"mc", cmd_measureCont},
  {"readContinuousData", cmd_measureRead},
  {"rd", cmd_measureRead},
  {"stopContinuous", cmd_measureStop},
  {"sc", cmd_measureStop},
  {"printAccel", cmd_printAccel},
  {"pa", cmd_printAccel},
  {NULL, NULL}
};


/*
 * Shell configuration
 */

#define SHELL_WA_SIZE   THD_WA_SIZE(2048)

static const ShellConfig shell_cfg1 = {
  (BaseSequentialStream *)&SDU1,
  commands
};


/*
 * Application entry point.
 */
int main(void) {
  /*
   * Shell thread
   */
  Thread *shelltp = NULL;

  /*
   * System initializations.
   * - HAL initialization, this also initializes the configured device drivers
   *   and performs the board-specific initializations.
   * - Kernel initialization, the main() function becomes a thread and the
   *   RTOS is active.
   */
  halInit();
  chSysInit();

  /*
   * Activate custom stuff
   */
  mypwmInit();
  myADCinit();
  mySPIinit();

  /*
   * Activates the USB driver and then the USB bus pull-up on D+.
   */
  myUSBinit();

  /*
   * Initialize IMU.
   */
  initIMU();


  /*
   * Main loop, does nothing except spawn a shell when the old one was terminated
   */
  while (TRUE) {
    if (!shelltp && isUsbActive())
      {
        shelltp = shellCreate(&shell_cfg1, SHELL_WA_SIZE, NORMALPRIO);
      }
    else if (chThdTerminated(shelltp)) {
      chThdRelease(shelltp);    /* Recovers memory of the previous shell.   */
      shelltp = NULL;           /* Triggers spawning of a new shell.        */
    }

    chThdSleepMilliseconds(1000);
  }
}
