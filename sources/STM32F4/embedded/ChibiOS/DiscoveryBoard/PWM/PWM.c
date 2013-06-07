#include "ch.h"
#include "hal.h"
#include "chprintf.h"
#include <stdlib.h>

#include "PWM/PWM.h"

/*
 * PWM configuration structure.
 * Cyclic callback enabled, channels 1 and 4 enabled without callbacks,
 * the active state is a logic one.
 */
static const PWMConfig pwmcfg = {
  100000,                                   /* 100kHz PWM clock frequency.  */
  128,                                      /* PWM period is 128 cycles.    */
  NULL,                                     /* No callback after each period */
  {
   {PWM_OUTPUT_ACTIVE_HIGH, NULL},
   {PWM_OUTPUT_ACTIVE_HIGH, NULL},
   {PWM_OUTPUT_ACTIVE_HIGH, NULL},
   {PWM_OUTPUT_ACTIVE_HIGH, NULL},
  },
  /* HW dependent part.*/
  0
};

/*
 * starts the PWM device
 */
void mypwmInit(void){
  /*
   * Initializes the PWM driver 4, routes the TIM4 outputs to the board LEDs.
   */
  pwmStart(&PWMD4, &pwmcfg);
  palSetPadMode(GPIOD, GPIOD_LED4, PAL_MODE_ALTERNATE(2));      /* Green.   */
  palSetPadMode(GPIOD, GPIOD_LED3, PAL_MODE_ALTERNATE(2));      /* Orange.  */
  palSetPadMode(GPIOD, GPIOD_LED5, PAL_MODE_ALTERNATE(2));      /* Red.     */
  palSetPadMode(GPIOD, GPIOD_LED6, PAL_MODE_ALTERNATE(2));      /* Blue.    */
}
