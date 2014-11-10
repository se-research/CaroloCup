/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

/**
 * This method implements a simple reverse polish notation calculator.
 *
 * @param input The string containing a formula in RPN, e.g. 2 3 +
 * @return Processed result.
 */
int RPN_calculator(char *input);

/**
 * This method enables interactive access to the reverse polish
 * calculator.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandRPNCalculator(BaseSequentialStream *chp, int argc, char *argv[]);

