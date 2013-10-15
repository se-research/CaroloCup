/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "TestSensorBoardSender.h"

int32_t main(int32_t argc, char **argv) {
    carolocup::TestSensorBoardSender tsbs(argc, argv);
    return tsbs.runModule();
}
