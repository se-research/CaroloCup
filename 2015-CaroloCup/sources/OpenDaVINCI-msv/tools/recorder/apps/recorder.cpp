/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "RecorderModule.h"

int32_t main(int32_t argc, char **argv) {
    recorder::RecorderModule r(argc, argv);
    return r.runModule();
}
