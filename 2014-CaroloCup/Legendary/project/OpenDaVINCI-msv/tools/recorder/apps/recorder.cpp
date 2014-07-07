/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Recorder.h"

int32_t main(int32_t argc, char **argv) {
    recorder::Recorder r(argc, argv);
    return r.runModule();
}
