/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Split.h"

int32_t main(int32_t argc, char **argv) {
    split::Split s(argc, argv);
    return s.runModule();
}
