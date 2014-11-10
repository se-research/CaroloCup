/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "PlayerModule.h"

int32_t main(int32_t argc, char **argv) {
    player::PlayerModule pm(argc, argv);
    return pm.runModule();
}
