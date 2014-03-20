/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Player.h"

int32_t main(int32_t argc, char **argv) {
    player::Player p(argc, argv);
    return p.runModule();
}
