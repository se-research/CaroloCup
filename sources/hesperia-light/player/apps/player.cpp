/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "Player.h"

int32_t main(int32_t argc, char **argv) {
    player::Player p(argc, argv);
    return p.runModule();
}
