/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "ChaseCar.h"

int32_t main(int32_t argc, char **argv) {
    chasecar::ChaseCar cc(argc, argv);
    return cc.runModule();
}
