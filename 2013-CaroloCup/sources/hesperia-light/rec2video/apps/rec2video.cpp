/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "Rec2Video.h"

int32_t main(int32_t argc, char **argv) {
    rec2video::Rec2Video r(argc, argv);
    return r.runModule();
}
