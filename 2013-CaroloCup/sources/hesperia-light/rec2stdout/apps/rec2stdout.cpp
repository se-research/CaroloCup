/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "Rec2Stdout.h"

int32_t main(int32_t argc, char **argv) {
    rec2stdout::Rec2Stdout r(argc, argv);
    return r.runModule();
}
