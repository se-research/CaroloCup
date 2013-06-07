/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "Recorder.h"

int32_t main(int32_t argc, char **argv) {
    recorder::Recorder r(argc, argv);
    return r.runModule();
}
