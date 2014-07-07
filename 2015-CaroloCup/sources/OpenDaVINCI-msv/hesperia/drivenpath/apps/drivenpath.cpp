/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include "DrivenPath.h"

int32_t main(int32_t argc, char **argv) {
    measurements::DrivenPath dp(argc, argv);
    return dp.runModule();
}
