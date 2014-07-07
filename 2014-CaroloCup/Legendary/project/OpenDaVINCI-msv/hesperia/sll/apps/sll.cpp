/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include "SLL.h"

int32_t main(int32_t argc, char **argv) {
    sll::SLL tsdsensor(argc, argv);
    return tsdsensor.runModule();
}
