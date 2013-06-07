/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "SuperComponent.h"

int main(int argc, char **argv) {
    supercomponent::SuperComponent sc(argc, argv);
    return sc.runModule();
}
