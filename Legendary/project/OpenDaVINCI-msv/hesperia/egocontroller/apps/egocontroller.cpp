/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "EgoController.h"

int main(int argc, char **argv) {
    egocontroller::EgoController controller(argc, argv);
    return controller.runModule();
}
