/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "SuperComponent.h"

int main(int argc, char **argv) {
    supercomponent::SuperComponent sc(argc, argv);
    return sc.runModule();
}
