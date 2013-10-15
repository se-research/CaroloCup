/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

#include "Example1.h"

namespace examples {

    using namespace std;
    using namespace core::base;

    Example1::Example1(const int32_t &argc, char **argv) :
            ConferenceClientModule(argc, argv, "Example1")
    		{}

    Example1::~Example1() {}

    void Example1::setUp() {}

    void Example1::tearDown() {}


    ModuleState::MODULE_EXITCODE Example1::body() {
        cout << "Hello OpenDaVINCI World!" << endl;

        return ModuleState::OKAY;
    }

} // examples
