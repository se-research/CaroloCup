/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

#include "core/base/KeyValueConfiguration.h"

#include "Example2.h"

namespace examples {

    using namespace std;
    using namespace core::base;

    Example2::Example2(const int32_t &argc, char **argv) :
            ConferenceClientModule(argc, argv, "Example2")
    		{}

    Example2::~Example2() {}

    void Example2::setUp() {}

    void Example2::tearDown() {}


    ModuleState::MODULE_EXITCODE Example2::body() {
    	// Get the configuration data for this module:
    	KeyValueConfiguration kv = getKeyValueConfiguration();

    	// The search for configuration values is case-insensitive:
    	cout << "myKey1: " << kv.getValue<string>("example2.MYKEY1") << endl;

    	// Get the second value as integer to be used for further calculations for example:
    	uint32_t data = kv.getValue<uint32_t>("example2.mysecondkey");
    	cout << "mySecondKey: " << data << endl;

    	// Get the module's identifier (i.e. if the module is started on the command line with example2 --cid=123 --id=1)
    	cout << "My identifier is: " << getIdentifier() << endl;

    	if (getIdentifier() == "1") {
    		cout << "I am 1 and my additional configuration data is: " << kv.getValue<string>("example2.withid") << endl;
    	}

    	if (getIdentifier() == "2") {
    		cout << "I am 2 and my additional configuration data is: " << kv.getValue<string>("example2.withid") << endl;
    	}

    	return ModuleState::OKAY;
    }

} // examples
