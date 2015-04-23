/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

#include "core/data/Container.h"
#include "core/io/ContainerConference.h"

#include "Example7Receiver.h"
#include "../Example7Data.h"

namespace examples {

    using namespace std;
    using namespace core::base;
    using namespace core::data;

    Example7Receiver::Example7Receiver(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "Example7Receiver")
		{}

    Example7Receiver::~Example7Receiver() {}

    void Example7Receiver::setUp() {}

    void Example7Receiver::tearDown() {}

    ModuleState::MODULE_EXITCODE Example7Receiver::body() {
        uint32_t sum = 0;
        uint32_t expected_sum = 0;
        uint32_t counter = 0;

    	while (getModuleState() == ModuleState::RUNNING) {
			Container c = getKeyValueDataStore().get(Container::USER_DATA_5);
			Example7Data data = c.getData<Example7Data>();
            sum += data.getNumericalValue();
			cout << "Latest container from data type " << (uint32_t)c.getDataType() << ", content: " << data.toString() << ", sum = " << sum << endl;
	    
            if (sum > 0) {
                counter++;
                expected_sum += counter;
                cout << "Diff: " << (expected_sum - sum) << endl;
            }
    	}

    	return ModuleState::OKAY;
    }

} // examples
