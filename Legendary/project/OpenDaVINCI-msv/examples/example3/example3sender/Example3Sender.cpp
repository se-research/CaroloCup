/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/io/ContainerConference.h"
#include "core/data/Container.h"

#include "Example3Sender.h"
#include "../Example3Data.h"

namespace examples {

    using namespace std;
    using namespace core::base;
    using namespace core::data;

    Example3Sender::Example3Sender(const int32_t &argc, char **argv) :
            ConferenceClientModule(argc, argv, "Example3Sender")
    		{}

    Example3Sender::~Example3Sender() {}

    void Example3Sender::setUp() {}

    void Example3Sender::tearDown() {}


    ModuleState::MODULE_EXITCODE Example3Sender::body() {
    	uint32_t counter = 0;

    	while (getModuleState() == ModuleState::RUNNING) {
    		// Create user data.
    		Example3Data data;
    		data.setNumericalValue(counter++);
    		data.setStringValue("Example3Sender");

    		// Create container with user data type ID 5.
    		Container c(Container::USER_DATA_5, data);

    		// Send container.
    		getConference().send(c);

    		// Restrict counter.
    		if (counter > 1000) counter = 0;
    	}

    	return ModuleState::OKAY;
    }

} // examples
