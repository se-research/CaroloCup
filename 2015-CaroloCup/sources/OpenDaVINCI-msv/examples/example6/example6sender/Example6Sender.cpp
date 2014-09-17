/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/io/ContainerConference.h"
#include "core/data/Container.h"

#include "Example6Sender.h"
#include "generated/examples/Example6Data.h"

namespace examples {

    using namespace std;
    using namespace core::base;
    using namespace core::data;

    Example6Sender::Example6Sender(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "Example6Sender")
		{}

    Example6Sender::~Example6Sender() {}

    void Example6Sender::setUp() {}

    void Example6Sender::tearDown() {}


    ModuleState::MODULE_EXITCODE Example6Sender::body() {
    	uint32_t counter = 0;

		Example6Data data;
    	while (getModuleState() == ModuleState::RUNNING) {
    		// Create user data.
    		data.setNumericalValue(counter);
    		data.setStringValue("Example6Sender");

            float f = (0.5f + (float)counter);
            cout << "Adding " << f << " to the list." << endl;
            data.addTo_ListOfValues(f);

    		// Create container with user data type ID 5.
    		Container c(Container::USER_DATA_5, data);

    		// Send container.
    		getConference().send(c);

            counter++;
    		// Restrict counter.
    		if (counter > 1000) counter = 0;
    	}

    	return ModuleState::OKAY;
    }

} // examples
