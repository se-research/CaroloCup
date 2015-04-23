/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <cmath>

#include "core/base/Thread.h"
#include "core/io/ContainerConference.h"
#include "core/data/Container.h"

#include "Example7Sender.h"
#include "../Example7Data.h"

namespace examples {

    using namespace std;
    using namespace core::base;
    using namespace core::data;

    Example7Sender::Example7Sender(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "Example7Sender")
		{}

    Example7Sender::~Example7Sender() {}

    void Example7Sender::setUp() {}

    void Example7Sender::tearDown() {}


    ModuleState::MODULE_EXITCODE Example7Sender::body() {
        uint32_t seed = 24;
    	uint32_t counter = 0;

        srand(seed);

    	while (getModuleState() == ModuleState::RUNNING) {
    		// Create user data.
    		Example7Data data;
    		data.setNumericalValue(counter++);
    		data.setStringValue("LCMTEST");
		data.m_bool = false;
		data.m_char = 'E';
		data.m_double = 3.14159265358979;
		data.m_float = 1234.4321;
		data.m_uc = 'S';

    		// Create container with user data type ID 5.
    		Container c(Container::USER_DATA_5, data);

    		// Send container.
    		getConference().send(c);

    		// Restrict counter.
    		if (counter > 1000) counter = 0;

            // Provoke unexpected delays to demonstrate the need for --pulse in supercomponent.
//            double r = ((rand() / (double)RAND_MAX)) * 1000 * 700;
//            cerr << "Sleep for " << r << " ms." << endl;
//            Thread::usleep((long)r);
        }

    	return ModuleState::OKAY;
    }

} // examples
