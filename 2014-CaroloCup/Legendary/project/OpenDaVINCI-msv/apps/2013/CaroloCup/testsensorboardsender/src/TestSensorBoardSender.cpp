/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/io/ContainerConference.h"
#include "core/data/Container.h"

#include "SensorBoardData.h"

#include "TestSensorBoardSender.h"

namespace carolocup {

    using namespace std;
    using namespace core::base;
    using namespace core::data;

    TestSensorBoardSender::TestSensorBoardSender(const int32_t &argc, char **argv) :
            ConferenceClientModule(argc, argv, "TestSensorBoardSender")
    		{}

    TestSensorBoardSender::~TestSensorBoardSender() {}

    void TestSensorBoardSender::setUp() {
        // This method will be call automatically _before_ running body().
    }

    void TestSensorBoardSender::tearDown() {
        // This method will be call automatically _after_ return from body().
    }

    double TestSensorBoardSender::randomDistance(const double &minDistance, const double &maxDistance) const {
        return ((int)minDistance * 10 + std::rand() % ((int)((maxDistance - minDistance) * 10))) / 10.0;
    }

    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE TestSensorBoardSender::body() {
    	while (getModuleState() == ModuleState::RUNNING) {
    		// Create sensorboard data.
            SensorBoardData sbd;
            sbd.update(1, randomDistance(1, 10)); 
            sbd.update(2, randomDistance(1, 50));

    		// Create container with user data Container::USER_DATA_0.
    		Container c(Container::USER_DATA_0, sbd);

    		// Send container.
    		getConference().send(c);
    	}

    	return ModuleState::OKAY;
    }

} // carolocup

