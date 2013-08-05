/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef STM32F4TESTER_H_
#define STM32F4TESTER_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/Mutex.h"
#include "core/wrapper/ConnectionListener.h"

#include "STM32F4DataListener.h"

namespace msv {

    using namespace std;

    /**
     * This class wraps the STM32F4 Discovery Board.
     */
    class STM32F4Tester : public core::base::ConferenceClientModule, public core::wrapper::ConnectionListener, public STM32F4DataListener {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            STM32F4Tester(const STM32F4Tester &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            STM32F4Tester& operator=(const STM32F4Tester &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            STM32F4Tester(const int32_t &argc, char **argv);

            virtual ~STM32F4Tester();

            core::base::ModuleState::MODULE_EXITCODE body();

            virtual void handleConnectionError();

            virtual void nextMeasurement(const vector<InfraredSensorMeasurement> &measurement);

            virtual void nextMeasurement(const vector<UltrasonicSensorMeasurement> &measurement);

            virtual void nextMeasurement(const RazorMeasurement &measurement);

            virtual void nextMeasurement(const STM32F4AccelerometerMeasurement &measurement);

            virtual void nextMeasurement(const core::data::environment::VehicleData &vehicleData);

        private:
            virtual void setUp();

            virtual void tearDown();
    };

} // msv

#endif /*STM32F4TESTER_H_*/
