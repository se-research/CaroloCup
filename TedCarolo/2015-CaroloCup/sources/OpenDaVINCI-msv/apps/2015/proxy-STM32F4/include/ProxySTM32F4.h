/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PROXYSTM32F4_H_
#define PROXYSTM32F4_H_

#include <map>

#include "core/base/ConferenceClientModule.h"
#include "core/base/Mutex.h"
#include "core/data/environment/VehicleData.h"
#include "core/wrapper/StringListener.h"
#include "core/wrapper/ConnectionListener.h"

#include "STM32F4DataListener.h"
#include "PointSensor.h"
#include "SensorBoardData.h"
#include "UserButtonData.h"

namespace msv {

    using namespace std;

    /**
     * This class wraps the STM32F4 Discovery Board.
     */
    class ProxySTM32F4 : public core::base::ConferenceClientModule, public STM32F4DataListener, public core::wrapper::ConnectionListener {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            ProxySTM32F4(const ProxySTM32F4 &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            ProxySTM32F4& operator=(const ProxySTM32F4 &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            ProxySTM32F4(const int32_t &argc, char **argv);

            virtual ~ProxySTM32F4();

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

        private:
            map<uint16_t, PointSensor*> m_mapOfPointSensors;
            
            core::base::Mutex m_userButtonMutex;
            UserButtonData m_userButtonData;

            core::base::Mutex m_sensorBoardMutex;
            SensorBoardData m_sensorBoardData;

            core::base::Mutex m_vehicleDataMutex;
            core::data::environment::VehicleData m_vehicleData;

            bool m_debug;
    };

} // msv

#endif /*PROXYSTM32F4_H_*/
