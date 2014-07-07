/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#ifndef VEHICLE_H_
#define VEHICLE_H_

#include "core/base/ConferenceClientModule.h"

namespace vehicle {

    using namespace std;

    /**
     * This class encapsulates the vehicle.
     */
    class Vehicle : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Vehicle(const Vehicle &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Vehicle& operator=(const Vehicle &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Vehicle(const int32_t &argc, char **argv);

            virtual ~Vehicle();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();

            virtual void tearDown();

            core::base::ModuleState::MODULE_EXITCODE runLinearBicycleModel();

            core::base::ModuleState::MODULE_EXITCODE runLinearBicycleModelNew(const bool &withSpeedController);
    };

} // vehicle

#endif /*VEHICLE_H_*/
