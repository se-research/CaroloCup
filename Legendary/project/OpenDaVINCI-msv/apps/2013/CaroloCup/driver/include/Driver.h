/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef DRIVER_H_
#define DRIVER_H_

#include "core/base/ConferenceClientModule.h"

namespace carolocup {

    using namespace std;

    /**
     * This class is a skeleton to send driving commands to Hesperia-light's vehicle driving dynamics simulation.
     */
    class Driver : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Driver(const Driver &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Driver& operator=(const Driver &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Driver(const int32_t &argc, char **argv);

            virtual ~Driver();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();
            virtual void tearDown();
			// Define control parameters
			float controlGains[3];	//For feedback linearization controller
			float deltaPath, lateralError, angularError, steeringWheelAngle, curvature, curvatureDifferential, oldCurvature, speed;
			const float ANGLE_TO_CURVATURE = 2.5;
			const float length = 0.3;
			const float SCALE_FACTOR = 12000;	//For example, 12000 dpm (dots-per-meter)
			Lines lines;
    };

} // carolocup

#endif /*DRIVER_H_*/
