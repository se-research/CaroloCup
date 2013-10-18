/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef DRIVER_H_2
#define DRIVER_H_2

#include "core/base/ConferenceClientModule.h"
#include "/home/fredrik/2014-CaroloCup/Legendary/project/OpenDaVINCI-msv/apps/2013/CaroloCup/cc-data/include/LaneDetectionData.h"
#include <opencv/cv.h>

namespace carolocup {

    using namespace std;
	using namespace msv;
	using namespace cv;

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

			/**
			 * The control algorithm
			 * @param lateralError The current lateral error
			 * @param angularError The current angular error
			 * @param curvature The current curvature
			 * @param curvatureDifferential Approximate derivative of the curvature w.r.t. path coordinate s
			 * @param steeringWheelAngle
			 * @param The current steering wheel angle
			 * @param speed The current set speed
			 * @param controlGains Pointer to vector of control gains
			 * @return The new desired steeringWheelAngle
			 */
			float feedbackLinearizationController();

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
			bool hasReceivedLaneDetectionData;
			// Define control parameters
			float controlGains[3];	//For feedback linearization controller
			float deltaPath, lateralError, angularError, steeringWheelAngle, curvature, curvatureDifferential, oldCurvature, speed;
			float desiredSteeringWheelAngle;
			float ANGLE_TO_CURVATURE;
			float length;
			float SCALE_FACTOR;	//For example, 12000 dpm (dots-per-meter)
			float scaledLength;
			Vec4i leftLine;
			Vec4i rightLine;
			Vec4i dashedLine;
    };

} // carolocup

#endif /*DRIVER_H_*/
