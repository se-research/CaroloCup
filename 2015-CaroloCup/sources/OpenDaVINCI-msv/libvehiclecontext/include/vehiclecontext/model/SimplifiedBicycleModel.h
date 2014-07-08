/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef VEHICLECONTEXT_MODEL_SIMPLIFIEDBICYCLEMODEL_H_
#define VEHICLECONTEXT_MODEL_SIMPLIFIEDBICYCLEMODEL_H_

#include <string>

#include "core/base/KeyValueConfiguration.h"
#include "core/data/TimeStamp.h"
#include "core/data/control/VehicleControl.h"
#include "core/data/environment/Point3.h"
#include "core/data/environment/VehicleData.h"

#include "context/base/SystemFeedbackComponent.h"

namespace vehiclecontext {
    namespace model {

        using namespace std;

        /**
         * This class realizes the simplified bicycle model.
         */
        class OPENDAVINCI_API SimplifiedBicycleModel : public context::base::SystemFeedbackComponent {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SimplifiedBicycleModel(const SimplifiedBicycleModel&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SimplifiedBicycleModel& operator=(const SimplifiedBicycleModel&);

            public:
                /**
                 * Constructor to create a SimplifiedBicycleModel.
                 *
                 * @param configuration Configuration data.
                 */
                SimplifiedBicycleModel(const string &configuration);

                /**
                 * Constructor to create a SimplifiedBicycleModel. This constructor overrides
                 * any specified frequency in the configuration.
                 *
                 * @param freq Desired runtime frequency.
                 * @param configuration Configuration data.
                 */
                SimplifiedBicycleModel(const float &freq, const string &configuration);

                virtual ~SimplifiedBicycleModel();

                virtual float getFrequency() const;

                virtual void setup();

                virtual void tearDown();

                virtual void step(const core::wrapper::Time &t, context::base::SendContainerToSystemsUnderTest &sender);

            private:
                core::base::KeyValueConfiguration m_kvc;
                float m_freq;

                double m_wheelbase;
                double m_maxSteeringLeftRad;
                double m_maxSteeringRightRad;
                int32_t m_invertedSteering;

                double m_maxSpeed;
                bool m_useSpeedControl;
                double m_faultModelNoise;

                double m_esum;
                double m_desiredSpeed;
                double m_desiredAcceleration;
                double m_desiredSteer;

                double m_speed;
                core::data::TimeStamp m_previousTime;
                core::data::environment::Point3 m_oldPosition;
                core::data::environment::Point3 m_orientation;
                double m_heading;
                core::data::environment::VehicleData m_vehicleData;

                core::data::control::VehicleControl m_vehicleControl;
                bool m_hasReceivedVehicleControl;
        };

    }
} // vehiclecontext::model

#endif /*VEHICLECONTEXT_MODEL_SIMPLIFIEDBICYCLEMODEL_H_*/
