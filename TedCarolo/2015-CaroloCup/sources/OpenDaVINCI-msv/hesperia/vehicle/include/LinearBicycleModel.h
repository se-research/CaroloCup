/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#ifndef VEHICLE_LINEARBICYCLEMODEL_H_
#define VEHICLE_LINEARBICYCLEMODEL_H_

#include <string>
#include <utility>

#include "core/base/KeyValueConfiguration.h"
#include "core/data/TimeStamp.h"
#include "hesperia/data/environment/EgoState.h"
#include "core/data/environment/VehicleData.h"

namespace vehicle {

    using namespace std;

    class LinearBicycleModel {
        public:
            LinearBicycleModel(const core::base::KeyValueConfiguration &kvc);
            virtual ~LinearBicycleModel();

            void accelerate(const double& value);
            void brake(const double& value);
            void steer(const double& value);

            virtual hesperia::data::environment::EgoState computeEgoState();

            hesperia::data::environment::VehicleData getVehicleData() const;

        protected:
            double m_minimumTurningRadius;
            double m_vehicleMass;
            double m_adherenceCoefficient;
            double m_idleForce;
            double m_Ksteering;
            double m_maximumSteeringRate;
            double m_Kthrottle;
            double m_tauBrake;
            double m_KstaticBrake;
            double m_KdynamicBrake;

            double m_steeringRate;
            double m_motorForce;
            double m_brakeForce;
            double m_adherenceForce;
            double m_turningCurvature;
            double m_deltaHeading;

            double m_vehicleDesiredRotationalEffort;
            double m_vehicleDesiredLinearEffortX;
            double m_vehicleDesiredResistiveEffortX;

            double m_vehicleRotationalEffort;
            double m_vehicleLinearEffortX;
            double m_vehicleResistiveEffortX;
            double m_speed;
            double m_speedOld;
            double m_heading;

            int32_t m_direction;

            core::data::TimeStamp m_previousTime;
            hesperia::data::environment::Point3 m_oldPosition;
            hesperia::data::environment::Point3 m_orientation;

            hesperia::data::environment::Point3 m_oldAcceleration;

            hesperia::data::environment::VehicleData m_vehicleData;
    };
}

#endif // VEHICLE_LINEARBICYCLEMODEL_H_
