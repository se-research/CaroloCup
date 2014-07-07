/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef EGOCONTROLLER_FORCECONTROLBEHAVIOURSIMPLIFIEDBICYCLEMODEL_H_
#define EGOCONTROLLER_FORCECONTROLBEHAVIOURSIMPLIFIEDBICYCLEMODEL_H_

#include "core/data/TimeStamp.h"

#include "ControlBehaviour.h"

namespace egocontroller {

    using namespace std;

    class ForceControlBehaviourSimplifiedBicycleModel : public ControlBehaviour
    {
        public:
            ForceControlBehaviourSimplifiedBicycleModel(
                    const double& minimumTurningRadius,
                    const double& vehicleMass,
                    const double& adherenceCoefficient,
                    const double& idleForce,
                    const double& Ksteering,
                    const double& maximumSteeringRate,
                    const double& Kthrottle,
                    const double& tauBrake,
                    const double& KstaticBrake,
                    const double& KdynamicBrake,
                    const double &l);
            virtual ~ForceControlBehaviourSimplifiedBicycleModel();

            virtual void accelerate(const double& value);
            virtual void brake(const double& value);
            virtual void turnLeft(const double& value);
            virtual void turnRight(const double& value);
            virtual void stop();
            virtual hesperia::data::environment::EgoState computeEgoState();

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
            double m_length;

            double m_steeringRate;
            double m_motorForce;
            double m_brakeForce;
            double m_adherenceForce;
            double m_velocity;

            double m_yaw;
            double m_steeringAngle;

            double m_vehicleDesiredSteeringAngle;
            double m_vehicleSteeringAngle;

            double m_vehicleDesiredLinearEffortX;
            double m_vehicleDesiredResistiveEffortX;

            double m_vehicleLinearEffortX;
            double m_vehicleResistiveEffortX;

            core::data::TimeStamp m_previousTime;
            core::data::environment::Point3 m_oldPosition;
            core::data::environment::Point3 m_orientation;
    };
}

#endif // EGOCONTROLLER_FORCECONTROLBEHAVIOURSIMPLIFIEDBICYCLEMODEL_H_
