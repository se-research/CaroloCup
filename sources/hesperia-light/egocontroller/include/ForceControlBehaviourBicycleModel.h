/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef EGOCONTROLLER_FORCECONTROLBEHAVIOURBICYCLEMODEL_H_
#define EGOCONTROLLER_FORCECONTROLBEHAVIOURBICYCLEMODEL_H_

#include "core/data/TimeStamp.h"

#include "ControlBehaviour.h"

namespace egocontroller {

    using namespace std;

    class ForceControlBehaviourBicycleModel : public ControlBehaviour
    {
        public:
            ForceControlBehaviourBicycleModel(
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
                    const double &lF,
                    const double &lR,
                    const double &iM,
                    const double &cF,
                    const double &cR);
            virtual ~ForceControlBehaviourBicycleModel();

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
            double m_distanceCenterOfMassToFrontAxle;
            double m_distanceCenterOfMassToRearAxle;
            double m_inertiaMoment;
            double m_skewStiffnessFront;
            double m_skewStiffnessRear;

            double m_steeringRate;
            double m_motorForce;
            double m_brakeForce;
            double m_adherenceForce;
            double m_velocity;

            double m_yawRate;
            double m_yaw;
            double m_attitudeAngle; // "Swimming angle" :-)
            double m_steeringAngle;

            double m_vehicleDesiredRotationalEffort;
            double m_vehicleDesiredLinearEffortX;
            double m_vehicleDesiredResistiveEffortX;

            double m_vehicleRotationalEffort;
            double m_vehicleLinearEffortX;
            double m_vehicleResistiveEffortX;

            core::data::TimeStamp m_previousTime;
            hesperia::data::environment::Point3 m_oldPosition;
            hesperia::data::environment::Point3 m_orientation;
    };
}

#endif // EGOCONTROLLER_FORCECONTROLBEHAVIOURBICYCLEMODEL_H_
