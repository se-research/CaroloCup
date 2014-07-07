/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef EGOCONTROLLER_SIMPLECONTROLBEHAVIOUR_H_
#define EGOCONTROLLER_SIMPLECONTROLBEHAVIOUR_H_

#include "core/data/TimeStamp.h"
#include "core/data/environment/Point3.h"

#include "ControlBehaviour.h"

namespace egocontroller {

    using namespace std;

    class SimpleControlBehaviour : public ControlBehaviour
    {
        public:
            SimpleControlBehaviour(const core::data::environment::Point3 &translation, const double &rotZ);
            virtual ~SimpleControlBehaviour();

            virtual void accelerate(const double& value);
            virtual void brake(const double& value);
            virtual void turnLeft(const double& value);
            virtual void turnRight(const double& value);
            virtual void stop();
            virtual hesperia::data::environment::EgoState computeEgoState();

        protected:
            core::data::TimeStamp m_previousTime;
            core::data::environment::Point3 m_oldPosition;
            core::data::environment::Point3 m_orientation;

            double m_angle;
            double m_speed;
            double m_speedInLastCycle;
    };
}

#endif // EGOCONTROLLER_SIMPLECONTROLBEHAVIOUR_H_
