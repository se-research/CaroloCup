/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef EGOCONTROLLER_CONTROLBEHAVIOUR_H_
#define EGOCONTROLLER_CONTROLBEHAVIOUR_H_

#include "hesperia/data/environment/EgoState.h"

namespace egocontroller {

    using namespace std;

    class ControlBehaviour
    {
        public:
            ControlBehaviour();
            virtual ~ControlBehaviour();

            virtual void accelerate(const double& value) = 0;
            virtual void brake(const double& value) = 0;
            virtual void turnLeft(const double& value) = 0;
            virtual void turnRight(const double& value) = 0;
            virtual void stop() = 0;

            virtual hesperia::data::environment::EgoState computeEgoState() = 0;
    };
}

#endif // EGOCONTROLLER_CONTROLBEHAVIOUR_H_
