/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef EGOCONTROLLER_CONTROLLER_H_
#define EGOCONTROLLER_CONTROLLER_H_

#include "hesperia/data/environment/EgoState.h"

namespace egocontroller {

    using namespace std;

    class Controller
    {
        public:
            Controller();
            virtual ~Controller();

            virtual void doWork() = 0;
            virtual hesperia::data::environment::EgoState getEgoState() = 0;
    };
}

#endif // EGOCONTROLLER_CONTROLLER_H_
