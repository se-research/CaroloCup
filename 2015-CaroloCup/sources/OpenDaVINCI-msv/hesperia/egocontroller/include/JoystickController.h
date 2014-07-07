/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef EGOCONTROLLER_JOYSTICKCONTROLLER_H_
#define EGOCONTROLLER_JOYSTICKCONTROLLER_H_

#include "Controller.h"
#include "ControlBehaviour.h"

namespace egocontroller {

    using namespace std;

    class JoystickController : public Controller
    {
        public:
            JoystickController(ControlBehaviour& m_behaviour, const string& device);
            virtual ~JoystickController();

            void doWork();
            hesperia::data::environment::EgoState getEgoState();

        protected:
            ControlBehaviour& m_behaviour;
            double MAX;
            double FACTOR_ACCELERATION;
            double FACTOR_ROTATION;

            int m_joy_fd;
            int* m_axes;
            double m_lastAxis0;
            double m_lastAxis1;

        private:
            JoystickController(const JoystickController&);
            JoystickController& operator=(const JoystickController&);
    };
}

#endif // EGOCONTROLLER_CONTROLLER_H_
