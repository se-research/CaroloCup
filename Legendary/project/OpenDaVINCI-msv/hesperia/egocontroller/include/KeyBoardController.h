/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef EGOCONTROLLER_KEABOARDCONTROLLER_H_
#define EGOCONTROLLER_KEABOARDCONTROLLER_H_

#include "Controller.h"
#include "ControlBehaviour.h"

namespace egocontroller {

    using namespace std;

    class KeyboardController : public Controller
    {
        public:
            KeyboardController(
                    ControlBehaviour& behaviour,
                    const char& keyAcc,
                    const char& keyBrake,
                    const char& keyLeft,
                    const char& keyRight,
                    const char& keyStop);

            virtual ~KeyboardController();

            virtual void doWork();
            virtual hesperia::data::environment::EgoState getEgoState();

        private:
            ControlBehaviour& m_behaviour;
            char m_keyAcc;
            char m_keyBrake;
            char m_keyLeft;
            char m_keyRight;
            char m_keyStop;
    };
}

#endif // EGOCONTROLLER_KEABOARDCONTROLLER_H_
