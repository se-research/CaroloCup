/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MOCKS__STRINGLISTENERMOCK_H
#define MOCKS__STRINGLISTENERMOCK_H

#include <string>
#include <iostream>

#include "core/mocks/FunctionCallWaiter.h"
#include "core/mocks/ParameterValueList.h"
#include "core/wrapper/StringListener.h"


namespace mocks {

    class StringListenerMock : public core::wrapper::StringListener {
        public:
            StringListenerMock() :
                CALLWAITER_nextString(),
                VALUES_nextString(),
                currentValue()
            {}

            virtual void nextString(const std::string &s) {
                currentValue = s;
                CALLWAITER_nextString.called();
            }

            bool correctCalled() {
                std::string value = VALUES_nextString.getCurrentItem();
                if ( currentValue != value ) {
                    std::cerr << "Expected: [" << value << "], Got: [" << currentValue << "]" << std::endl;
                    currentValue = "";
                    return false;
                }
                currentValue = "";
                return true;
            }

            FunctionCallWaiter CALLWAITER_nextString;
            ParameterValueList< std::string > VALUES_nextString;

        private:
            std::string currentValue;
    };
}
#endif
