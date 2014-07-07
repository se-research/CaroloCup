/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MOCKS__STRINGLISTENERMOCK_H
#define MOCKS__STRINGLISTENERMOCK_H

#include <string>

#include "FunctionCallWaiter.h"
#include "ParameterValueList.h"
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
                return ( currentValue == VALUES_nextString.getCurrentItem() );
            }

            FunctionCallWaiter CALLWAITER_nextString;
            ParameterValueList< std::string > VALUES_nextString;
        private:
            std::string currentValue;
    };
}
#endif
