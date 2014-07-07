/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MOCKS__CONTAINERLISTENERMOCK_H
#define MOCKS__CONTAINERLISTENERMOCK_H

#include "FunctionCallWaiter.h"
#include "ParameterValueList.h"
#include "core/data/Container.h"
#include "core/io/ContainerListener.h"

namespace mocks {

    class ContainerListenerMock : public core::io::ContainerListener {
        public:
            ContainerListenerMock() :
                VALUES_nextContainer(),
                CALLWAITER_nextContainer(),
                currentValue()
            {};

            virtual void nextContainer(core::data::Container &c) {
                currentValue = c;
                CALLWAITER_nextContainer.called();
            }

            ParameterValueList< core::data::Container > VALUES_nextContainer;
            FunctionCallWaiter CALLWAITER_nextContainer;
            core::data::Container currentValue;
    };
}
#endif // MOCKS__CONTAINERLISTENERMOCK_H
