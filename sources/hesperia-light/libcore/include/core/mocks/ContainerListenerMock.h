/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MOCKS__CONTAINERLISTENERMOCK_H
#define MOCKS__CONTAINERLISTENERMOCK_H

#include "core/mocks/FunctionCallWaiter.h"
#include "core/mocks/ParameterValueList.h"
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
