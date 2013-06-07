/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "context/base/ControlledTimeFactory.h"

namespace context {
    namespace base {

        using namespace core::base;

        ControlledTimeFactory::ControlledTimeFactory() :
            m_timeMutex(),
            m_time() {
            core::wrapper::TimeFactory::setSingleton(this);
        }

        ControlledTimeFactory::~ControlledTimeFactory() {}

        core::wrapper::Time* ControlledTimeFactory::now() {
            core::wrapper::Time *t = NULL;
            {
                Lock l(m_timeMutex);
                t = new ControlledTime(m_time);
            }
            return t;
        }

        void ControlledTimeFactory::setTime(const ControlledTime &ct) {
            Lock l(m_timeMutex);
            m_time.setSeconds(ct.getSeconds());
            m_time.setPartialMicroseconds(ct.getPartialMicroseconds());
        }

    }
} // context::base
