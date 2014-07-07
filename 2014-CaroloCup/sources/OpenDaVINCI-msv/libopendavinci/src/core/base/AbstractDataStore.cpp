/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/AbstractDataStore.h"
#include "core/base/Lock.h"

namespace core {
    namespace base {

        AbstractDataStore::AbstractDataStore() :
            m_condition() {}

        AbstractDataStore::~AbstractDataStore() {}

        void AbstractDataStore::waitForData() {
            Lock l(m_condition);
            if (isEmpty()) {
                m_condition.waitOnSignal();
            }
        }

        void AbstractDataStore::wait() {
          Lock l(m_condition);
          m_condition.waitOnSignal();
        }

        void AbstractDataStore::wakeAll() {
            Lock l(m_condition);
            m_condition.wakeAll();
        }


    }
} // core::base
