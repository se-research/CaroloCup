/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"

namespace core {
    namespace base {

        Lock::Lock(Mutex &mutex) :
                m_mutex(mutex) {
            m_mutex.lock();
        }

        Lock::~Lock() {
            m_mutex.unlock();
        }

    }
} // core::base
