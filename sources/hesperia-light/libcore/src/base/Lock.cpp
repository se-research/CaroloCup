/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
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
