/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Mutex.h"
#include "core/wrapper/MutexFactory.h"

namespace core {
    namespace base {

        Mutex::Mutex() :
                m_mutex() {
            m_mutex = SharedPointer<wrapper::Mutex>(wrapper::MutexFactory::createMutex());
        }

        Mutex::~Mutex() {}

        void Mutex::lock() {
            m_mutex->lock();
        }

        bool Mutex::tryLock() {
            return m_mutex->tryLock();
        }

        void Mutex::unlock() {
            m_mutex->unlock();
        }

    }
} // core::base
