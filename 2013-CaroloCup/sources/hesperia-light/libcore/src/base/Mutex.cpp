/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Mutex.h"
#include "core/wrapper/MutexFactory.h"

namespace core {
    namespace base {

        Mutex::Mutex() :
                m_mutex() {
            m_mutex = SharedPointer<wrapper::Mutex>(wrapper::MutexFactory::getInstance().createMutex());
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
