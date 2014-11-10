/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/WIN32/WIN32Mutex.h"

namespace core {
    namespace wrapper {
        namespace WIN32Impl {

            using namespace std;

            WIN32Mutex::WIN32Mutex() : m_mutex() {}

            WIN32Mutex::~WIN32Mutex() {}

            void WIN32Mutex::lock() {
                m_mutex.lock();
            }

            bool WIN32Mutex::tryLock() {
                return m_mutex.try_lock();
            }

            void WIN32Mutex::unlock() {
                m_mutex.unlock();
            }

            mutex& WIN32Mutex::getNativeMutex() {
                return m_mutex;
            }

        }
    }
} // core::wrapper::WIN32Impl
