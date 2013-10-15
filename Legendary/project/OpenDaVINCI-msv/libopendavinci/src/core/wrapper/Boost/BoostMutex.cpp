/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/Boost/BoostMutex.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            BoostMutex::BoostMutex() : m_mutex() {}

            BoostMutex::~BoostMutex() {
                // The following statements are necessary to avoid BOOST_VERIFY(...) errors! DO NOT REMOVE THEM!
                tryLock();
                unlock();
            }

            void BoostMutex::lock() {
                m_mutex.lock();
            }

            bool BoostMutex::tryLock() {
                return m_mutex.try_lock();
            }

            void BoostMutex::unlock() {
                m_mutex.unlock();
            }

            boost::try_mutex& BoostMutex::getNativeMutex() {
                return m_mutex;
            }
        }
    }
} // core::wrapper::Boost
