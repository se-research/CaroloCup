/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/Boost/BoostCondition.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            BoostCondition::BoostCondition() : m_condition(), m_mutex() {}

            BoostCondition::~BoostCondition() {
                // The following statements are necessary to avoid BOOST_VERIFY(...) errors! DO NOT REMOVE THEM!
                tryLock();
                unlock();
            }

            void BoostCondition::waitOnSignal() {
                m_condition.wait(m_mutex.getNativeMutex());
            }

            bool BoostCondition::waitOnSignalWithTimeout(const unsigned long ms) {
                boost::system_time timeout = boost::get_system_time() +
                                             boost::posix_time::milliseconds(ms);

                return m_condition.timed_wait(m_mutex.getNativeMutex(), timeout);
            }

            void BoostCondition::wakeOne() {
                m_condition.notify_one();
            }

            void BoostCondition::wakeAll() {
                m_condition.notify_all();
            }

            void BoostCondition::lock() {
                m_mutex.lock();
            }

            bool BoostCondition::tryLock() {
                return m_mutex.tryLock();
            }

            void BoostCondition::unlock() {
                m_mutex.unlock();
            }
        }
    }
} // core::wrapper::Boost
