/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

// Using c++11 standard.
#include <chrono>

#include "core/wrapper/WIN32/WIN32Condition.h"

namespace core {
    namespace wrapper {
        namespace WIN32Impl {

            using namespace std;

            WIN32Condition::WIN32Condition() : m_condition(), m_mutex() {}

            WIN32Condition::~WIN32Condition() {}

            void WIN32Condition::waitOnSignal() {
				// Adopt the lock so that the condition can wait on this native mutex.
                std::unique_lock<std::mutex> l(m_mutex.getNativeMutex(), std::adopt_lock);
                m_condition.wait(l);
				// Release the lock (without unlocking it!) so that the outer scope receives the responsibility to unlock the mutex.
				// Thus, the existing semantics which is compliant with the POSIX implementation is preserved.
				l.release();
            }

            bool WIN32Condition::waitOnSignalWithTimeout(const unsigned long ms) {
				// Adopt the lock so that the condition can wait on this native mutex.
				std::unique_lock<std::mutex> l(m_mutex.getNativeMutex(), std::adopt_lock);
                bool retVal = m_condition.wait_for(l, std::chrono::milliseconds(ms)) == std::cv_status::no_timeout;
				// Release the lock (without unlocking it!) so that the outer scope receives the responsibility to unlock the mutex.
				// Thus, the existing semantics which is compliant with the POSIX implementation is preserved.
				l.release();
				return retVal;
            }

            void WIN32Condition::wakeOne() {
                m_condition.notify_one();
            }

            void WIN32Condition::wakeAll() {
                m_condition.notify_all();
            }

            void WIN32Condition::lock() {
				m_mutex.lock();
            }

            bool WIN32Condition::tryLock() {
				return m_mutex.tryLock();
            }

            void WIN32Condition::unlock() {
				m_mutex.unlock();
			}

        }
    }
} // core::wrapper::WIN32Impl
