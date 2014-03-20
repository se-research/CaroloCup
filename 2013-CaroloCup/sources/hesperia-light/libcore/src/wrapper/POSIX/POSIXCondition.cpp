/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>
#include <cerrno>
#include <cstring>
#include <sstream>

#include "core/wrapper/POSIX/POSIXCondition.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            using namespace std;

            POSIXCondition::POSIXCondition() : m_condition(), m_mutex() {
                // Initialize condition based on pthread.
                if (pthread_cond_init(&m_condition, NULL) != 0) {
                    stringstream s;
                    s << "Error while creating condition at " << __FILE__ << ": " << __LINE__ << ": " << strerror(errno);
                    throw s.str();
                }
            }

            POSIXCondition::~POSIXCondition() {
                pthread_cond_destroy(&m_condition);
            }

            void POSIXCondition::waitOnSignal() {
                pthread_cond_wait(&m_condition, &m_mutex.getNativeMutex());
            }

            bool POSIXCondition::waitOnSignalWithTimeout(const unsigned long ms) {
                struct timespec timeout;

                clock_gettime (CLOCK_REALTIME, &timeout);

                uint32_t seconds = 0;
                unsigned long milliseconds = ms;
                while (milliseconds >= 1000) {
                    seconds++;
                    milliseconds -= 1000;
                }
                timeout.tv_sec += seconds;
                timeout.tv_nsec += milliseconds * 1000 * 1000;

                int32_t error = pthread_cond_timedwait(&m_condition, &m_mutex.getNativeMutex(), &timeout);

                return (error == 0);
            }

            void POSIXCondition::wakeOne() {
                pthread_cond_signal(&m_condition);
            }

            void POSIXCondition::wakeAll() {
                pthread_cond_broadcast(&m_condition);
            }

            void POSIXCondition::lock() {
                m_mutex.lock();
            }

            bool POSIXCondition::tryLock() {
                return m_mutex.tryLock();
            }

            void POSIXCondition::unlock() {
                m_mutex.unlock();
            }

        }
    }
} // core::wrapper::Boost
