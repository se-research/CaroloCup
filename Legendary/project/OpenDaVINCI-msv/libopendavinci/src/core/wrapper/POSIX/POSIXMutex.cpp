/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/POSIX/POSIXMutex.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            using namespace std;

            POSIXMutex::POSIXMutex() : m_mutex() {
                // Initialize mutex based on pthread.
                if (pthread_mutex_init(&m_mutex, NULL) != 0) {
                    stringstream s;
                    s << "Error while creating mutex at " << __FILE__ << ": " << __LINE__ << ": " << strerror(errno);
                    throw s.str();
                }
            }

            POSIXMutex::~POSIXMutex() {
                pthread_mutex_destroy(&m_mutex);
            }

            void POSIXMutex::lock() {
                pthread_mutex_lock(&m_mutex);
            }

            bool POSIXMutex::tryLock() {
                return (pthread_mutex_trylock(&m_mutex) != EBUSY);
            }

            void POSIXMutex::unlock() {
                pthread_mutex_unlock(&m_mutex);
            }

            pthread_mutex_t& POSIXMutex::getNativeMutex() {
                return m_mutex;
            }
        }
    }
} // core::wrapper::POSIX
