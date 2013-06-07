/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/POSIX/POSIXThread.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            using namespace std;

            /**
             * This method encapsulates the runnable.
             */
            void *threadRunner(void *v) {
                pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
                Runnable *runnable = static_cast<Runnable*>(v);

                try {
                    runnable->run();
                } catch (string &s) {
                    clog << "Exception caught at " << __FILE__ << ":" << __LINE__ << ": " << s << endl;
                    throw;
                } catch (...) {
                    clog << "Unknown exception caught at " << __FILE__ << ":" << __LINE__ << "." << endl;
                    throw;
                }

                return NULL;
            }

            POSIXThread::POSIXThread(Runnable &r) :
                    m_threadStateMutex(NULL),
                    m_threadState(INITIALIZED),
                    m_runnable(r),
                    m_threadWrapper() {
                // Create mutex.
                m_threadStateMutex = MutexFactory::createMutex();
                if (m_threadStateMutex == NULL) {
                    stringstream s;
                    s << "Error while creating mutex at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }
            }

            POSIXThread::~POSIXThread() {
                stop();

                if (m_threadStateMutex != NULL) {
                    delete m_threadStateMutex;
                }
                m_threadStateMutex = NULL;
            }

            bool POSIXThread::start() {
                m_threadStateMutex->lock();
                {
                    if (m_threadState == INITIALIZED) {
                        pthread_create(&m_threadWrapper, NULL, threadRunner, &m_runnable);
                        m_threadState = RUNNING;
                    }
                }
                m_threadStateMutex->unlock();

                return isRunning();
            }

            bool POSIXThread::stop() {
                bool doJoin = false;
                m_threadStateMutex->lock();
                {
                    if (m_threadState == RUNNING) {
                        doJoin = true;
                        m_threadState = STOPPED;
                    }
                }
                m_threadStateMutex->unlock();

                if (doJoin) {
                    void *retVal = NULL;
                    pthread_join(m_threadWrapper, &retVal);
                }

                return !isRunning();
            }

            bool POSIXThread::isRunning() const {
                bool retVal = false;
                m_threadStateMutex->lock();
                {
                    retVal = (m_threadState == RUNNING);
                }
                m_threadStateMutex->unlock();

                return retVal;
            }

        }
    }
} // core::wrapper::POSIX
