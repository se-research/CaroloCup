/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/Boost/BoostThread.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            using namespace std;

            BoostThread::BoostThread(Runnable &r) :
                    m_threadStateMutex(NULL),
                    m_threadState(INITIALIZED),
                    m_runnable(r),
                    m_boostThread(NULL),
                    m_threadWrapper(r) {
                // Create mutex.
                m_threadStateMutex = MutexFactory::createMutex();
                if (m_threadStateMutex == NULL) {
                    stringstream s;
                    s << "Error while creating mutex at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }
            }

            BoostThread::~BoostThread() {
                stop();

                if (m_boostThread != NULL) {
                    delete m_boostThread;
                }
                m_boostThread = NULL;

                if (m_threadStateMutex != NULL) {
                    delete m_threadStateMutex;
                }
                m_threadStateMutex = NULL;
            }

            bool BoostThread::start() {
                m_threadStateMutex->lock();
                {
                    if (m_threadState == INITIALIZED) {
                        m_boostThread = new ::boost::thread(m_threadWrapper);
                        if (m_boostThread != NULL) {
                            m_threadState = RUNNING;
                        }
                    }
                }
                m_threadStateMutex->unlock();

                return isRunning();
            }

            bool BoostThread::stop() {
                if (isRunning()) {
                    m_threadStateMutex->lock();
                    m_threadState = STOPPED;
                    m_threadStateMutex->unlock();

                    if (m_boostThread != NULL) {
                        m_boostThread->join();
                    }
                }

                return !isRunning();
            }

            bool BoostThread::isRunning() const {
                bool retVal = false;
                m_threadStateMutex->lock();
                {
                    retVal = (m_threadState == RUNNING);
                }
                m_threadStateMutex->unlock();

                return retVal;
            }

            BoostThread::ThreadWrapper::ThreadWrapper(Runnable &runnable) :
                    m_runnable(runnable) {}

            BoostThread::ThreadWrapper::ThreadWrapper(const ThreadWrapper &obj) :
                    m_runnable(obj.m_runnable) {}

            void BoostThread::ThreadWrapper::operator()() {
                try {
                    m_runnable.run();
                } catch (string &s) {
                    clog << "Exception caught at " << __FILE__ << ":" << __LINE__ << ": " << s << endl;
                    throw;
                } catch (...) {
                    clog << "Unknown exception caught at " << __FILE__ << ":" << __LINE__ << "." << endl;
                    throw;
                }
            }

        }
    }
} // core::wrapper::Boost
