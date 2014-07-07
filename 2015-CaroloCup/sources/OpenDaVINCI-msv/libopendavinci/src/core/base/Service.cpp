/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/base/Service.h"
#include "core/wrapper/ConcurrencyFactory.h"

namespace core {
    namespace base {

        using namespace exceptions;

        Service::Service() throw (ThreadException) :
                m_thread(),
                m_serviceStateMutex(),
                m_serviceState(INITIALIZED),
                m_serviceReadyCondition(),
                m_serviceReady(false) {
            m_thread = SharedPointer<wrapper::Thread>(wrapper::ConcurrencyFactory::createThread(*this));
            if (!m_thread.isValid()) {
                OPENDAVINCI_CORE_THROW_EXCEPTION(ThreadException, "Thread could not be created!");
            }
        }

        Service::~Service() {
            // Do not call this->stop() here because beforeStop() is a pure virtual method!
            m_thread->stop();
        }

        void Service::start() {
            Lock l(m_serviceStateMutex);
            if (m_serviceState == INITIALIZED) {
                m_serviceState = RUNNING;
                m_thread->start();

                // wait until new service signals readiness
                {
                    Lock ll(m_serviceReadyCondition);
                    while (!m_serviceReady) {
                        m_serviceReadyCondition.waitOnSignal();
                    }
                }
            }

        }

        void Service::stop() {
            bool doStop = false;
            {
                Lock l(m_serviceStateMutex);
                if (m_serviceState == RUNNING) {
                    m_serviceState = STOPPED;
                    doStop = true;
                }
            }
            if (doStop) {
                beforeStop();
                m_thread->stop();
            }
        }

        void Service::serviceReady() {
            Lock l(m_serviceReadyCondition);
            m_serviceReady = true;
            m_serviceReadyCondition.wakeAll();
        }

        bool Service::isRunning() {
            Lock l(m_serviceStateMutex);
            return (m_serviceState == RUNNING);
        }
    }
} // core::base
