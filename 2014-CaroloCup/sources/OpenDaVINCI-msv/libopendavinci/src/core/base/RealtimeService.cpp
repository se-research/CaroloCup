/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/base/RealtimeService.h"
#include "core/wrapper/ConcurrencyFactory.h"

namespace core {
    namespace base {

        using namespace exceptions;

        RealtimeService::RealtimeService(const enum PERIOD &period) throw (ThreadException) :
                RealtimeRunnable(period),
                m_thread(),
                m_realtimeServiceStateMutex(),
                m_realtimeServiceState(INITIALIZED) {
            m_thread = SharedPointer<wrapper::Thread>(wrapper::ConcurrencyFactory::createThread(*this));
            if (!m_thread.isValid()) {
                OPENDAVINCI_CORE_THROW_EXCEPTION(ThreadException, "Thread could not be created!");
            }
        }

        RealtimeService::~RealtimeService() {
            stop();
        }

        void RealtimeService::start() {
            Lock l(m_realtimeServiceStateMutex);
            if (m_realtimeServiceState == INITIALIZED) {
                m_realtimeServiceState = RUNNING;
                m_thread->start();
            }
        }

        void RealtimeService::stop() {
            bool doStop = false;
            {
                Lock l(m_realtimeServiceStateMutex);
                if (m_realtimeServiceState == RUNNING) {
                    m_realtimeServiceState = STOPPED;
                    doStop = true;
                }
            }
            if (doStop) {
                m_thread->stop();
            }
        }

        bool RealtimeService::isRunning() {
            Lock l(m_realtimeServiceStateMutex);
            return (m_realtimeServiceState == RUNNING);
        }

    }
} // core::base
