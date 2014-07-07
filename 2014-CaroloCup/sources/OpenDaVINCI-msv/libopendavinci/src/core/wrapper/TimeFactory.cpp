/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/TimeFactory.h"

namespace core {
    namespace wrapper {

        // Set up TimeFactory that can be exchanged on runtime.
        TimeFactory* TimeFactory::instance = NULL;
        TimeFactory* TimeFactory::controlledInstance = NULL;
        Mutex* TimeFactory::m_singletonMutex = MutexFactory::createMutex();

        SystemTimeFactory::worker_type SystemTimeFactory::instance = SystemTimeFactory::worker_type();
        
        TimeFactory::TimeFactory() {
            if (TimeFactory::instance == NULL) {
                TimeFactory::instance = this;
            }
        }

        TimeFactory& TimeFactory::getInstance() {
        	TimeFactory::m_singletonMutex->lock();
            if (TimeFactory::instance == NULL) {
                TimeFactory::instance = new TimeFactory();
            }
            TimeFactory::m_singletonMutex->unlock();

            if (TimeFactory::controlledInstance != NULL) {
                return *(TimeFactory::controlledInstance);
            }

            return *(TimeFactory::instance);
        }

        Time* TimeFactory::now() {
        	Time *t = NULL;
        	TimeFactory::m_singletonMutex->lock();
            if (TimeFactory::controlledInstance == NULL) {
                t = SystemTimeFactory::getInstance().now();
            }
            TimeFactory::m_singletonMutex->unlock();

            // Otherwise, use this time factory (might be overridden in sub classes).
			if (t == NULL) {
				t = TimeFactory::getInstance().now();
			}
            return t;
        }

        void TimeFactory::setSingleton(TimeFactory *tf) {
        	TimeFactory::m_singletonMutex->lock();
            	TimeFactory::controlledInstance = tf;
            TimeFactory::m_singletonMutex->unlock();
        }  

    }
} // core::wrapper
