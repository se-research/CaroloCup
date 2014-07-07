/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Condition.h"
#include "core/wrapper/ConditionFactory.h"

namespace core {
    namespace base {

        Condition::Condition() :
                m_condition() {
            m_condition = SharedPointer<wrapper::Condition>(wrapper::ConditionFactory::createCondition());
        }

        Condition::~Condition() {}

        void Condition::waitOnSignal() {
            assert(!m_condition->tryLock());
            m_condition->waitOnSignal();
        }

        bool Condition::waitOnSignalWithTimeout(const unsigned long& ms) {
            assert(!m_condition->tryLock());
            return m_condition->waitOnSignalWithTimeout(ms);
        }

        void Condition::wakeOne() {
            assert(!m_condition->tryLock());
            m_condition->wakeOne();
        }

        void Condition::wakeAll() {
            assert(!m_condition->tryLock());
            m_condition->wakeAll();
        }

        void Condition::lock() {
            m_condition->lock();
        }

        bool Condition::tryLock() {
            return m_condition->tryLock();
        }

        void Condition::unlock() {
            m_condition->unlock();
        }

    }
} // core::base
