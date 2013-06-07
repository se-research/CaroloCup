#include "cxxtest/TestSuite.h"

#include "core/mocks/FunctionCallWaiter.h"
#include "core/base/Lock.h"

namespace mocks {
    using namespace core::base;

    FunctionCallWaiter::FunctionCallWaiter():
        m_called(false),
        m_condition()
    {}

    bool FunctionCallWaiter::wait() {
        Lock lock(m_condition);
        if (!m_called) {
            return m_condition.waitOnSignalWithTimeout(2000);
        }

        return true;
    }

    bool FunctionCallWaiter::wasCalled() {
        Lock lock(m_condition);
        return m_called;
    }

    void FunctionCallWaiter::called() {
        Lock lock(m_condition);
        m_called = true;
        m_condition.wakeAll();
    }

    void FunctionCallWaiter::reset() {
        Lock lock(m_condition);
        m_called = false;
    }
}
