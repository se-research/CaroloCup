/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "context/base/BlockableContainerListener.h"

namespace context {
    namespace base {

        using namespace std;
        using namespace core::base;

        BlockableContainerListener::BlockableContainerListener() :
            m_nextContainerAllowedMutex(),
            m_nextContainerAllowed(false) {}

        BlockableContainerListener::~BlockableContainerListener() {
            // Break blocking.
            setNextContainerAllowed(true);
        }

        void BlockableContainerListener::setNextContainerAllowed(const bool &allowed) {
            {
                Lock l(m_nextContainerAllowedMutex);
                m_nextContainerAllowed = allowed;
            }
        }

        bool BlockableContainerListener::isNextContainerAllowed() const {
            bool retVal = false;
            {
                Lock l(m_nextContainerAllowedMutex);
                retVal = m_nextContainerAllowed;
            }
            return retVal;
        }

    }
} // context::base
