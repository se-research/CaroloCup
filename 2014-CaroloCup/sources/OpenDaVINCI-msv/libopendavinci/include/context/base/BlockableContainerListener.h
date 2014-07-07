/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_BLOCKABLECONTAINERLISTENER_H_
#define CONTEXT_BASE_BLOCKABLECONTAINERLISTENER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/io/ContainerListener.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class provides methods for blocking a ContainerListener.
         */
        class OPENDAVINCI_API BlockableContainerListener : public core::io::ContainerListener {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                BlockableContainerListener(const BlockableContainerListener&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                BlockableContainerListener& operator=(const BlockableContainerListener&);

            public:
                BlockableContainerListener();

                virtual ~BlockableContainerListener();

                /**
                 * This method enables or disables the processing of
                 * containers using nextContainer(...).
                 *
                 * @param allowed True to allow nextContainer(...).
                 */
                void setNextContainerAllowed(const bool &allowed);

            protected:
                /**
                 * This method returns true if nextContainer(...) is allowed.
                 */
                bool isNextContainerAllowed() const;

            private:
                mutable core::base::Mutex m_nextContainerAllowedMutex;
                bool m_nextContainerAllowed;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_BLOCKABLECONTAINERLISTENER_H_*/
