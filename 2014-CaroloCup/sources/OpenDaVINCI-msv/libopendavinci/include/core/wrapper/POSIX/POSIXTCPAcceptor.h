/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIXTCPACCEPTOR_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIXTCPACCEPTOR_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Runnable.h"
#include "core/wrapper/TCPAcceptor.h"
#include "core/wrapper/Thread.h"
#include "core/wrapper/Mutex.h"

namespace core {
    namespace wrapper {
        namespace POSIX {
            class POSIXTCPAcceptor : public TCPAcceptor, public Runnable {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXTCPAcceptor(const POSIXTCPAcceptor &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXTCPAcceptor& operator=(const POSIXTCPAcceptor &);

                public:
                    POSIXTCPAcceptor(const uint32_t& port);
                    virtual ~POSIXTCPAcceptor();

                    virtual void setAcceptorListener(TCPAcceptorListener* listener);

                    virtual void start();
                    virtual void stop();

                    virtual bool isRunning();
                    virtual void run();

                protected:
                    void invokeAcceptorListener(TCPConnection* connection);
                    static const int32_t BACKLOG = 100;

                    Thread *m_thread;
                    Mutex* m_listenerMutex;
                    TCPAcceptorListener* m_listener;

                    int32_t m_fileDescriptor;
                    int32_t m_port;
            };
        }
    }
}
#endif /* OPENDAVINCI_CORE_WRAPPER_POSIXTCPACCEPTOR_H_ */
