/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIXTCPCONNECTION_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIXTCPCONNECTION_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Runnable.h"
#include "core/wrapper/TCPConnection.h"
#include "core/wrapper/Thread.h"
#include "core/wrapper/Mutex.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            class POSIXTCPConnection : public TCPConnection, public Runnable {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXTCPConnection(const POSIXTCPConnection &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXTCPConnection& operator=(const POSIXTCPConnection &);

                public:
                    POSIXTCPConnection(const int32_t fileDescriptor);

                    POSIXTCPConnection(const std::string& ip, const uint32_t port);

                    virtual ~POSIXTCPConnection();

                    virtual void sendImplementation(const std::string& data);

                    virtual void start();
                    virtual void stop();

                    virtual bool isRunning();
                    virtual void run();

                protected:
                    void initialize();

                    std::auto_ptr<Thread> m_thread;

                    std::auto_ptr<Mutex> m_socketMutex;
                    int32_t m_fileDescriptor;

                    enum {BUFFER_SIZE = 65535};
                    char m_buffer[BUFFER_SIZE];
                    std::string m_ip;
                    uint32_t m_port;
            };
        }
    }
}
#endif /* OPENDAVINCI_CORE_WRAPPER_POSIXTCPCONNECTION_H_ */
