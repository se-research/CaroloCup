/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_STRINGPROTOCOL_H_
#define OPENDAVINCI_CORE_WRAPPER_STRINGPROTOCOL_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <sstream>

#include "core/wrapper/Mutex.h"
#include "core/wrapper/AbstractProtocol.h"
#include "core/wrapper/StringObserver.h"
#include "core/wrapper/StringSender.h"

namespace core {
    namespace wrapper {

        using namespace std;

        class OPENDAVINCI_API StringProtocol : public StringObserver, public AbstractProtocol {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                StringProtocol(const StringProtocol &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                StringProtocol& operator=(const StringProtocol &);

            public:
                /**
                 * Constructor.
                 */
                StringProtocol();

                virtual ~StringProtocol();

                /**
                 * This method sends the data in the following format:
                 *
                 * size data
                 *
                 * Size: htonl((uint32_t) data.length())
                 *
                 * @param data Data to be sent.
                 */
                void send(const string& data);

                /**
                 * This method sets the StringListener that will receive
                 * incoming data.
                 *
                 * @param listener StringListener that will receive incoming data.
                 */
                void setStringListener(StringListener* listener);

                virtual void receivedPartialString(const string &partialData);

            private:
                bool hasCompleteData();

                /**
                 * This method is used to pass received data thread-safe
                 * to the registered StringListener.
                 */
                void invokeStringListener(const string& data);

                std::auto_ptr<Mutex> m_stringListenerMutex;
                StringListener *m_stringListener;

                stringstream m_partialData;
        };
    }
}

#endif /* OPENDAVINCI_CORE_WRAPPER_STRINGPROTOCOL_H_ */

