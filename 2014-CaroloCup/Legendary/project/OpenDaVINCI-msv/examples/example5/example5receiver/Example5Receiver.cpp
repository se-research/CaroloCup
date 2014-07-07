/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

#include "core/macros.h"
#include "core/base/Lock.h"
#include "core/wrapper/SerialPortFactory.h"
#include "core/wrapper/SerialPort.h"
#include "core/wrapper/StringProtocol.h"

#include "Example5Receiver.h"

namespace examples {

    using namespace std;
    using namespace core::base;

    Example5Receiver::Example5Receiver(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "Example5Receiver"),
        m_bufferMutex(),
        m_buffer("")
	    {}

    Example5Receiver::~Example5Receiver() {}

    void Example5Receiver::setUp() {}

    void Example5Receiver::tearDown() {}

    void Example5Receiver::handleConnectionError() {
        cout << "Example5Receiver: connection error.'" << endl;
    }

    void Example5Receiver::nextString(const string &s) {
        cout << "Example5Receiver: '" << s << "'" << endl;
        {
            Lock l(m_bufferMutex);
            m_buffer = s;
        }
    }

    ModuleState::MODULE_EXITCODE Example5Receiver::body() {
        // Create SerialPort.
        core::wrapper::SerialPort *serialPort = core::wrapper::SerialPortFactory::createSerialPort("/dev/pts/3", 115200);

        core::wrapper::StringProtocol sp;
        sp.setStringListener(this); // Example5Receiver shall receive completely received strings from the StringProtocol.
        sp.setStringSender(serialPort); // The StringProtocol will use the SerialPort to send data.
        serialPort->setPartialStringReceiver(&sp); // The SerialPort will distribute partially received data to StringProtocol.

        // Start receiving.
        serialPort->start();

    	while (getModuleState() == ModuleState::RUNNING) {
            cout << "Example5Receiver: Do something..." << endl;

            {
                Lock l(m_bufferMutex);
                sp.send(m_buffer);
                m_buffer = "";
            }
        }

        // Destroy connections to UDP_Server.
        OPENDAVINCI_CORE_DELETE_POINTER(serialPort);

    	return ModuleState::OKAY;
    }

} // examples
