/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <cstring>
#include <cmath>

#include <string>

#include "core/macros.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/io/URL.h"
#include "core/wrapper/SerialPortFactory.h"
#include "core/wrapper/SerialPort.h"

#include "STM32F4Protocol.h"
#include "STM32F4Tester2.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::io;

    STM32F4Tester2::STM32F4Tester2(const int32_t &argc, char **argv) :
	    ConferenceClientModule(argc, argv, "proxy-STM32F4"),
        m_partialData(),
        m_receivedPayload()
     {}

    STM32F4Tester2::~STM32F4Tester2() {
    }

    void STM32F4Tester2::setUp() {
	    // This method will be call automatically _before_ running body().
    }

    void STM32F4Tester2::tearDown() {
	    // This method will be call automatically _after_ return from body().
    }

    void STM32F4Tester2::handleConnectionError() {
        cout << "STM32F4Tester2: TODO: Handle connection error here." << endl;
    }

    void STM32F4Tester2::receivedPartialString(const string &s) {
        m_partialData.write(s.c_str(), s.length());

        cout << "Received: '" << s << "', buffer: '" << m_partialData.str() << "'" << endl;

        decodeNetstring();

        // Put the write pointer to the end of the stream.
        m_partialData.seekp(0, ios_base::end);
    }

    void STM32F4Tester2::decodeNetstring(void) {
        // Netstrings have the following format:
        // ASCII Number representing the length of the payload + ':' + payload + ','

        m_partialData.seekg(0, ios_base::beg);

        // Start decoding only if we have received enough data.
        while(m_partialData.str().length() > 3) {
            unsigned int lengthOld = m_partialData.str().length();
            const string &s = m_partialData.str();
            const char *receiveBuffer = s.c_str();
            char *colonSign = NULL;
            unsigned int lengthOfPayload = strtol(receiveBuffer, &colonSign, 10);
            if (lengthOfPayload == 0) {
                // Remove empty Netstring: 0:, (size is 3).

                m_partialData.str(m_partialData.str().substr(3));
                continue; // Try to decode next Netstring if any.
            }

            if (*colonSign == 0x3a) {
                // Found colon sign. 

                // First, check if the buffer is as long as it is stated in the netstring.
                if (m_partialData.str().length() < lengthOfPayload) {
                    // Received data is too short. Skip further processing this part.
                    break;
                }

                // Now, check if (receiveBuffer + 1 + lengthOfPayload) == ','.
                if ((colonSign[1 + lengthOfPayload]) == 0x2c) {
                    // Successfully found a complete Netstring.
                    m_receivedPayload = m_partialData.str().substr((colonSign + 1) - receiveBuffer, lengthOfPayload);

                    // Remove decoded Netstring: "<lengthOfPayload> : <payload> ,"
                    int lengthOfNetstring = (colonSign + 1 + lengthOfPayload + 1) - receiveBuffer;

                    m_partialData.str(m_partialData.str().substr(lengthOfNetstring));
                }
            }

            if (lengthOld == m_partialData.str().length()) {
                // This should not happen, received data might be corrupt.

                // Reset buffer.
                m_partialData.str("");
            }
        }    
    }

    string STM32F4Tester2::encodeNetstring(const string &d) {
        stringstream netstring;
        if (d.length() > 0) {
            netstring << (int)d.length() << ":" << d << ",";
        }
        else {
            netstring << "0:,";
        }
        return netstring.str();
    }

    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE STM32F4Tester2::body() {
        // Get configuration data.
        KeyValueConfiguration kv = getKeyValueConfiguration();

        const URL u(kv.getValue<string>("proxy-STM32F4.serial_port"));
        const string SERIAL_PORT = u.getResource();
        const uint32_t SERIAL_SPEED = kv.getValue<uint32_t>("proxy-STM32F4.serial_speed");

        cerr << "STM32F4Tester2: Connecting to port " << SERIAL_PORT << "@" << SERIAL_SPEED << endl;

        // Open serial port.
        core::wrapper::SerialPort *serialPort = core::wrapper::SerialPortFactory::createSerialPort(SERIAL_PORT, SERIAL_SPEED);
        serialPort->setPartialStringReceiver(this);

        // Start receiving.
        serialPort->start();

        string input;
        while (getModuleState() == ModuleState::RUNNING) {
            cout << "Type a formula in RPN (e.g. 4 5 +):" << endl;
            getline(cin, input);

            string netstring = encodeNetstring(input);

            cout << "Sending '" << netstring << "'" << endl;
            serialPort->send(netstring);

            cout << "Receive buffer '" << m_receivedPayload << "'" << endl;
        }

        // Stop receiving.
        serialPort->stop();

        // Destroy connections to UDP_Server.
        OPENDAVINCI_CORE_DELETE_POINTER(serialPort);

        return ModuleState::OKAY;
    }

} // msv

