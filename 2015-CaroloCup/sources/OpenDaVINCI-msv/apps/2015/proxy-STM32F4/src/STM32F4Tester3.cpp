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
#include "STM32F4Tester3.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::io;

    STM32F4Tester3::STM32F4Tester3(const int32_t &argc, char **argv) :
	    ConferenceClientModule(argc, argv, "proxy-STM32F4"),
        m_netstringsProtocol()
     {}

    STM32F4Tester3::~STM32F4Tester3() {
    }

    void STM32F4Tester3::setUp() {
	    // This method will be call automatically _before_ running body().
    }

    void STM32F4Tester3::tearDown() {
	    // This method will be call automatically _after_ return from body().
    }

    void STM32F4Tester3::handleConnectionError() {
        cout << "STM32F4Tester3: TODO: Handle connection error here." << endl;
    }

    void STM32F4Tester3::nextString(const string &s) {
        cout << "Successfully received: " << s << endl;
    }

    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE STM32F4Tester3::body() {
        // Get configuration data.
        KeyValueConfiguration kv = getKeyValueConfiguration();

        const URL u(kv.getValue<string>("proxy-STM32F4.serial_port"));
        const string SERIAL_PORT = u.getResource();
        const uint32_t SERIAL_SPEED = kv.getValue<uint32_t>("proxy-STM32F4.serial_speed");

        cerr << "STM32F4Tester3: Connecting to port " << SERIAL_PORT << "@" << SERIAL_SPEED << endl;

        // This is the listener for any received strings.
        m_netstringsProtocol.setStringListener(this);

        // Open serial port.
        core::wrapper::SerialPort *serialPort = core::wrapper::SerialPortFactory::createSerialPort(SERIAL_PORT, SERIAL_SPEED);
        serialPort->setPartialStringReceiver(&m_netstringsProtocol);

        // Start receiving.
        serialPort->start();

        while (getModuleState() == ModuleState::RUNNING) {
        }

        // Stop receiving.
        serialPort->stop();

        // Destroy connections to UDP_Server.
        OPENDAVINCI_CORE_DELETE_POINTER(serialPort);

        return ModuleState::OKAY;
    }

} // msv

