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
#include "core/base/FIFOQueue.h"

#include "STM32F4Control.h"
#include "STM32F4Data.h"

#include "STM32F4Protocol.h"
#include "STM32F4Wrapper.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::io;

    STM32F4Wrapper::STM32F4Wrapper(const int32_t &argc, char **argv) :
	    ConferenceClientModule(argc, argv, "proxy-STM32F4"),
        m_netstringsProtocol()
     {}

    STM32F4Wrapper::~STM32F4Wrapper() {}

    void STM32F4Wrapper::setUp() {
	    // This method will be call automatically _before_ running body().
    }

    void STM32F4Wrapper::tearDown() {
	    // This method will be call automatically _after_ return from body().
    }

    void STM32F4Wrapper::handleConnectionError() {
        cout << "STM32F4Wrapper: TODO: Handle connection error here." << endl;
    }

    void STM32F4Wrapper::nextString(const string &s) {
        STM32F4Data d;
        d.setRawData(s);

        Container c(Container::USER_DATA_0, d); 
        getConference().send(c);
    }

    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE STM32F4Wrapper::body() {
        // Get configuration data.
        KeyValueConfiguration kv = getKeyValueConfiguration();

        const URL u(kv.getValue<string>("proxy-STM32F4.serial_port"));
        const string SERIAL_PORT = u.getResource();
        const uint32_t SERIAL_SPEED = kv.getValue<uint32_t>("proxy-STM32F4.serial_speed");

        cerr << "STM32F4Wrapper: Connecting to port " << SERIAL_PORT << "@" << SERIAL_SPEED << endl;

        // This is the listener for any received strings.
        m_netstringsProtocol.setStringListener(this);

        // Open serial port.
        core::wrapper::SerialPort *serialPort = core::wrapper::SerialPortFactory::createSerialPort(SERIAL_PORT, SERIAL_SPEED);
        serialPort->setPartialStringReceiver(&m_netstringsProtocol);

        m_netstringsProtocol.setStringSender(serialPort);

        // Start receiving.
        serialPort->start();

        // FIFO for incoming containers.
        FIFOQueue m_fifo;
    	addDataStoreFor(m_fifo);

    	while (getModuleState() == ModuleState::RUNNING) {
    		// Process received entries.
    		while (!m_fifo.isEmpty()) {
    			Container c = m_fifo.leave();

    			if (c.getDataType() == Container::USER_DATA_1) {
    				STM32F4Control control = c.getData<STM32F4Control>();

                    cout << control.toString() << endl;

                    stringstream data;
                    data << "1:" << control.getDataFeed();
                    m_netstringsProtocol.send(data.str());
    			}
    		}
        }

        // Stop receiving.
        serialPort->stop();

        // Destroy connections to UDP_Server.
        OPENDAVINCI_CORE_DELETE_POINTER(serialPort);

        return ModuleState::OKAY;
    }

} // msv

