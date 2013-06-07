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
#include "STM32F4Tester.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::io;

    STM32F4Tester::STM32F4Tester(const int32_t &argc, char **argv) :
	    ConferenceClientModule(argc, argv, "proxy-STM32F4") {}

    STM32F4Tester::~STM32F4Tester() {
    }

    void STM32F4Tester::setUp() {
	    // This method will be call automatically _before_ running body().
    }

    void STM32F4Tester::tearDown() {
	    // This method will be call automatically _after_ return from body().
    }

    void STM32F4Tester::handleConnectionError() {
        cout << "STM32F4Tester: TODO: Handle connection error here." << endl;
    }

    void STM32F4Tester::nextMeasurement(const vector<InfraredSensorMeasurement> &measurement) {
        vector<InfraredSensorMeasurement> l = measurement;
        vector<InfraredSensorMeasurement>::iterator it = l.begin();
        while (it != l.end()) {
            InfraredSensorMeasurement m = (*it++);
            cout << "STM32F4Tester: " << m.address << ": " << m.value << endl;
        }
    }

    void STM32F4Tester::nextMeasurement(const vector<UltrasonicSensorMeasurement> &measurement) {
        vector<UltrasonicSensorMeasurement> l = measurement;
        vector<UltrasonicSensorMeasurement>::iterator it = l.begin();
        while (it != l.end()) {
            UltrasonicSensorMeasurement m = (*it++);
            cout << "STM32F4Tester: " << m.address << ": " << m.value << endl;
        }
    }

    void STM32F4Tester::nextMeasurement(const RazorMeasurement &/*measurement*/) {
        cout << "STM32F4Tester: Received Razor measurement: " << endl;
    }

    void STM32F4Tester::nextMeasurement(const STM32F4AccelerometerMeasurement &/*measurement*/) {
        cout << "STM32F4Tester: Received STM32F4 accelerometer measurement: " << endl;
    }

    void STM32F4Tester::nextMeasurement(const core::data::environment::VehicleData &/*measurement*/) {
        cout << "STM32F4Tester: Received STM32F4 IMU algorithms (VehicleData) measurement: " << endl;
    }

    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE STM32F4Tester::body() {
        // Get configuration data.
        KeyValueConfiguration kv = getKeyValueConfiguration();

        const URL u(kv.getValue<string>("proxy-STM32F4.serial_port"));
        const string SERIAL_PORT = u.getResource();
        const uint32_t SERIAL_SPEED = kv.getValue<uint32_t>("proxy-STM32F4.serial_speed");

        cerr << "STM32F4Tester: Connecting to port " << SERIAL_PORT << "@" << SERIAL_SPEED << endl;

        // Open serial port.
        core::wrapper::SerialPort *serialPort = core::wrapper::SerialPortFactory::createSerialPort(SERIAL_PORT, SERIAL_SPEED);

        STM32F4Protocol p;
        p.setStringSender(serialPort);
        p.setSTM32F4DataListener(this);
        serialPort->setPartialStringReceiver(&p);

        // Start receiving.
        serialPort->start();

        uint32_t counter = 0;
        double degree = -28;
	int step = 1;
        while (getModuleState() == ModuleState::RUNNING) {
            counter++;
            if (counter == 1) {
                p.request(STM32F4Protocol::AllInfrareds, 0, degree);
            }
            if (counter == 2) {
                p.request(STM32F4Protocol::AllUltrasonics, 1, degree);
            }
            if (counter == 3) {
                p.request(STM32F4Protocol::YawData, 2, degree);
            }
            if (counter == 4) {
                p.request(STM32F4Protocol::Magnetometer, 3, degree);
            }
            if (counter == 5) {
                p.request(STM32F4Protocol::Gyroscope, 4, degree);
            }
            if (counter == 6) {
                p.request(STM32F4Protocol::AccelerometerRazor, 5,degree);
            }
            if (counter == 7) {
                p.request(STM32F4Protocol::AccelerometerSTM32, 6, degree);
            }
            if (counter == 8) {
                p.request(STM32F4Protocol::RazorAll, 7, degree);
            }
            if (counter == 9) {
                p.request(STM32F4Protocol::RazorAllSTM32,8,degree);
            }
            if (counter == 10) {
                p.request(STM32F4Protocol::CurrentPosition, 0, degree);
            }
            if (counter == 11) {
                p.request(STM32F4Protocol::TraveledPath, 0, degree);
            }
            if (counter == 12) {
                p.request(STM32F4Protocol::Velocity, 0, degree);
            }
            if (counter == 13) {
                p.request(STM32F4Protocol::Orientation, 0, degree);
            }

            if(degree > 28)
            	step=-1;
            if(degree < -28) 
		step=1;
	    degree+=step;
            if (counter > 13) counter = 0;
        }

        // Stop receiving.
        serialPort->stop();

        // Destroy connections to UDP_Server.
        OPENDAVINCI_CORE_DELETE_POINTER(serialPort);

        return ModuleState::OKAY;
    }

} // msv

