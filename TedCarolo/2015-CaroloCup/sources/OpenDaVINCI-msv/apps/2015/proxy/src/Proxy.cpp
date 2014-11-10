/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <ctype.h>
#include <cstring>
#include <cmath>

#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"

#include "core/base/LIFOQueue.h"

#include "core/io/ContainerConference.h"
#include "core/wrapper/SerialPortFactory.h"
#include "core/wrapper/SerialPort.h"
#include "core/wrapper/StringProtocol.h"
#include "core/data/Container.h"
#include "core/data/control/VehicleControl.h"

#include "OpenCVCamera.h"

#include "simple.pb.h"
#include "pb_encode.h"
#include "pb_decode.h"

#ifdef HAVE_UEYE
#include "uEyeCamera.h"
#endif

#include "Proxy.h"

namespace msv {

using namespace std;
using namespace core::base;
using namespace core::data;
using namespace tools::recorder;
using namespace core::data::control;

bool sendStatus;
uint8_t sendBuffer[128];

Proxy::Proxy(const int32_t &argc, char **argv) :
		ConferenceClientModule(argc, argv, "proxy"), m_recorder(NULL), m_camera(
		NULL) {
}

Proxy::~Proxy() {
}

void Proxy::setUp() {
	// This method will be call automatically _before_ running body().
	if (getFrequency() < 20) {
		cerr << endl << endl
				<< "Proxy: WARNING! Running proxy with a LOW frequency (consequence: data updates are too seldom and will influence your algorithms in a negative manner!) --> suggestions: --freq=20 or higher! Current frequency: "
				<< getFrequency() << " Hz." << endl << endl << endl;
	}

	// Get configuration data.
	KeyValueConfiguration kv = getKeyValueConfiguration();

	// Create built-in recorder.

}

void Proxy::tearDown() {
	// This method will be call automatically _after_ return from body().

}

void Proxy::nextString(const string &s) {
	cout << "proxy '" << s << "'" << endl;
}

void Proxy::handleConnectionError() {
	cout << "Proxy: connection error.'" << endl;
}

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE Proxy::body() {

	core::base::LIFOQueue lifo;
	addDataStoreFor(lifo);

	core::wrapper::SerialPort *serialPort1 =
			core::wrapper::SerialPortFactory::createSerialPort("/dev/ttyACM1",
					115200);
	serialPort1->setConnectionListener(this);

	core::wrapper::StringProtocol sp1;
	//sp1.setStringListener(this); // Example5Sender shall receive completely received strings from the StringProtocol.
	sp1.setStringSender(serialPort1); // The StringProtocol will use the SerialPort to send data.
	serialPort1->setPartialStringReceiver(&sp1); // The SerialPort will distribute partially received data to StringProtocol.

	// Start receiving.
	serialPort1->start();

	/*core::wrapper::SerialPort *serialPort2 = core::wrapper::SerialPortFactory::createSerialPort("/dev/ttyACM1", 115200);
	 serialPort2->setConnectionListener(this);

	 core::wrapper::StringProtocol sp2;
	 sp2.setStringListener(this); // Example5Sender shall receive completely received strings from the StringProtocol.
	 sp2.setStringSender(serialPort2); // The StringProtocol will use the SerialPort to send data.
	 serialPort2->setPartialStringReceiver(&sp2); // The SerialPort will distribute partially received data to StringProtocol.

	 // Start receiving.
	 serialPort2->start();*/

	while (getModuleState() == ModuleState::RUNNING) {
		VehicleControl vc;
		// Regular data processing: try to find matching containers.
		while (!lifo.isEmpty()) {
			// Read next received container.
			Container con = lifo.pop();

			if (con.getDataType() == Container::VEHICLECONTROL) {
				vc = con.getData<VehicleControl>();
				double speed = vc.getSpeed();
				double steeringAngle = vc.getSteeringWheelAngle();
				bool brakeLight = vc.getBrakeLights();
				bool leftFlash = vc.getLeftFlashingLights();
				bool rightFlash = vc.getRightFlashingLights();

				udooToArduino sm;
				pb_ostream_t sendStream = pb_ostream_from_buffer(sendBuffer,
						sizeof(sendBuffer));
				sm.speed = speed;
				sm.steering = steeringAngle;
				sm.leftBlink = leftFlash; //turnLeft;
				sm.rightBlink = rightFlash; //turnRight;
				sm.stopLight = brakeLight; //breakLed;

				sendStatus = pb_encode(&sendStream, udooToArduino_fields, &sm);
				if (sendStatus) {
					cout << "BYTES WRITTEN: " << (int) sendStream.bytes_written
							<< endl;
					cout << "MY MESSAGE: ";
					ostringstream message;
					for (int i = 0; i < (int) sendStream.bytes_written; i++) {
						message << sendBuffer[i];
						cout << (int) sendBuffer[i] << ',';
					}
					cout << message.str();
					 sp1.send(encodeNetstring(message.str()));
				}
			}

		}

		// Destroy connections to UDP_Server.
		OPENDAVINCI_CORE_DELETE_POINTER(serialPort1);
		// OPENDAVINCI_CORE_DELETE_POINTER(serialPort2);

	}
	return ModuleState::OKAY;
}
string Proxy::encodeNetstring(const string &d) {
	stringstream netstring;
	if (d.length() > 0) {
		netstring << (int) d.length() << ":" << d << ",";
	} else {
		netstring << "0:,";
	}
	return netstring.str();
}
} // msv

