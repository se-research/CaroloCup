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
#include "libsend.pb.h"
#include "libreceive.pb.h"

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
uint8_t receivePBuffer[128];
int receiveMessage_length;
bool receiveStatus;
arduinoToUdoo rm;

Proxy::Proxy(const int32_t &argc, char **argv) :
		ConferenceClientModule(argc, argv, "proxy"), m_recorder(NULL), m_camera(
		NULL), m_sensorBoardData() {
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

	for (int i = 0; i < (int) s.length(); i++) {
		receivePBuffer[i] = s[i];
	}
	receiveMessage_length = s.length();

	pb_istream_t stream = pb_istream_from_buffer(receivePBuffer,
			receiveMessage_length);

	receiveStatus = pb_decode(&stream, arduinoToUdoo_fields, &rm);
	if (receiveStatus) {
//			      us_left = rm.us_left;
//			      us_center = rm.us_center;
//			      us_right = rm.us_right;
//			      ir_left = rm.ir_left;
//			      ir_right_front = rm.ir_right_front;
//			      ir_right_back = rm.ir_right_back;
		m_sensorBoardData.update(0, (int16_t) rm.us_left);
		m_sensorBoardData.update(1, (int16_t) rm.us_left);
		Container c(Container::USER_DATA_0, m_sensorBoardData);
		getConference().send(c);
	}

}

void Proxy::handleConnectionError() {
	cout << "Proxy: connection error.'" << endl;
}

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE Proxy::body() {

	core::base::LIFOQueue lifo;
	addDataStoreFor(lifo);

	core::wrapper::SerialPort *serialPort1 =
			core::wrapper::SerialPortFactory::createSerialPort("/dev/ttyACM0",
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

	// Initialize m_sensorBoardData data structure for this sensor.
	m_sensorBoardData.update(0, -1);  //Change values to #define later
	m_sensorBoardData.update(2, -1);

	/////////end sensorBoardData initialization

	while (getModuleState() == ModuleState::RUNNING) {
		VehicleControl vc;

		// Regular data processing: try to find matching containers.
		while (true/*!lifo.isEmpty()*/) {
			// Read next received container.
			//	Container con = lifo.pop();

			if (true/*con.getDataType() == Container::VEHICLECONTROL*/) {
				//	vc = con.getData<VehicleControl>();
				double speed = 10;			//.getSpeed();
				double steeringAngle = 10;		//vc.getSteeringWheelAngle();
				bool brakeLight = true;			//vc.getBrakeLights();
				bool leftFlash = true;			//vc.getLeftFlashingLights();
				bool rightFlash = false;		//vc.getRightFlashingLights();

				udooToArduino sm;
				pb_ostream_t sendStream = pb_ostream_from_buffer(sendBuffer,
						sizeof(sendBuffer));


				cout<<"Buff size:"<<sendStream.max_size<<endl;
				sm.speed = speed;
				sm.steering = steeringAngle;
				sm.leftBlink = leftFlash; //turnLeft;
				sm.rightBlink = rightFlash; //turnRight;
				sm.stopLight = brakeLight; //breakLed;

				cout<<"Buff written size:"<<sendStream.bytes_written<<endl;
				sendStatus = pb_encode(&sendStream, udooToArduino_fields, &sm);
				cout<<"Buff written size:"<<sendStream.bytes_written<<endl;
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

