#include "ArduinoMegaProtocol.h"

namespace carolocup {
	using namespace std;

	ArduinoMegaProtocol::ArduinoMegaProtocol(const char *port, int bufSize) : 
		m_SerialProtocol(port, bufSize){
	}

	void ArduinoMegaProtocol::setSpeed(unsigned speed) {
		stringstream ss;
		ss << 'm' << speed << '\0';
		int errorCode = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setSteeringAngle(unsigned angle) {
		stringstream ss;
		ss << 's' << angle << '\0';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setBrakeForce(unsigned brakeFrc) {
		stringstream ss;
		ss << 'b' << brakeFrc << '\0';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	ArduinoMegaProtocol::~ArduinoMegaProtocol() {
	}
}
