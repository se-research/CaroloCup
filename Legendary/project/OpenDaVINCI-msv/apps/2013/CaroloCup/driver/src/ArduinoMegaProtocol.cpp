#include "ArduinoMegaProtocol.h"

namespace carolocup {
	using namespace std;

	ArduinoMegaProtocol::ArduinoMegaProtocol(const char *port, int bufSize) : 
		m_SerialProtocol(port, bufSize){
	}

	void ArduinoMegaProtocol::setSpeed(int speed) {
		stringstream ss;
		ss << 'm' << speed << '/';
		int errorCode = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setWheelFrequency(int freq) {
		stringstream ss;
		ss << 'f' << freq << '/';
		int errorCode = m_SerialProtocol.writeToSerial(&ss);
 	}

	void ArduinoMegaProtocol::setSteeringAngle(int angle) {
		stringstream ss;
		ss << 's' << angle << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setCamAngle(int angle) {
		stringstream ss;
		ss << 'c' << angle << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setBrakeForce(char brakeFrc) {
		stringstream ss;
		ss << 'b' << brakeFrc << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setIndicatorsLeft() {
		stringstream ss;
		ss << "il" << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setIndicatorsRight() {
		stringstream ss;
		ss << "ir" << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setIndicatorsAll() {
		stringstream ss;
		ss << "ia" << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setIndicatorsStop() {
		stringstream ss;
		ss << "is" << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	ArduinoMegaProtocol::~ArduinoMegaProtocol() {
	}
}
