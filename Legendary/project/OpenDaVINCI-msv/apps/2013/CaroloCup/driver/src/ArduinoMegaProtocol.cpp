#include "ArduinoMegaProtocol.h"

namespace carolocup {
	using namespace std;

	ArduinoMegaProtocol::ArduinoMegaProtocol(string &port, int bufSize) : SerialProtocol(port) {
		buf = new char[bufSize];
	}

	void ArduinoMegaProtocol::setSpeed(uint16_t speed) {
		stringstream ss;
		ss << 'm' << speed << '\0';
		int e = write(fd, ss.str());
	}

	void ArduinoMegaProtocol::setSteeringAngle(uint16_t angle) {
		stringstream ss;
		ss << 's' << angle << '\0';
		int e = write(fd, ss.str());
	}

	void ArduinoMegaProtocol::setBrakeForce(uint16_t brakeFrc) {
		stringstream ss;
		ss << 'b' << brakeFrc '\0';
		int e = write(fd, ss.str());
	}
}