#include "ArduinoMegaProtocol.h"

namespace carolocup {
	using namespace std;

	ArduinoMegaProtocol::ArduinoMegaProtocol(const char *port, int bufSize) : SerialProtocol(port, bufSize) {
	}

	void ArduinoMegaProtocol::setSpeed(unsigned speed) {
		stringstream ss;
		ss << 'm' << speed << '\0';
		strcpy(buf, ss.str().c_str());
		int error_code = write(fd, buf, ss.str().size());
	}

	void ArduinoMegaProtocol::setSteeringAngle(unsigned angle) {
		stringstream ss;
		ss << 's' << angle << '\0';
		strcpy(buf, ss.str().c_str());
		int error_code = write(fd, buf, ss.str().size());
	}

	void ArduinoMegaProtocol::setBrakeForce(unsigned brakeFrc) {
		stringstream ss;
		ss << 'b' << brakeFrc << '\0';
		strcpy(buf, ss.str().c_str());
		int error_code = write(fd, buf, ss.str().size());
	}
}

