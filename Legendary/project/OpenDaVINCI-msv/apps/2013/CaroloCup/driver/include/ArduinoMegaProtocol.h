#ifndef ARDUINO_MEGA_PROTOCOL_H_
#define ARDUINO_MEGA_PROTOCOL_H_

#include <new>
#include "SerialProtocol.h"

namespace carolocup {
	using namespace std;

	class ArduinoMegaProtocol : public SerialProtocol {
	public:
		ArduinoMegaProtocol(const char *port, int bufSize);
		void setSpeed(unsigned speed);
		void setSteeringAngle(unsigned angle);
		void setBrakeForce(unsigned brakeFrc);
	};
}
#endif

