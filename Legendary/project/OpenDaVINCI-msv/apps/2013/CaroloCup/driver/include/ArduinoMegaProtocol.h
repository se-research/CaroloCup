#ifndef ARDUINO_MEGA_PROTOCOL_H_
#define ARDUINO_MEGA_PROTOCOL_H_

#include "SerialProtocolSample.h"

namespace carolocup {
	using namespace std;

	class ArduinoMegaProtocol {
	public:
		ArduinoMegaProtocol(const char *port, int bufSize);
		~ArduinoMegaProtocol();
		void setSpeed(unsigned speed);
		void setSteeringAngle(unsigned angle);
		void setBrakeForce(unsigned brakeFrc);
	private:
		SerialProtocolSample m_SerialProtocol;
	};
}
#endif

