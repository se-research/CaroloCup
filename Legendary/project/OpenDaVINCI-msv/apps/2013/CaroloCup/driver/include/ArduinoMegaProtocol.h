#ifndef ARDUINO_MEGA_PROTOCOL_H_
#define ARDUINO_MEGA_PROTOCOL_H_

#include <new>

namespace carolocup {
	using namespace std;

	class ArduinoMegaProtocol : public SerialPtotocol {
	public:
		SerialProtocol(string &port, int bufSize);
		void setSpeed(uint16_t speed);
		void setSteeringAngle(uint16_t angle);
		void setBrakeForce(uint16_t brakeFrc);
	};
}
#endif
