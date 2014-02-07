#ifndef ARDUINO_MEGA_PROTOCOL_H_
#define ARDUINO_MEGA_PROTOCOL_H_

#include "SerialProtocolSample.h"

namespace carolocup {
	using namespace std;

	class ArduinoMegaProtocol {
	public:
		ArduinoMegaProtocol(const char *port, int bufSize);
		~ArduinoMegaProtocol();
		void setSpeed(int speed);
		void setWheelFrequency(int freq);
		void setSteeringAngle(int angle);
		void setCamAngle(int angle);
		void setBrakeForce(char brakeFrc);
		void setIndicatorsLeft();
		void setIndicatorsRight();
		void setIndicatorsAll();
		void setIndicatorsStop();
	private:
		SerialProtocolSample m_SerialProtocol;
	};
}
#endif

