#ifndef ARDUINO_MEGA_PROTOCOL_H_
#define ARDUINO_MEGA_PROTOCOL_H_

#include "SerialProtocolSample.h"

namespace msv {
	using namespace std;

	class ArduinoMegaProtocol {
	public:
		ArduinoMegaProtocol(const string &port, int bufSize);
		~ArduinoMegaProtocol();
		void setSpeed(int speed);
		void setWheelFrequency(int freq, bool reverse);
		void setSteeringAngle(int angle);
		void setCamAngle(int angle);
		void setBrakeForce(char brakeFrc);
		void setIndicatorsLeft();
		void setIndicatorsRight();
		void setIndicatorsAll();
		void setIndicatorsStop();
	private:
    int chkSum(int v);
    int chkSum(const char* p);
    int chkSum(char c);
		SerialProtocolSample m_SerialProtocol;
	};
}
#endif

