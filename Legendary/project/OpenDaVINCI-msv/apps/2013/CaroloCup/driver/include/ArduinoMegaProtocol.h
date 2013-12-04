#ifndef ARDUINO_MEGA_PROTOCOL_H_
#define ARDUINO_MEGA_PROTOCOL_H_

<<<<<<< Updated upstream
#include "SerialProtocolSample.h"
=======
#include <sstream>
#include "SerialProtocol.h"
>>>>>>> Stashed changes

namespace carolocup {
	using namespace std;

<<<<<<< Updated upstream
	class ArduinoMegaProtocol {
	public:
		ArduinoMegaProtocol(const char *port, int bufSize);
		~ArduinoMegaProtocol();
		void setSpeed(unsigned speed);
		void setSteeringAngle(unsigned angle);
		void setBrakeForce(unsigned brakeFrc);
	private:
		SerialProtocolSample m_SerialProtocol;
=======
	class ArduinoMegaProtocol : public SerialProtocol {
	public:
		ArduinoMegaProtocol();
		ArduinoMegaProtocol(const char *port, int bufSize);

                ~ArduinoMegaProtocol();
		void setSpeed(int speed);
		void setSteeringAngle(int angle);
		void setBrakeForce(int brakeFrc);
>>>>>>> Stashed changes
	};
}
#endif

