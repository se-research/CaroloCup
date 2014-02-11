#include "ArduinoMegaProtocol.h"

namespace carolocup {
	using namespace std;

	ArduinoMegaProtocol::ArduinoMegaProtocol(const char *port, int bufSize) :
		m_SerialProtocol(port, bufSize){
	}

	void ArduinoMegaProtocol::setSpeed(int speed) {
		stringstream ss;
		ss << 'm' << speed << '/' << chkSum(speed) + chkSum('m') << ';';
		cout << ss.str() << endl;
		int errorCode = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setWheelFrequency(int freq, bool reverse) {
		stringstream ss;
		if(freq == 0) {
			ss << "f-" << '/' << chkSum("f-") << ';';
		} else {
			int prepsum;
			if(reverse){
				ss << "fr";
				prepsum = chkSum("fr");
			} else {
				ss << "ff";
				prepsum = chkSum("ff");
			}
			ss << freq << '/' << chkSum(freq) + prepsum<< ';';
		}
		int errorCode = m_SerialProtocol.writeToSerial(&ss);
 	}

	void ArduinoMegaProtocol::setSteeringAngle(int angle) {
		stringstream ss;
		int sum = 0;
		ss << 's' ;
		if(angle < 0) {
			ss << '-';
			sum += chkSum('-');
			angle = angle * (-1);
		}
		sum += chkSum(angle) + chkSum('s');
		
		ss << angle << '/' << sum << ';';
		cout << "Angle message ========================= " << ss.str() << endl;
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setCamAngle(int angle) {
		stringstream ss;
		ss << 'c' << angle << '/' << chkSum(angle) + chkSum('c') << ';';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setBrakeForce(char brakeFrc) {
		stringstream ss;
		ss << 'b' << brakeFrc << '/' << chkSum(brakeFrc) + chkSum('b') << ';';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setIndicatorsLeft() {
		stringstream ss;
		ss << "il" << '/' << chkSum("il") << ';';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setIndicatorsRight() {
		stringstream ss;
		ss << "ir" << '/' << chkSum("ir") << ';';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setIndicatorsAll() {
		stringstream ss;
		ss << "ia" << '/' << chkSum("ia") << ';';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	void ArduinoMegaProtocol::setIndicatorsStop() {
		stringstream ss;
		ss << "is" << '/' << chkSum("is") << ';';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	}

	ArduinoMegaProtocol::~ArduinoMegaProtocol() {
	}

  int ArduinoMegaProtocol::chkSum(int v){
    int chksum = 0;
    while(v/10){
      chksum += v%10;
      v /= 10;
    }
    chksum += v%10;

    return chksum;
  }

  int ArduinoMegaProtocol::chkSum(const char* p){
    int chksum = 0;
    while(*p){
      chksum += (*p) - '0';
      p++;
    }

    return chksum;
  }

  int ArduinoMegaProtocol::chkSum(char c){
    return c - '0';
  }
}
