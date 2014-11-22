#include "ArduinoMegaProtocol.h"

namespace msv {
	using namespace std;

	ArduinoMegaProtocol::ArduinoMegaProtocol(const string &port, int bufSize) :
		m_SerialProtocol(port, bufSize){
	}

	void ArduinoMegaProtocol::setSpeed(int speed) {
		stringstream ss;
		ss << 'm' << speed << '/';
		cout << ss.str() << endl;
		int errorCode = m_SerialProtocol.writeToSerial(&ss);
		if(errorCode==-1){
		cerr<<"Serial send failed."<<endl;
		}
	}

	void ArduinoMegaProtocol::setWheelFrequency(int freq, bool reverse) {
		stringstream ss;
		if(freq == 0) {
			ss << "f-" << '/';
			cout << "============Stop cmd!================" << endl;
		} else {
//			int prepsum;
			if(reverse){
				ss << "fr";
//				prepsum = chkSum("fr");
			} else {
				ss << "ff";
//				prepsum = chkSum("ff");
			}
			ss << freq << '/';
		}
		int errorCode = m_SerialProtocol.writeToSerial(&ss);
	if (errorCode == -1) {
		cerr << "Serial send failed." << endl;
	}
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
		
		ss << angle << '/';
		//cout << "Angle message ========================= " << ss.str() << endl;
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	if (error_code == -1) {
		cerr << "Serial send failed." << endl;
	}
	}

	void ArduinoMegaProtocol::setCamAngle(int angle) {
		stringstream ss;
		ss << 'c' << angle << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	if (error_code == -1) {
		cerr << "Serial send failed." << endl;
	}
	}

	void ArduinoMegaProtocol::setBrakeForce(char brakeFrc) {
		stringstream ss;
		ss << 'b' << brakeFrc << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	if (error_code == -1) {
		cerr << "Serial send failed." << endl;
	}
	}

	void ArduinoMegaProtocol::setIndicatorsLeft() {
		stringstream ss;
		ss << "il" << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	if (error_code == -1) {
		cerr << "Serial send failed." << endl;
	}
	}

	void ArduinoMegaProtocol::setIndicatorsRight() {
		stringstream ss;
		ss << "ir" << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	if (error_code == -1) {
		cerr << "Serial send failed." << endl;
	}
	}

	void ArduinoMegaProtocol::setIndicatorsAll() {
		stringstream ss;
		ss << "ia" << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	if (error_code == -1) {
		cerr << "Serial send failed." << endl;
	}
	}

	void ArduinoMegaProtocol::setIndicatorsStop() {
		stringstream ss;
		ss << "is" << '/';
		int error_code = m_SerialProtocol.writeToSerial(&ss);
	if (error_code == -1) {
		cerr << "Serial send failed." << endl;
	}
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
