
#include "SerialProtocolSample.h"

namespace msv {

using namespace std;

SerialProtocolSample::SerialProtocolSample():fd(0),
		buf(NULL),
		bufSize(0),
		portState(false){

}

SerialProtocolSample::SerialProtocolSample(const string &port, int buffSize)
{
	SerialProtocolSample();
	connect(port);
	buf = new char[buffSize];
	this->bufSize = buffSize;
}

SerialProtocolSample::~SerialProtocolSample() {
	close(fd);
}

void SerialProtocolSample::connect(const string &port) {
	//fd = open(port, O_RDWR | O_NOCTTY | O_NDELAY);
	fd = open(port.c_str(), O_RDWR | O_NONBLOCK );
	if (fd == -1) {
		perror("cannot open");
	}
	else {
//		fcntl(fd, F_SETFL, 0);
	}
	struct termios options;
	tcgetattr(fd, &options);
	cfsetispeed(&options, B115200);
	cfsetospeed(&options, B115200);
	options.c_cflag |= (CLOCAL | CREAD);
	tcsetattr(fd, TCSANOW, &options);
	options.c_cflag &= ~CSIZE;
	options.c_cflag &= ~PARENB;
	options.c_cflag &= ~CSTOPB;
	options.c_cflag &= ~CSIZE;
	options.c_cflag |= CS8;


	//see https://github.com/todbot/arduino-serial/blob/master/arduino-serial-lib.c
	options.c_cflag &= ~CRTSCTS;

	options.c_iflag &= ~(IXON | IXOFF | IXANY); // turn off s/w flow ctrl
	options.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG); // make raw
	options.c_oflag &= ~OPOST; // make raw

	 // see: http://unixwiz.net/techtips/termios-vmin-vtime.html
	options.c_cc[VMIN] = 0;
	options.c_cc[VTIME] = 0;

	 if( tcsetattr(fd, TCSAFLUSH, &options) < 0) {
	perror("init_serialport: Couldn't set term attributes");
	 }

}

void SerialProtocolSample::disconnect() {
	close(fd);
}

int SerialProtocolSample::writeToSerial(stringstream *ss) {
	strcpy(buf, ss->str().c_str());
    	return write(fd, buf, bufSize);
}

}

