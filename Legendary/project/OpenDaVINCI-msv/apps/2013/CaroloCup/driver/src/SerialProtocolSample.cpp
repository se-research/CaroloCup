
#include "SerialProtocolSample.h"

namespace carolocup {

using namespace std;

SerialProtocolSample::SerialProtocolSample() {
	fd = 0;
	buf = NULL;
	bufSize = 0;
	portState = false;
}

SerialProtocolSample::SerialProtocolSample(const char *port, int bufSize) {
	SerialProtocolSample();
	connect(port);
	buf = new char[bufSize];
	this->bufSize = bufSize;
}

SerialProtocolSample::~SerialProtocolSample() {
	close(fd);
}

void SerialProtocolSample::connect(const char *port) {
	fd = open(port, O_RDWR | O_NOCTTY | O_NDELAY);
	if (fd == -1) {
		perror("cannot open");
	}
	else {
		fcntl(fd, F_SETFL, 0);
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
}

void SerialProtocolSample::disconnect() {
	close(fd);
}

int SerialProtocolSample::writeToSerial(stringstream *ss) {
	strcpy(buf, ss->str().c_str());
    	return write(fd, buf, ss->str().size());
}

}

