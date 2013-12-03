#include <string>
#include "core/base/Lock.h"
#include "core/data/Constants.h"
#include "SerialProtocol.h"

namespace carolocup {

using namespace std;
using namespace core::base;
using namespace core::data;

SerialProtocol::SerialProtocol() :
		m_partialData(),
		m_dataListenerMutex(),
		m_dataListener(NULL) {
}

SerialProtocol::SerialProtocol(string &port) {
	SerialProtocol();
	connect(port);
}

SerialProtocol::~SerialProtocol() {
	setSerialDataListener(NULL);
	close(fd);
}

SertialProtocol::connect(string &port) {
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

SerialProtocol::close() {
	close(fd);
}

void SerialProtocol::setSerialDataListener(SerialDataListener *listener) {
	Lock l(m_dataListenerMutex);
	m_dataListener = listener;
}

}
