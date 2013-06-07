#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <QtCore/QCoreApplication>
#include "udpserver.h"

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    UdpServer server;

    if (server.startServer("/dev/ttyACM0", 115200, 27800)) {
        qDebug() << "The server was successfully started.";
        qDebug() << "UDP Port:" << server.getUdpPort();
        qDebug() << "Serial Port:" << server.getSerialPortPath();
    } else {
        qCritical() << "Errors occured. Exiting...";
        QTimer::singleShot(500, &a, SLOT(quit()));
    }
    
    return a.exec();
}
