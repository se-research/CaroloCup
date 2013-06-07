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

        if (argc > 1) {
            qDebug() << "Adding subscribers";
            for(int i = 1;i < argc;i++) {
                QHostAddress adr(QString(argv[i]));
                server.addSubscriber(adr);
                qDebug() << adr;
            }
        }
    } else {
        qCritical() << "Errors occured. Exiting...";
        QTimer::singleShot(500, &a, SLOT(quit()));
    }
    
    return a.exec();
}
