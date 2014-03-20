#include <QtCore/QCoreApplication>
#include "udpserver.h"

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    UdpServer server;

    QTimer::singleShot(1000, &a, SLOT(quit()));

    return a.exec();
}
