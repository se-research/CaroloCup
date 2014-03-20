#include "udpserver.h"

UdpServer::UdpServer(QObject *parent) :
    QObject(parent)
{
    mPacketInterface = new PacketInterface(this);

    QHostAddress addr;
    addr.setAddress("127.0.0.1");
    mPacketInterface->connectUdp(addr, 27800);
    mPacketInterface->setMotorServo(0.0, 110);
}
