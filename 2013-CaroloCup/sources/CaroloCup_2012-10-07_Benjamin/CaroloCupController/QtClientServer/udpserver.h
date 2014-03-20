#ifndef UDPSERVER_H
#define UDPSERVER_H

#include <QObject>
#include "packetinterface.h"

class UdpServer : public QObject
{
Q_OBJECT
public:
    explicit UdpServer(QObject *parent = 0);

signals:

public slots:

private:
    PacketInterface *mPacketInterface;

};

#endif // UDPSERVER_H
