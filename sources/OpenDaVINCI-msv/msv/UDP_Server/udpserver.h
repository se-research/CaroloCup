#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#ifndef UDPSERVER_H
#define UDPSERVER_H

#include <QObject>
#include <QUdpSocket>
#include <QTimer>
#include "serialport.h"

class UdpServer : public QObject
{
    Q_OBJECT
public:
    explicit UdpServer(QObject *parent = 0);
    bool startServer(const QString &serialPort, int baudrate, quint16 udpPort);
    QString getSerialPortPath();
    quint16 getUdpPort();

signals:
    
public slots:
    void readPendingDatagrams();
    void serialDataAvilable();
    void serialPortError(int e);
    void timerSlot();

private:
    unsigned short crc16(const unsigned char *buf, unsigned int len);
    void processPacket(const unsigned char *data, int len);

    QUdpSocket *mUdpSocket;
    SerialPort *mSerialPort;
    QTimer *mTimer;
    int mRxTimeout;
    int mRxState;
    QHostAddress mLastHostAddress;
    QString mPortPath;
    int mBaudrate;
    quint16 mUdpPort;
    QByteArray mSendArray;

    unsigned int mPayloadLength;
    unsigned char mRxBuffer[32768];
    unsigned char mRxDataPtr;
    unsigned char mCrcLow;
    unsigned char mCrcHigh;

};

#endif // UDPSERVER_H
