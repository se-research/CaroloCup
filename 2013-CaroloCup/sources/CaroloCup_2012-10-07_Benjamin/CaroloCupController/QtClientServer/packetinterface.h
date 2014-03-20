#ifndef PACKETINTERFACE_H
#define PACKETINTERFACE_H

#include <QObject>
#include <QUdpSocket>
#include <QTimer>
#include "locpoint.h"

class PacketInterface : public QObject
{
    Q_OBJECT
public:
    typedef struct {
        double v_batt;
        double v_log;
        double temp;
        double speed;	// Speed in meters/second
    } MC_VALUES;

    typedef struct {
        quint8 address;
        quint16 value;
    } ULTRA_SENSOR_VALUE;

    // Packets that expect response
    typedef enum {
        CAR_PACKET_READ_VALUES = 0,
        CAR_PACKET_READ_POS,
        CAR_PACKET_READ_SENS_ULTRA,
        CAR_PACKET_READ_SENS_IR,
        CAR_PACKET_PING
    } CAR_RES_PACKET_ID;

    // Packets that only expect ack
    typedef enum {
        CAR_PACKET_SET_POWER_SERVO = 0,
        CAR_PACKET_WRITE_POS
    } CAR_NORES_PACKET_ID;

    typedef enum {
        CAR_PACKET_RES = 1,
        CAR_PACKET_NORES = 2,
        CAR_PACKET_ACK = 254
    } CAR_SPECIAL_CMD;

    explicit PacketInterface(QObject *parent = 0);
    bool connectUdp(QHostAddress addr, quint16 port);
    bool sendPacket(const unsigned char *data, int len);
    bool sendPacket(QByteArray data);
    bool setMotorServo(double speed, quint8 dir);
    bool setPosition(LocPoint &pos);
    bool readValues();
    bool readPosition();
    bool readSensorsUltra();

signals:
    void carValuesReceived(PacketInterface::MC_VALUES values);
    void carPosReceived(LocPoint pos);
    void carPingReceived();
    void carResponseTimedOut();
    void carSensorsUltraReceived(QVector<PacketInterface::ULTRA_SENSOR_VALUE> values);
    
public slots:
    void readPendingDatagrams();
    void timerSlot();

private:
    unsigned short crc16(const unsigned char *buf, unsigned int len);
    void processPacket(const unsigned char *data, int len);

    quint16 mPort;
    QUdpSocket *mUdpSocket;
    QTimer *mTimer;
    QHostAddress mHostAddress;
    int mAckTimeout;
    int mAckTimer;
    QVector<QByteArray> mPacketBuffer;
    int mMaxSendBufferSize;

    // Packet state machine variables
    int mRxTimer;
    int mRxState;
    unsigned int mPayloadLength;
    unsigned char mRxBuffer[32768];
    unsigned char mRxDataPtr;
    unsigned char mCrcLow;
    unsigned char mCrcHigh;
    
};

#endif // PACKETINTERFACE_H
