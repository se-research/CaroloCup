#include "packetinterface.h"
#include <QDebug>
#include <math.h>

namespace {
// Table of CRC constants - implements x^16+x^12+x^5+1
const unsigned short crc16_tab[] = { 0x0000, 0x1021, 0x2042, 0x3063, 0x4084,
        0x50a5, 0x60c6, 0x70e7, 0x8108, 0x9129, 0xa14a, 0xb16b, 0xc18c, 0xd1ad,
        0xe1ce, 0xf1ef, 0x1231, 0x0210, 0x3273, 0x2252, 0x52b5, 0x4294, 0x72f7,
        0x62d6, 0x9339, 0x8318, 0xb37b, 0xa35a, 0xd3bd, 0xc39c, 0xf3ff, 0xe3de,
        0x2462, 0x3443, 0x0420, 0x1401, 0x64e6, 0x74c7, 0x44a4, 0x5485, 0xa56a,
        0xb54b, 0x8528, 0x9509, 0xe5ee, 0xf5cf, 0xc5ac, 0xd58d, 0x3653, 0x2672,
        0x1611, 0x0630, 0x76d7, 0x66f6, 0x5695, 0x46b4, 0xb75b, 0xa77a, 0x9719,
        0x8738, 0xf7df, 0xe7fe, 0xd79d, 0xc7bc, 0x48c4, 0x58e5, 0x6886, 0x78a7,
        0x0840, 0x1861, 0x2802, 0x3823, 0xc9cc, 0xd9ed, 0xe98e, 0xf9af, 0x8948,
        0x9969, 0xa90a, 0xb92b, 0x5af5, 0x4ad4, 0x7ab7, 0x6a96, 0x1a71, 0x0a50,
        0x3a33, 0x2a12, 0xdbfd, 0xcbdc, 0xfbbf, 0xeb9e, 0x9b79, 0x8b58, 0xbb3b,
        0xab1a, 0x6ca6, 0x7c87, 0x4ce4, 0x5cc5, 0x2c22, 0x3c03, 0x0c60, 0x1c41,
        0xedae, 0xfd8f, 0xcdec, 0xddcd, 0xad2a, 0xbd0b, 0x8d68, 0x9d49, 0x7e97,
        0x6eb6, 0x5ed5, 0x4ef4, 0x3e13, 0x2e32, 0x1e51, 0x0e70, 0xff9f, 0xefbe,
        0xdfdd, 0xcffc, 0xbf1b, 0xaf3a, 0x9f59, 0x8f78, 0x9188, 0x81a9, 0xb1ca,
        0xa1eb, 0xd10c, 0xc12d, 0xf14e, 0xe16f, 0x1080, 0x00a1, 0x30c2, 0x20e3,
        0x5004, 0x4025, 0x7046, 0x6067, 0x83b9, 0x9398, 0xa3fb, 0xb3da, 0xc33d,
        0xd31c, 0xe37f, 0xf35e, 0x02b1, 0x1290, 0x22f3, 0x32d2, 0x4235, 0x5214,
        0x6277, 0x7256, 0xb5ea, 0xa5cb, 0x95a8, 0x8589, 0xf56e, 0xe54f, 0xd52c,
        0xc50d, 0x34e2, 0x24c3, 0x14a0, 0x0481, 0x7466, 0x6447, 0x5424, 0x4405,
        0xa7db, 0xb7fa, 0x8799, 0x97b8, 0xe75f, 0xf77e, 0xc71d, 0xd73c, 0x26d3,
        0x36f2, 0x0691, 0x16b0, 0x6657, 0x7676, 0x4615, 0x5634, 0xd94c, 0xc96d,
        0xf90e, 0xe92f, 0x99c8, 0x89e9, 0xb98a, 0xa9ab, 0x5844, 0x4865, 0x7806,
        0x6827, 0x18c0, 0x08e1, 0x3882, 0x28a3, 0xcb7d, 0xdb5c, 0xeb3f, 0xfb1e,
        0x8bf9, 0x9bd8, 0xabbb, 0xbb9a, 0x4a75, 0x5a54, 0x6a37, 0x7a16, 0x0af1,
        0x1ad0, 0x2ab3, 0x3a92, 0xfd2e, 0xed0f, 0xdd6c, 0xcd4d, 0xbdaa, 0xad8b,
        0x9de8, 0x8dc9, 0x7c26, 0x6c07, 0x5c64, 0x4c45, 0x3ca2, 0x2c83, 0x1ce0,
        0x0cc1, 0xef1f, 0xff3e, 0xcf5d, 0xdf7c, 0xaf9b, 0xbfba, 0x8fd9, 0x9ff8,
        0x6e17, 0x7e36, 0x4e55, 0x5e74, 0x2e93, 0x3eb2, 0x0ed1, 0x1ef0 };
}

PacketInterface::PacketInterface(QObject *parent) :
    QObject(parent)
{
    mRxState = 0;
    mRxTimer = 0;
    mAckTimeout = 20;
    mAckTimer = 0;
    mMaxSendBufferSize = 6;

    mPayloadLength = 0;
    mRxDataPtr = 0;
    mCrcLow = 0;
    mCrcHigh = 0;

    mUdpSocket = new QUdpSocket(this);
    mTimer = new QTimer(this);
    mTimer->setInterval(10);
    mTimer->start();
    mPacketBuffer.clear();

    connect(mUdpSocket, SIGNAL(readyRead()),
            this, SLOT(readPendingDatagrams()));
    connect(mTimer, SIGNAL(timeout()), this, SLOT(timerSlot()));
}

bool PacketInterface::connectUdp(QHostAddress addr, quint16 port)
{
    mPort = port;
    mHostAddress = addr;
    return mUdpSocket->bind(QHostAddress::Any, mPort);
}

void PacketInterface::readPendingDatagrams()
{
    unsigned char rx_data;
    const int rx_timeout = 50;

    while (mUdpSocket->hasPendingDatagrams()) {
        QByteArray data;
        data.resize(mUdpSocket->pendingDatagramSize());
        QHostAddress sender;
        quint16 senderPort;

        mUdpSocket->readDatagram(data.data(), data.size(),
                                &sender, &senderPort);

        for(int i = 0;i < data.length();i++) {
            rx_data = data[i];

            switch (mRxState) {
            case 0:
                if (rx_data == 2) {
                    mRxState++;
                    mRxTimer = rx_timeout;
                    mRxDataPtr = 0;
                } else {
                    mRxState = 0;
                }
                break;

            case 1:
                mPayloadLength = rx_data;
                mRxState++;
                mRxTimer = rx_timeout;
                break;

            case 2:
                mRxBuffer[mRxDataPtr++] = rx_data;
                if (mRxDataPtr == mPayloadLength) {
                    mRxState++;
                }
                mRxTimer = rx_timeout;
                break;

            case 3:
                mCrcHigh = rx_data;
                mRxState++;
                mRxTimer = rx_timeout;
                break;

            case 4:
                mCrcLow = rx_data;
                mRxState++;
                mRxTimer = rx_timeout;
                break;

            case 5:
                if (rx_data == 3) {
                    if (crc16(mRxBuffer, mPayloadLength) ==
                            ((unsigned short)mCrcHigh << 8 | (unsigned short)mCrcLow)) {
                        // Packet received!
                        processPacket(mRxBuffer, mPayloadLength);
                    }
                }

                mRxState = 0;
                break;

            default:
                mRxState = 0;
                break;
            }
        }
    }
}

void PacketInterface::timerSlot()
{
    if (mRxTimer) {
        mRxTimer--;
    } else {
        mRxState = 0;
    }

    if (mAckTimer) {
        mAckTimer--;

        if (!mAckTimer) {
            emit carResponseTimedOut();
        }
    }

    // Send packets that were buffered while waiting for the last packet
    if (mPacketBuffer.size() && !mAckTimer) {
        QByteArray packet = mPacketBuffer.first();
        mPacketBuffer.pop_front();
        sendPacket(packet);
    }
}

unsigned short PacketInterface::crc16(const unsigned char *buf, unsigned int len)
{
    unsigned int i;
    unsigned short cksum = 0;
    for (i = 0; i < len; i++) {
        cksum = crc16_tab[(((cksum >> 8) ^ *buf++) & 0xFF)] ^ (cksum << 8);
    }
    return cksum;
}

void PacketInterface::processPacket(const unsigned char *data, int len)
{
    unsigned int i = 0;
    quint16 tmp_us = 0;
    qint16 tmp_s = 0;
    LocPoint loc;
    MC_VALUES values;
    QVector<ULTRA_SENSOR_VALUE> sensor_values;
    ULTRA_SENSOR_VALUE ultra_sensor_tmp;

    // Send packets that were buffered while waiting for the last response
    mAckTimer = 0;
    if (mPacketBuffer.size() && !mAckTimer) {
        QByteArray packet = mPacketBuffer.first();
        mPacketBuffer.pop_front();
        sendPacket(packet);
    }

    switch (data[0]) {
    case CAR_PACKET_READ_VALUES:
        data++;
        i = 0;

        tmp_us = (quint16)data[i] << 8 | (quint16)data[i + 1];
        i += 2;
        values.v_batt = (double)tmp_us / 1000.0;

        tmp_us = (quint16)data[i] << 8 | (quint16)data[i + 1];
        i += 2;
        values.v_log = (double)tmp_us / 1000.0;

        tmp_s = (qint16)data[i] << 8 | (qint16)data[i + 1];
        i += 2;
        values.temp = (double)tmp_s / 100.0;

        tmp_s = (qint16)data[i] << 8 | (qint16)data[i + 1];
        i += 2;
        values.speed = (double)tmp_s / 100.0;

        emit carValuesReceived(values);
        break;

    case CAR_PACKET_READ_POS:
        data++;
        i = 0;
        loc.setX((double)((qint32)data[i] << 24 | (qint32)data[i + 1] << 16 |
                 (qint32)data[i + 2] << 8 | (qint32)data[i + 3]));
        i += 4;

        loc.setY((double)((qint32)data[i] << 24 | (qint32)data[i + 1] << 16 |
                 (qint32)data[i + 2] << 8 | (qint32)data[i + 3]));
        i += 4;

        loc.setAlpha((double)((quint16)data[i] << 8 | (quint16)data[i + 1]) / 10000.0);
        i += 2;

        emit carPosReceived(loc);
        break;

    case CAR_PACKET_READ_SENS_ULTRA:
        data++;
        sensor_values.clear();
        tmp_us = (len - 1) / 3;

        for (int i = 0;i < tmp_us;i++) {
            ultra_sensor_tmp.address = (quint8)data[i];
            ultra_sensor_tmp.value = ((quint16)data[i * 2 + tmp_us] << 8) |
                    (quint16)data[i * 2 + tmp_us + 1];
            sensor_values.append(ultra_sensor_tmp);
        }

        emit carSensorsUltraReceived(sensor_values);
        break;

    case CAR_PACKET_READ_SENS_IR:
        // TODO
        break;

    case CAR_PACKET_PING:
        emit carPingReceived();
        break;

    case CAR_PACKET_ACK:
        break;

    default:
        break;
    }
}

bool PacketInterface::sendPacket(const unsigned char *data, int len)
{
    if (mAckTimer) {
        if (mPacketBuffer.size() < mMaxSendBufferSize) {
            QByteArray packet;
            packet.clear();
            packet.append((char*)data, len);
            mPacketBuffer.append(packet);
            return true;
        } else {
            return false;
        }
    }

    if (QString::compare(mHostAddress.toString(), "0.0.0.0") == 0) {
        qDebug() << "No send destination...";
        return false;
    }

    unsigned char buffer[len + 5];
    buffer[0] = 2;
    buffer[1] = len;

    memcpy(buffer + 2, data, len);

    unsigned short crc = crc16(data, len);
    buffer[len + 2] = crc >> 8;
    buffer[len + 3] = crc;
    buffer[len + 4] = 3;

    QByteArray sendData = QByteArray::fromRawData((char*)buffer, len + 5);
    mAckTimer = mAckTimeout;

    return mUdpSocket->writeDatagram(sendData, mHostAddress, mPort) > 0 ? true : false;
}

bool PacketInterface::sendPacket(QByteArray data)
{
    return sendPacket((const unsigned char*)data.data(), data.size());
}

bool PacketInterface::setMotorServo(double speed, quint8 dir)
{
    QByteArray buffer;
    buffer.clear();
    buffer.append((char)CAR_PACKET_NORES);
    buffer.append((char)CAR_PACKET_SET_POWER_SERVO);

    qint16 speed_int = (quint16)(speed * 100.0);

    buffer.append((char)(speed_int >> 8));
    buffer.append((char)speed_int);
    buffer.append((char)dir);
    return sendPacket(buffer);
}

bool PacketInterface::setPosition(LocPoint &pos)
{
    QByteArray buffer;
    qint32 px, py;
    qint16 alpha;
    double alpha_d;

    // Make sure that the angle is within the range 0 - 2*pi
    alpha_d = pos.getAlpha();
    while (alpha_d > 2.0 * M_PI) {
        alpha_d -= 2.0 * M_PI;
    }

    while (alpha_d < 0) {
        alpha_d += 2.0 * M_PI;
    }

    buffer.clear();
    buffer.append((char)CAR_PACKET_NORES);
    buffer.append((char)CAR_PACKET_WRITE_POS);

    px = (qint32)pos.getX();
    py = (qint32)pos.getY();
    alpha = (qint16)(alpha_d * 10000.0);

    buffer.append((char)(px >> 24));
    buffer.append((char)(px >> 16));
    buffer.append((char)(px >> 8));
    buffer.append((char)px);

    buffer.append((char)(py >> 24));
    buffer.append((char)(py >> 16));
    buffer.append((char)(py >> 8));
    buffer.append((char)py);

    buffer.append((char)(alpha >> 8));
    buffer.append((char)alpha);
    return sendPacket(buffer);
}

bool PacketInterface::readValues()
{
    QByteArray buffer;
    buffer.clear();
    buffer.append((char)CAR_PACKET_RES);
    buffer.append((char)CAR_PACKET_READ_VALUES);
    return sendPacket(buffer);
}

bool PacketInterface::readPosition()
{
    QByteArray buffer;
    buffer.clear();
    buffer.append((char)CAR_PACKET_RES);
    buffer.append((char)CAR_PACKET_READ_POS);
    return sendPacket(buffer);
}

bool PacketInterface::readSensorsUltra()
{
    QByteArray buffer;
    buffer.clear();
    buffer.append((char)CAR_PACKET_RES);
    buffer.append((char)CAR_PACKET_READ_SENS_ULTRA);
    return sendPacket(buffer);
}
