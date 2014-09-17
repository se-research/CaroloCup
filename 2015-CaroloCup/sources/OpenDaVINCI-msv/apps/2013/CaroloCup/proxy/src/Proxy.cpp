/*
* CaroloCup.
*
* This software is open source. Please see COPYING and AUTHORS for further information.
*/

#include <cstring>
#include <cmath>

#include <string>

#include "core/macros.h"
#include "core/base/LIFOQueue.h"
#include "core/data/Container.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/base/Thread.h"
#include "core/data/Constants.h"
#include "core/data/TimeStamp.h"
#include "core/data/control/ForceControl.h"
#include "core/data/environment/VehicleData.h"
#include "core/io/ContainerConference.h"
#include "core/wrapper/UDPFactory.h"
#include "core/wrapper/UDPSender.h"
#include "core/wrapper/UDPReceiver.h"
#include "core/io/StreamFactory.h"
#include "core/io/URL.h"
#include "core/exceptions/Exceptions.h"

#include "SensorBoardData.h"
#include "Proxy.h"

namespace carolocup {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::data::control;
    using namespace core::data::environment;
    using namespace core::exceptions;
    using namespace core::io;

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


    Proxy::Proxy(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "Proxy"),
        m_mapOfPointSensors(),
        m_sensorBoardData(),
        m_steeringOffset(127),
        m_US_ID(4),
        m_debug(false) {
    }

    Proxy::~Proxy() {
    }

    void Proxy::setUp() {
// This method will be call automatically _before_ running body().
    }

    void Proxy::tearDown() {
// This method will be call automatically _after_ return from body().
    }

    unsigned short Proxy::crc16(const unsigned char *buf, unsigned int len) const {
        unsigned int i;
        unsigned short cksum = 0;
        for (i = 0; i < len; i++) {
            cksum = crc16_tab[(((cksum >> 8) ^ *buf++) & 0xFF)] ^ (cksum << 8);
        }
        return cksum;
    }

    string Proxy::createPayloadForQueryOnboardData() const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_RES;
        strbuffer << (char)CAR_PACKET_READ_VALUES;
        return strbuffer.str();
    }

    string Proxy::createPayloadForQueryUltraSonicData() const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_RES;
        strbuffer << (char)CAR_PACKET_READ_SENS_ULTRA;
        return strbuffer.str();
    }

    string Proxy::createPayloadForQueryInfraRedData() const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_RES;
        strbuffer << (char)CAR_PACKET_READ_SENS_IR;
        return strbuffer.str();
    }

    string Proxy::createPayloadForQueryPositionData() const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_RES;
        strbuffer << (char)CAR_PACKET_READ_POS;
        return strbuffer.str();
    }

    string Proxy::createPayloadForSettingAccelerationAndSteering(const double &speed, const uint8_t &direction) const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_NORES;
        strbuffer << (char)CAR_PACKET_SET_POWER_SERVO;

        int16_t speed_int = (int16_t)(speed * 100.0);
        
        strbuffer << (char)(speed_int >> 8);
        strbuffer << (char)(speed_int);

        uint8_t dir = direction;
        strbuffer << (char)(dir);
        return strbuffer.str();
    }

    string Proxy::createPayloadForWritePos(const double &x, const double &y, const double &heading) const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_NORES;
        strbuffer << (char)CAR_PACKET_WRITE_POS;

        int32_t px = 0, py = 0;
        int16_t alpha = 0;
        double alpha_d = heading;

        while (alpha_d > 2.0 * Constants::PI) {
            alpha_d -= 2.0 * Constants::PI;
        }

        while (alpha_d < 0) {
            alpha_d += 2.0 * Constants::PI;
        }

        px = (int32_t)x;
        py = (int32_t)y;
        alpha = (int16_t)(alpha_d * 10000.0);

        strbuffer << ((char)(px >> 24));
        strbuffer << ((char)(px >> 16));
        strbuffer << ((char)(px >> 8));
        strbuffer << ((char)px);

        strbuffer << ((char)(py >> 24));
        strbuffer << ((char)(py >> 16));
        strbuffer << ((char)(py >> 8));
        strbuffer << ((char)py);

        strbuffer << ((char)(alpha >> 8));
        strbuffer << ((char)alpha);

        return strbuffer.str();
    }

    string Proxy::createPayloadForAddPosition(const double &x, const double &y, const double &speed) const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_NORES;
        strbuffer << (char)CAR_PACKET_ADD_POINT;

        int32_t px = 0, py = 0;
        px = (int32_t)x;
        py = (int32_t)y;

        strbuffer << ((char)(px >> 24));
        strbuffer << ((char)(px >> 16));
        strbuffer << ((char)(px >> 8));
        strbuffer << ((char)px);

        strbuffer << ((char)(py >> 24));
        strbuffer << ((char)(py >> 16));
        strbuffer << ((char)(py >> 8));
        strbuffer << ((char)py);

        int16_t speed_int = (int16_t)(speed * 100.0);
        strbuffer << (char)(speed_int >> 8);
        strbuffer << (char)(speed_int);

        return strbuffer.str();
    }

    string Proxy::createPayloadForReadTravelCounter(const bool &isAbs) const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_RES;
        strbuffer << (char)CAR_PACKET_READ_TRAVEL_COUNTER;
        strbuffer << (char)(isAbs ? 1 : 0);
        return strbuffer.str();
    }

    string Proxy::createPayloadForRunAP(const bool &run) const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_NORES;
        strbuffer << (char)CAR_PACKET_AP_RUN;
        strbuffer << (char)(run ? 1 : 0);
        return strbuffer.str();
    }

    string Proxy::createPayloadForClearAP() const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_NORES;
        strbuffer << (char)CAR_PACKET_AP_CLEAR;
        return strbuffer.str();
    }

    string Proxy::createPayloadForResetTravelCounter() const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_NORES;
        strbuffer << (char)CAR_PACKET_RESET_TRAVEL_CNT;
        return strbuffer.str();
    }

    string Proxy::createPayloadForFullBrake() const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_NORES;
        strbuffer << (char)CAR_PACKET_FULL_BRAKE;
        return strbuffer.str();
    }

    string Proxy::createPayloadForServoOffset(const uint32_t &offset) const {
        stringstream strbuffer;
        strbuffer << (char)CAR_PACKET_NORES;
        strbuffer << (char)CAR_PACKET_SERVO_OFFSET;
        strbuffer << (char)(offset);
        return strbuffer.str();
    }

    string Proxy::createPacket(const string &s) const {
        // Create packet for UDP_Server with crc16 checksum.
        const uint16_t len = s.size();
        unsigned char *buffer = new unsigned char[len + 5];
        buffer[0] = 2;
        buffer[1] = len;

        memcpy(buffer + 2, s.c_str(), len);

        unsigned short crc = crc16((const unsigned char*)s.c_str(), len);
        buffer[len + 2] = crc >> 8;
        buffer[len + 3] = crc;
        buffer[len + 4] = 3;

        // Send to UDP_Server.
        string retVal((const char*)buffer, len+5);
        OPENDAVINCI_CORE_DELETE_ARRAY(buffer);
        return retVal;
    }

    void Proxy::nextString(const string &s) {
        const char *data = s.c_str();
        // First byte is always 2. Skip it.
        data++;

        // Second byte is length.
        const uint16_t len = (uint16_t)(*data);
        data++;

        // Third byte is type of reply.
        CAR_RES_PACKET_ID reply = (enum CAR_RES_PACKET_ID)(uint16_t)(*data);

        // data+len is crc high.
        unsigned char crcHigh = *(data+len);
        // data+len+1 is crc low
        unsigned char crcLow = *(data+len+1);

        bool validCRC = (crc16((const unsigned char*)data, len) == ((unsigned short)crcHigh << 8 | (unsigned short)crcLow));

        // data+len+2 must be 3 according to Benjamin's protocol.
        bool packetTerminator = (*(data+len+2) == 3);

        if (m_debug) {
// cerr << "Proxy received '" << s << "', length = " << len << ", type: " << reply << ", validCRC: " << validCRC << ", packetTerminator: " << packetTerminator << endl;
        }

        if (validCRC && packetTerminator) {
            switch (reply) {
                case CAR_PACKET_READ_VALUES:
                {
                    data++;
                    uint16_t i = 0;

                    VehicleData vd;

                    uint16_t tmp_us = (uint16_t)data[i] << 8 | (uint16_t)data[i + 1];
                    i += 2;
                    vd.setV_batt((double)tmp_us / 1000.0);

                    tmp_us = (uint16_t)data[i] << 8 | (uint16_t)data[i + 1];
                    i += 2;
                    vd.setV_log((double)tmp_us / 1000.0);

                    int16_t tmp_s = (int16_t)data[i] << 8 | (int16_t)data[i + 1];
                    i += 2;
                    vd.setTemp((double)tmp_s / 1000.0);

                    tmp_s = (int16_t)data[i] << 8 | (int16_t)data[i + 1];
                    i += 2;
                    vd.setSpeed((double)tmp_s / 1000.0);

                    Container c(Container::VEHICLEDATA, vd);
                    getConference().send(c);

                    break;
                }

                case CAR_PACKET_READ_POS:
                {
                    data++;
                    uint16_t i = 0;

                    double x = (double)((int32_t)data[i] << 24 | (int32_t)data[i + 1] << 16 | (int32_t)data[i + 2] << 8 | (int32_t)data[i + 3]);
                    i += 4;

                    double y = (double)((int32_t)data[i] << 24 | (int32_t)data[i + 1] << 16 | (int32_t)data[i + 2] << 8 | (int32_t)data[i + 3]);
                    i += 4;

                    double heading = (double)((uint16_t)data[i] << 8 | (uint16_t)data[i + 1]) / 10000.0;
                    i += 2;

//                    m_vehiclePosition.setX(x);
//                    m_vehiclePosition.setY(y);
//                    m_vehiclePosition.setHeading(heading);

                    if (m_debug) {
                        cout << "Position data: x=" << x << ", y=" << y << ", heading=" << heading << endl;
                    }

                    break;
                }

                case CAR_PACKET_READ_SENS_ULTRA:
                {
                    bool sensorBoardData_updated = false;
                    data++;
                    uint16_t tmp_us = (len - 1) / 3;

                    for (int i = 0; i < tmp_us; i++) {
                        uint8_t us_address = (uint8_t)data[i];
                        uint16_t us_value = ((uint16_t)data[i * 2 + tmp_us] << 8) | (uint16_t)data[i * 2 + tmp_us + 1];

                        if (m_debug) {
                            cout << "Ultra sonic: " << (int)us_address << ": " << us_value << endl;
                        }

                        // Find address of this sensor.
                        map<uint16_t, PointSensor*>::iterator it = m_mapOfPointSensors.find(us_address);
                        if(it != m_mapOfPointSensors.end()) {
                            // Address was configured in configuration file.
                            const uint16_t SENSOR_ID = m_mapOfPointSensors[us_address]->getID();

                            // Check if we need to clamp the distance to -1.
                            double distance = us_value/100.0;
                            if (distance > m_mapOfPointSensors[us_address]->getClampDistance()) {
                                distance = -1;
                            }

                            // Update SensorBoardData.
                            m_sensorBoardData.update(SENSOR_ID, distance);
                            if (m_debug) {
                                cerr << "Updated " << SENSOR_ID << " with " << distance << endl;
                            }
                            sensorBoardData_updated = true;
                        }
                    }

                    if (sensorBoardData_updated) {
                        // Distribute SensorBoardData.

                 // CaroloCup: Create a container with user data Container::USER_DATA_0.
                 Container c(Container::USER_DATA_0, m_sensorBoardData);

                        // CaroloCup: Send container.
                        getConference().send(c);
                    }

                    break;
                }

                case CAR_PACKET_READ_SENS_IR:
                {
                    bool sensorBoardData_updated = false;
                    data++;
                    uint16_t tmp_ir = (len - 1) / 2;

                    for (int i = 0; i < tmp_ir; i++) {
                        uint8_t ir_address = i; // Just loop through the sensors.
                        uint16_t ir_value = ((uint16_t)data[i * 2] << 8) | (uint16_t)data[i * 2 + 1];

                        // Mapping because the values are inverted.
                        double distance = 0.122 * pow(ir_value * 0.005, -0.975);

                        if (m_debug) {
                            cout << "Infra red: " << (int)ir_address << ": " << ir_value << ", mapped value [m]: " << distance << endl;
                        }

                        // Find address of this sensor.
                        map<uint16_t, PointSensor*>::iterator it = m_mapOfPointSensors.find(ir_address);
                        if(it != m_mapOfPointSensors.end()) {
                            // Address was configured in configuration file.
                            const uint16_t SENSOR_ID = m_mapOfPointSensors[ir_address]->getID();

                            // Check if we need to clamp the distance to -1.
                            if (distance > m_mapOfPointSensors[ir_address]->getClampDistance()) {
                                distance = -1;
                            }

                            // Update SensorBoardData.
                            m_sensorBoardData.update(SENSOR_ID, distance);
                            if (m_debug) {
                                cerr << "Updated " << SENSOR_ID << " with " << distance << endl;
                            }
                            sensorBoardData_updated = true;
                        }
                    }

                    if (sensorBoardData_updated) {
                        // Distribute SensorBoardData.

                 // CaroloCup: Create a container with user data Container::USER_DATA_0.
                 Container c(Container::USER_DATA_0, m_sensorBoardData);

                        // CaroloCup: Send container.
                        getConference().send(c);
                    }

                    break;
                }

                case CAR_PACKET_READ_TRAVEL_COUNTER:
                {
                    data++;
                    uint16_t i = 0;

                    double traveledDistance = (double)((int32_t)data[i] << 24 | (int32_t)data[i + 1] << 16 | (int32_t)data[i + 2] << 8 | (int32_t)data[i + 3]);
                    i += 4;

                    if (m_debug) {
                        cout << "traveled distance = " << traveledDistance << endl;
                    }

//                    m_vehiclePosition.setTraveledPath(traveledDistance);

                    break;
                }

                case CAR_PACKET_PING:
                    break;

                case CAR_PACKET_ACK2:
                    break;

                default:
                    break;
            }
        }
    }

    void Proxy::parkTrajectory(core::wrapper::UDPSender *connectionToUDPServer) {
        string payload = "";
        string packet = "";

        
                    if (m_debug) {
                        cout << "Stopping AP" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForRunAP(false);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Setting position" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForWritePos(0, 710, Constants::PI/2.0);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Clearing point list" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForClearAP();
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Add 1st point" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForAddPosition(0, 700, -0.7);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Add 2nd point" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForAddPosition(200, 350, -0.7);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Add 3rd point" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForAddPosition(400, 0, -0.4);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Start AP" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForRunAP(true);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);
    }

    void Proxy::parkTrajectoryNew(core::wrapper::UDPSender *connectionToUDPServer) {
        string payload = "";
        string packet = "";

        
                    if (m_debug) {
                        cout << "Stopping AP" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForRunAP(false);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Setting position" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForWritePos(0, 0, Constants::PI/2.0);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Clearing point list" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForClearAP();
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                // Create a sendable packet.
                payload = createPayloadForAddPosition(0, 320, 0.7);
                packet = createPacket(payload);
                connectionToUDPServer->send(packet);

                payload = createPayloadForAddPosition(150, 0, -0.7);
                packet = createPacket(payload);
                connectionToUDPServer->send(packet);

                payload = createPayloadForAddPosition(300, -350, -0.6);
                packet = createPacket(payload);
                connectionToUDPServer->send(packet);

                payload = createPayloadForAddPosition(320, -250, 0.6);
                packet = createPacket(payload);
                connectionToUDPServer->send(packet);

                payload = createPayloadForRunAP(true);
                packet = createPacket(payload);
                connectionToUDPServer->send(packet);
    }


    void Proxy::goForward(core::wrapper::UDPSender *connectionToUDPServer) {
        string payload = "";
        string packet = "";

                    if (m_debug) {
                        cout << "Send servo offset" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForServoOffset(m_steeringOffset);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    payload = createPayloadForResetTravelCounter();
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Stopping AP" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForRunAP(false);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Setting position" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForWritePos(0, 0, Constants::PI/2.0);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Clearing point list" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForClearAP();
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    // Create a sendable packet.
                    payload = createPayloadForAddPosition(0, 100, 0.7);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    // Create a sendable packet.
                    payload = createPayloadForAddPosition(0, 200, 0.7);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    // Create a sendable packet.
                    payload = createPayloadForAddPosition(0, 8000, 0.7);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

                    if (m_debug) {
                        cout << "Start AP" << endl;
                    }
                    // Create a sendable packet.
                    payload = createPayloadForRunAP(true);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);
    }



    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE Proxy::body() {
        // Get configuration data.
        KeyValueConfiguration kv = getKeyValueConfiguration();
        m_debug = kv.getValue<uint32_t>("proxy.debug") == 1;
        const string host = kv.getValue<string>("proxy.host");
        const uint32_t server_port = kv.getValue<uint32_t>("proxy.port");
        const uint32_t client_port = server_port + 1;

// const int32_t STEERING_CENTER = kv.getValue<int32_t>("proxy.steering.center");
// const int32_t STEERING_MIN = kv.getValue<int32_t>("proxy.steering.min");
// const int32_t STEERING_MAX = kv.getValue<int32_t>("proxy.steering.max");
// const bool STEERING_INVERT = kv.getValue<int32_t>("proxy.steering.invert") == 1;

        m_US_ID = kv.getValue<uint32_t>("proxy.parking.us_id");
        m_steeringOffset = kv.getValue<uint32_t>("proxy.steering.offset");

        const int32_t ACCELERATION_HYSTERESIS = kv.getValue<int32_t>("proxy.acceleration.hysteresis");
        const double ACCELERATION_MAX = kv.getValue<double>("proxy.acceleration.max");

        const string USER_START_BUTTON_FILE = kv.getValue<string>("proxy.user_start_button_file");
        const string BRAKE_LED_FILE = kv.getValue<string>("proxy.GPIO_brakeLED_file");
        const string TURN_LEFT_LED_FILE = kv.getValue<string>("proxy.GPIO_turnLeftLED_file");
        const string TURN_RIGHT_LED_FILE = kv.getValue<string>("proxy.GPIO_turnRightLED_file");

        // Setup all point sensors.
        for (uint32_t i = 0; i < getKeyValueConfiguration().getValue<uint32_t>("proxy.numberOfSensors"); i++) {
            stringstream sensorID;
            sensorID << "proxy.sensor" << i << ".id";
            uint16_t id(getKeyValueConfiguration().getValue<uint16_t>(sensorID.str()));

            stringstream sensorName;
            sensorName << "proxy.sensor" << i << ".name";
            string name(getKeyValueConfiguration().getValue<string>(sensorName.str()));

            stringstream sensorAddress;
            sensorAddress << "proxy.sensor" << i << ".address";
            uint16_t address(getKeyValueConfiguration().getValue<uint16_t>(sensorAddress.str()));

            stringstream sensorClampDistance;
            sensorClampDistance << "proxy.sensor" << i << ".clampDistance";
            double clampDistance(getKeyValueConfiguration().getValue<double>(sensorClampDistance.str()));

            PointSensor *ps = new PointSensor(id, name, address, clampDistance);

            if (ps != NULL) {
                // Save for later.
                m_mapOfPointSensors[ps->getAddress()] = ps;

                // Initialize m_sensorBoardData data structure for this sensor.
                m_sensorBoardData.update(ps->getID(), -1);

                if (m_debug) {
                    cerr << "Registered point sensor " << ps->getName() << "(" << ps->getID() << ")" << ": " << ps->getAddress() << endl;
                }
            }
        }

        // Create UDP sender.
        core::wrapper::UDPSender *connectionToUDPServer = core::wrapper::UDPFactory::createUDPSender(host, server_port);

        // Create UDP receiver.
        core::wrapper::UDPReceiver *connectionFromUDPServer = core::wrapper::UDPFactory::createUDPReceiver(host, client_port);
        // Register ourselves as string listeners so that we will receive any data send from UDP_Server.
        connectionFromUDPServer->setStringListener(this);
        // Start receiving.
        connectionFromUDPServer->start();

        // Create LIFO data store to receive containers.
core::base::LIFOQueue lifo;
addDataStoreFor(lifo);

        string payload = "";
        string packet = "";

        // The following lines are used to open the GPIO file specified in the configuration file to test if the user pressed the button to start.
        istream *in = NULL;
        const int16_t USER_START_BUTTON_PRESSED_VALUE = 0; // if the user pressed the button the value changes to 0.
        bool userButtonAvailable = false;
        bool userButtonPressed = false;
        bool userButtonReleased = false;
        bool userButtonPressedTS_set = false;
        TimeStamp userButtonPressedTS;
        TimeStamp userButtonReleasedTS;
        double userButtonPressedDuration = 0;
        // Distinguish between driver & parkingDriver. ForceControl.getUserData() == 1000 --> Driver, ForceControl.getUserData() == 2000 --> ParkingDriver

        bool useForceControlDriver = false;
        bool useForceControlParkingDriver = false;
        URL url_start_button_file(USER_START_BUTTON_FILE);
        try {
            in = &(StreamFactory::getInstance().getInputStream(url_start_button_file));
            userButtonAvailable = true;
        } catch(InvalidArgumentException &iae) {
            // The specified user start button file could not be opened. Assuming that there is not such file available and start right ahead.
            userButtonAvailable = false;

            cerr << "(Proxy) warning: '" << iae.toString() << "'. Enabling direct pass-through WITHOUT FILTERING USERDATA (ie. NEVER RUN DRIVER && PARKINGDRIVER IN PARALLEL (only on PandaBoard)!!!" << endl;
        }

        // The following lines are used to open the GPIO file specified in the configuration file to control the brake LEDs.
        ostream *outBrakeLED = NULL;
        const string BRAKE_LED_OFF = "0"; // Value to turn off the brake LEDs.
        const string BRAKE_LED_ON = "1"; // Value to turn off the brake LEDs.
        URL url_brake_LED_file(BRAKE_LED_FILE);
        try {
            outBrakeLED = &(StreamFactory::getInstance().getOutputStream(url_brake_LED_file));
        } catch(InvalidArgumentException &iae) {
            // The specified file could not be opened. Assuming that there is not such file available.
            outBrakeLED = NULL;

            cerr << "(Proxy) warning: '" << iae.toString() << "'." << endl;
        }

        // The following lines are used to open the GPIO file specified in the configuration file to control the left turning LEDs.
        ostream *outTurnLeftLED = NULL;
        const string TURN_LEFT_LED_OFF = "0"; // Value to turn off the turn left LEDs.
        const string TURN_LEFT_LED_ON = "1"; // Value to turn off the turn left LEDs.
        URL url_turn_left_LED_file(TURN_LEFT_LED_FILE);
        try {
            outTurnLeftLED = &(StreamFactory::getInstance().getOutputStream(url_turn_left_LED_file));
        } catch(InvalidArgumentException &iae) {
            // The specified file could not be opened. Assuming that there is not such file available.
            outTurnLeftLED = NULL;

            cerr << "(Proxy) warning: '" << iae.toString() << "'." << endl;
        }
        uint32_t turnLeftLEDcounter = 0;
        const uint32_t turnLeftLEDcounterMax = ((uint32_t)(getFrequency()) < 2 ? 2 : (uint32_t)(getFrequency()));

        // The following lines are used to open the GPIO file specified in the configuration file to control the right turning LEDs.
        ostream *outTurnRightLED = NULL;
        const string TURN_RIGHT_LED_OFF = "0"; // Value to turn off the turn left LEDs.
        const string TURN_RIGHT_LED_ON = "1"; // Value to turn off the turn left LEDs.
        URL url_turn_right_LED_file(TURN_RIGHT_LED_FILE);
        try {
            outTurnRightLED = &(StreamFactory::getInstance().getOutputStream(url_turn_right_LED_file));
        } catch(InvalidArgumentException &iae) {
            // The specified file could not be opened. Assuming that there is not such file available.
            outTurnRightLED = NULL;

            cerr << "(Proxy) warning: '" << iae.toString() << "'." << endl;
        }
        uint32_t turnRightLEDcounter = 0;
        const uint32_t turnRightLEDcounterMax = ((uint32_t)(getFrequency()) < 2 ? 2 : (uint32_t)(getFrequency()));
        

        // This counter switches between the various requests from the car.
        uint32_t query_counter = 0;

        // State machine to switch between sending steering/acceleration packets and querying sensor values.
        bool querySensors = false;

        // Defining driving direction: +1 forwards, -1 backwards.
        double drivingDirection = 1;
        while (getModuleState() == ModuleState::RUNNING) {
            // Try to wait for the "start" signal: The file USER_START_BUTTON_FILE/value changes from 1 --> 0.
            if (userButtonAvailable) {
                if (in != NULL) {
                    if (in->good()) {
                        int16_t input;
                        *in >> input;
                        if (userButtonPressed && (USER_START_BUTTON_PRESSED_VALUE != input)) {
                            userButtonPressed = false;
                            userButtonReleased = true;
                            if (m_debug) {
                                cout << "button released!" << endl;
                            }
                        }

                        if (!userButtonPressed && (USER_START_BUTTON_PRESSED_VALUE == input)) {
                            userButtonPressed = true;
                            userButtonReleased = false;
                            if (m_debug) {
                                cout << "button pressed!" << endl;
                            }
                        }
                    }

                    // Clear any error states of the stream.
                    in->clear();
                    // Seek to the beginning of the input stream.
                    in->seekg(ios::beg);

                    if (userButtonPressed && !userButtonReleased && !userButtonPressedTS_set) {
                        userButtonPressedTS = TimeStamp();
                        userButtonPressedTS_set = true;

                        // Turn off the turn left LEDs.
                        if (outTurnLeftLED != NULL) {
                            *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                            outTurnLeftLED->flush();
                        }
                        // Turn off the turn right LEDs.
                        if (outTurnRightLED != NULL) {
                            *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                            outTurnRightLED->flush();
                        }
                        // Turn off the brake LEDs.
                        if (outBrakeLED != NULL) {
                            *outBrakeLED << BRAKE_LED_OFF << endl;
                            outBrakeLED->flush();
                        }
                    }
                    if (userButtonPressed && !userButtonReleased && userButtonPressedTS_set) {
                        TimeStamp userButtonCurrent;
                        double userButtonCurrentDuration = (userButtonCurrent - userButtonPressedTS).getSeconds();

                        if (userButtonCurrentDuration > 10.0) {
                            // Turn on the brake LEDs.
                            if (outBrakeLED != NULL) {
                                *outBrakeLED << BRAKE_LED_ON << endl;
                                outBrakeLED->flush();
                            }
                            Thread::usleep(2*1000*1000);
                            // Turn off the turn right LEDs.
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                                outTurnRightLED->flush();
                            }
                        }
                        else if (userButtonCurrentDuration > 3.0) {
                            // Turn on the turn right LEDs.
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_ON << endl;
                                outTurnRightLED->flush();
                            }

                            // Turn off the turn left LEDs.
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                                outTurnLeftLED->flush();
                            }
                        }
                        else if (userButtonCurrentDuration > 1.0) {
                            // Turn on the turn left LEDs.
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_ON << endl;
                                outTurnLeftLED->flush();
                            }
                        }
                    }
                    if (!userButtonPressed && userButtonReleased) {
                        userButtonReleasedTS = TimeStamp();

                        // Reset TS for button pressed.
                        userButtonPressedTS_set = false;

                        // Reset buttonReleased flag to measure again.
                        userButtonReleased = false;

                        userButtonPressedDuration = (userButtonReleasedTS - userButtonPressedTS).getSeconds();

                        if (userButtonPressedDuration > 10.0) {
                            useForceControlDriver = false;
                            useForceControlParkingDriver = false;

                            if (m_debug) {
                                cout << "User button released - pressed longer than 10s" << endl;
                            }

                            // Turn on the brake LEDs.
                            if (outBrakeLED != NULL) {
                                *outBrakeLED << BRAKE_LED_ON << endl;
                                outBrakeLED->flush();
                            }
                            Thread::usleep(2*1000*1000);
                            // Turn off the brake LEDs.
                            if (outBrakeLED != NULL) {
                                *outBrakeLED << BRAKE_LED_OFF << endl;
                                outBrakeLED->flush();
                            }
                        }
                        else if (userButtonPressedDuration > 3.0) {
                            useForceControlDriver = false;
                            useForceControlParkingDriver = true;

                            if (m_debug) {
                                cout << "User button released - pressed longer than 3s - using ParkingDriver" << endl;
                            }

                            // Turn on the turn right LEDs.
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_ON << endl;
                                outTurnRightLED->flush();
                            }
                            Thread::usleep(2*1000*1000);
                            // Turn off the turn right LEDs.
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                                outTurnRightLED->flush();
                            }
                        }
                        else if (userButtonPressedDuration > 1.0) {
                            useForceControlDriver = true;
                            useForceControlParkingDriver = false;

                            if (m_debug) {
                                cout << "User button released - pressed longer than 1s - using Driver" << endl;
                            }

                            // Turn on the turn left LEDs.
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_ON << endl;
                                outTurnLeftLED->flush();
                            }
                            Thread::usleep(2*1000*1000);
                            // Turn off the turn left LEDs.
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                                outTurnLeftLED->flush();
                            }
                        }
                    }
                }
            }


            if (userButtonAvailable) {
                if (!useForceControlDriver && useForceControlParkingDriver) {
                    cerr << "Alright! Parking - let's rock'n'roll." << endl;

                    goForward(connectionToUDPServer);

                    double oldDistance = -1;
                    bool measuring = false;
                    bool foundGap = false;
                    bool finish = false;
                    double tp1 = 0;
                    double tp2 = 0;
                    uint32_t stateMachine = 0;
                    bool flashing = false;
                    const uint32_t flashingMax = ((uint32_t)(getFrequency()) < 2 ? 2 : (uint32_t)(getFrequency()));
                    bool parkingSequenceSent = false;

                    while (!finish && (getModuleState() == ModuleState::RUNNING)) {
                        // Query position data from the vehicle.
                        payload = createPayloadForQueryPositionData();
                        packet = createPacket(payload);
                        connectionToUDPServer->send(packet);

                        // Query travel counter from the vehicle.
                        payload = createPayloadForReadTravelCounter(true);
                        packet = createPacket(payload);
                        connectionToUDPServer->send(packet);

                        // Query infra red values from the vehicle.
                        payload = createPayloadForQueryUltraSonicData();
                        packet = createPacket(payload);
                        connectionToUDPServer->send(packet);

                        if (stateMachine > 0) {
                            if (!measuring && (oldDistance > 0) && (m_sensorBoardData.getDistance(m_US_ID) < 0)) {
                                measuring = true;
//                                tp1 = m_vehiclePosition.getTraveledPath();
                            }
                            else if (measuring && (oldDistance < 0) && (m_sensorBoardData.getDistance(m_US_ID) > 0)) {
                                measuring = false;
//                                tp2 = m_vehiclePosition.getTraveledPath();

                                foundGap = true;
                            }

                            if (foundGap) {
                                foundGap = false;

                                cerr << "Found gap = " << (tp2 - tp1) << endl;
                                // Strategy one: skip really large spots.
//                                if ( ((tp2 - tp1) > 500) && ((tp2 - tp1) < 700) ) {
                                // Strategy two: Go for the first applicable one.
                                if ((tp2 - tp1) > 500) {

                                    // Turn on the brake LEDs.
                                    if (outBrakeLED != NULL) {
                                        *outBrakeLED << BRAKE_LED_ON << endl;
                                        outBrakeLED->flush();
                                    }

                                    if (!parkingSequenceSent) {
                                        parkTrajectoryNew(connectionToUDPServer);
                                        parkingSequenceSent = true;
                                    }
                                    flashing = true;
                                    stateMachine = 100;
                                }
                            }

                            oldDistance = m_sensorBoardData.getDistance(m_US_ID);

                            if (flashing) {
                                if (turnRightLEDcounter < (turnRightLEDcounterMax/2)) {
                                    if (outTurnRightLED != NULL) {
                                        *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                                        outTurnRightLED->flush();
                                    }
                                }
                                if (turnRightLEDcounter >= (turnRightLEDcounterMax/2)) {
                                    if (outTurnRightLED != NULL) {
                                        *outTurnRightLED << TURN_RIGHT_LED_ON << endl;
                                        outTurnRightLED->flush();
                                    }
                                // Turn off the brake LEDs.
                                if (outBrakeLED != NULL) {
                                cout << "Turn OFF brake lights." << endl;
                                    *outBrakeLED << BRAKE_LED_OFF << endl;
                                    outBrakeLED->flush();
                                }
                                }
                                turnRightLEDcounter++;
                                turnRightLEDcounter %= turnRightLEDcounterMax;

                                if (stateMachine > (3*flashingMax + 100)) {
                                    finish = true;
                                    // Turn on the brake LEDs.
                                    if (outBrakeLED != NULL) {
                                    cout << "Turn ON brake lights." << endl;
                                        *outBrakeLED << BRAKE_LED_ON << endl;
                                        outBrakeLED->flush();
                                    }
                                }
                            }
                        }

                        stateMachine++;

//                        cerr << m_vehiclePosition.getTraveledPath() << ": " << m_sensorBoardData.getDistance(m_US_ID) << endl;
//                        cerr << endl;
//                        cerr << endl;
                    }


                    // Parking finished.
                    useForceControlParkingDriver = false;
                }
            }

             // Switch between querying sensors and sending steering/acceleration values.
            if (!querySensors) {
                // Next cycle do the sensor querying.
                querySensors = !querySensors;

                bool foundFilteredData = false;
                ForceControl fc;

                // Regular data processing: try to find matching containers.
                while (!lifo.isEmpty() && !foundFilteredData) {
                    // Read next received container.
                    Container con = lifo.pop();

                    if (con.getDataType() == Container::FORCECONTROL) {
                        // Otherwise, unpack container.
                        fc = con.getData<ForceControl>();
                        if (!userButtonAvailable) {
                            foundFilteredData = true;
                        }
                        else {
                            // If userButton is available, check which type of ForceControl shall be used.
                            if (!useForceControlDriver && !useForceControlParkingDriver) {
                                // Case: button available but not pressed yet.
                                // Action: Discard everything and continue.
                                lifo.clear();
                                break;
                            }

                            if (useForceControlDriver && !useForceControlParkingDriver) {
                                // Case: button available and use ForceControl from Driver.
                                // Action: Check userData == 1000
//                                if (fc.getUserData() == 1000) {
//                                    foundFilteredData = true;
//                                }
                            }

                            if (!useForceControlDriver && useForceControlParkingDriver) {
                                // Case: button available and use ForceControl from ParkingDriver.
                                // Action: Check userData == 2000
//                                if (fc.getUserData() == 2000) {
//                                    foundFilteredData = true;
//                                }
                            }

                            if (useForceControlDriver && useForceControlParkingDriver) {
                                // Case: button available and use ForceControl from Driver && ParkingDriver ==> error
                                // Action: Message and do nothing.
                                if (m_debug) {
                                    cout << "Error! useForceControlDriver && useForceControlParkingDriver." << endl;
                                }

                                lifo.clear();
                                break;
                            }
                        }
                    }
                }

                if (foundFilteredData) {
                    // Translate to UDP_Server specific format.
                    if (m_debug) {
                        cout << "Received: " << fc.toString() << endl;
                    }

                    // Check if we need to decorate ourselves with the LEDs.
                    if (fc.getBrakeLights()) {
                        if (m_debug) {
                            cout << "Turn ON brake lights." << endl;
                        }
                        // Turn on the brake LEDs.
                        if (outBrakeLED != NULL) {
                            *outBrakeLED << BRAKE_LED_ON << endl;
                            outBrakeLED->flush();
                        }
                    }
                    else {
                        if (m_debug) {
                            cout << "Turn OFF brake lights." << endl;
                        }
                        // Turn off the brake LEDs.
                        if (outBrakeLED != NULL) {
                            *outBrakeLED << BRAKE_LED_OFF << endl;
                            outBrakeLED->flush();
                        }
                    }

                    if (fc.getLeftFlashingLights()) {
                        if (turnLeftLEDcounter < (turnLeftLEDcounterMax/2)) {
                            if (m_debug) {
                                cout << "Left flashing OFF." << endl;
                            }
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                                outTurnLeftLED->flush();
                            }
                        }
                        if (turnLeftLEDcounter >= (turnLeftLEDcounterMax/2)) {
                            if (m_debug) {
                                cout << "Left flashing ON." << endl;
                            }
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_ON << endl;
                                outTurnLeftLED->flush();
                            }
                        }
                        turnLeftLEDcounter++;
                        turnLeftLEDcounter %= turnLeftLEDcounterMax;
                    }
                    else {
                        if (m_debug) {
                            cout << "Turn OFF left flashing lights." << endl;
                        }
                        // Turn off the left turn LEDs.
                        if (outTurnLeftLED != NULL) {
                            *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                            outTurnLeftLED->flush();
                        }
                        turnLeftLEDcounter = 0;
                    }

                    if (fc.getRightFlashingLights()) {
                        if (turnRightLEDcounter < (turnRightLEDcounterMax/2)) {
                            if (m_debug) {
                                cout << "Right flashing OFF." << endl;
                            }
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                                outTurnRightLED->flush();
                            }
                        }
                        if (turnRightLEDcounter >= (turnRightLEDcounterMax/2)) {
                            if (m_debug) {
                                cout << "Right flashing ON." << endl;
                            }
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_ON << endl;
                                outTurnRightLED->flush();
                            }
                        }
                        turnRightLEDcounter++;
                        turnRightLEDcounter %= turnRightLEDcounterMax;
                    }
                    else {
                        if (m_debug) {
                            cout << "Turn OFF right flashing lights." << endl;
                        }
                        // Turn off the right turn LEDs.
                        if (outTurnRightLED != NULL) {
                            *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                            outTurnRightLED->flush();
                        }
                        turnRightLEDcounter = 0;
                    }

                    // Transform data from ForceControl to UDP_Server packet data.
                    // Transform fc.getSteeringForce() (-50 (max left) .. 0 (straight) .. 50 (max right)) to +255 (max left) .. 127 (straight) .. 0 (max right)
                    double steering = 0;
                    if (fabs(fc.getSteeringForce()) < 1e-2) {
                        // Straight forward.
                        steering = 127;
                    } else {
    steering = 127 - (fc.getSteeringForce() * 2.54);
    }

                    // Acceleration mapping: -127 .. -71 : backwards
                    // 71 .. 127 : forwards
                    double acceleration = 0;
                    if (fabs(fc.getBrakeForce()) > 5.9) {
                        acceleration = 0;
                    }
                    else {
                        acceleration = 80 - fabs(fc.getBrakeForce())/0.656;
                    }

                    // Determine driving direction but ONLY if we are standing still.
                    if (fabs(acceleration) < 1e-2) {
                        // If fc.getAccelerationForce() is negative, drive backwards, otherwise forwards.
                        drivingDirection = (fc.getAccelerationForce() < 0 ? -1 : +1);
                    }

                    if (fabs(acceleration) > ACCELERATION_HYSTERESIS) {
                        acceleration = (fabs(acceleration) - ACCELERATION_HYSTERESIS) * drivingDirection;
                    }
                    else {
                        acceleration = 0;
                    }
                    if (m_debug) {
                        cout << "acceleration: " << acceleration << endl;	
                    }

                    double speed = (ACCELERATION_MAX * acceleration) / (127.0 - ACCELERATION_HYSTERESIS);
speed=0.8;
                    // Create a sendable packet.
                    payload = createPayloadForSettingAccelerationAndSteering(speed, (uint8_t)steering);
                    packet = createPacket(payload);

                    connectionToUDPServer->send(packet);
                }
            }
            else {
           // Next cycle do the steering/acceleration sending.
           querySensors = !querySensors;

            // Now, query values from the UDP_Server: one at a time.
            query_counter++;
            switch (query_counter) {
                case 1:
                    // Query values from the vehicle.
                    payload = createPayloadForQueryOnboardData();
                break;

                case 2:
                    // Query ultra sonic values from the vehicle.
                    payload = createPayloadForQueryUltraSonicData();
                break;

                case 3:
                    // Query infra red values from the vehicle.
                    payload = createPayloadForQueryInfraRedData();
                break;

                case 4:
                    // Query position data from the vehicle.
                    payload = createPayloadForQueryPositionData();

                    // Reset counter. This call must be carried out in the last case of this switch statement.
                    query_counter = 0;
                break;

                default:
                    query_counter = 0;
            }

            // Send packet to car and wait for reply.
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);
}
}
//Stop the car after stopping the Proxy
        const double STEERING_STRAIGHT = 127;
        const double SPEED_0 = 0;
payload = createPayloadForSettingAccelerationAndSteering(SPEED_0 , STEERING_STRAIGHT);
packet = createPacket(payload);
connectionToUDPServer->send(packet);

        // Destroy connections to UDP_Server.
        OPENDAVINCI_CORE_DELETE_POINTER(connectionToUDPServer);

        // Stop receiving.
        connectionFromUDPServer->stop();
        connectionFromUDPServer->setStringListener(NULL);
        OPENDAVINCI_CORE_DELETE_POINTER(connectionFromUDPServer);

return ModuleState::OKAY;
    }




    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE Proxy::bodyParkingOnly() {
        // Get configuration data.
        KeyValueConfiguration kv = getKeyValueConfiguration();
        m_debug = kv.getValue<uint32_t>("proxy.debug") == 1;
        const string host = kv.getValue<string>("proxy.host");
        const uint32_t server_port = kv.getValue<uint32_t>("proxy.port");
        const uint32_t client_port = server_port + 1;

        const string USER_START_BUTTON_FILE = kv.getValue<string>("proxy.user_start_button_file");
        const string BRAKE_LED_FILE = kv.getValue<string>("proxy.GPIO_brakeLED_file");
        const string TURN_LEFT_LED_FILE = kv.getValue<string>("proxy.GPIO_turnLeftLED_file");
        const string TURN_RIGHT_LED_FILE = kv.getValue<string>("proxy.GPIO_turnRightLED_file");

        // Setup all point sensors.
        for (uint32_t i = 0; i < getKeyValueConfiguration().getValue<uint32_t>("proxy.numberOfSensors"); i++) {
            stringstream sensorID;
            sensorID << "proxy.sensor" << i << ".id";
            uint16_t id(getKeyValueConfiguration().getValue<uint16_t>(sensorID.str()));

            stringstream sensorName;
            sensorName << "proxy.sensor" << i << ".name";
            string name(getKeyValueConfiguration().getValue<string>(sensorName.str()));

            stringstream sensorAddress;
            sensorAddress << "proxy.sensor" << i << ".address";
            uint16_t address(getKeyValueConfiguration().getValue<uint16_t>(sensorAddress.str()));

            stringstream sensorClampDistance;
            sensorClampDistance << "proxy.sensor" << i << ".clampDistance";
            double clampDistance(getKeyValueConfiguration().getValue<double>(sensorClampDistance.str()));

            PointSensor *ps = new PointSensor(id, name, address, clampDistance);

            if (ps != NULL) {
                // Save for later.
                m_mapOfPointSensors[ps->getAddress()] = ps;

                // Initialize m_sensorBoardData data structure for this sensor.
                m_sensorBoardData.update(ps->getID(), -1);

                if (m_debug) {
                    cerr << "Registered point sensor " << ps->getName() << "(" << ps->getID() << ")" << ": " << ps->getAddress() << endl;
                }
            }
        }

        // Create UDP sender.
        core::wrapper::UDPSender *connectionToUDPServer = core::wrapper::UDPFactory::createUDPSender(host, server_port);

        // Create UDP receiver.
        core::wrapper::UDPReceiver *connectionFromUDPServer = core::wrapper::UDPFactory::createUDPReceiver(host, client_port);
        // Register ourselves as string listeners so that we will receive any data send from UDP_Server.
        connectionFromUDPServer->setStringListener(this);
        // Start receiving.
        connectionFromUDPServer->start();

        goForward(connectionToUDPServer);

        string payload = "";
        string packet = "";

        // Which sensor to use.
        uint32_t US_ID = 4;

        double oldDistance = -1;
        bool measuring = false;
        bool foundGap = false;
        bool finish = false;
        double tp1 = 0;
        double tp2 = 0;
        int stateMachine = 0;

        while (!finish && (getModuleState() == ModuleState::RUNNING)) {
            // Query position data from the vehicle.
            payload = createPayloadForQueryPositionData();
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

            // Query travel counter from the vehicle.
            payload = createPayloadForReadTravelCounter(true);
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

            // Query infra red values from the vehicle.
            payload = createPayloadForQueryUltraSonicData();
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);

            if (stateMachine > 0) {
                if (!measuring && (oldDistance > 0) && (m_sensorBoardData.getDistance(US_ID) < 0)) {
                    measuring = true;
//                    tp1 = m_vehiclePosition.getTraveledPath();
                }
                else if (measuring && (oldDistance < 0) && (m_sensorBoardData.getDistance(US_ID) > 0)) {
                    measuring = false;
//                    tp2 = m_vehiclePosition.getTraveledPath();

                    foundGap = true;
                }

                if (foundGap) {
                    foundGap = false;

                    cerr << "Found gap = " << (tp2 - tp1) << endl;
                    // Strategy one: skip really large spots.
                    if ( ((tp2 - tp1) > 500) && ((tp2 - tp1) < 700) ) {
                    // Strategy two: Go for the first applicable one.
//                    if ((tp2 - tp1) > 500) {
                        parkTrajectoryNew(connectionToUDPServer);
                        finish = true;
                    }
                }

                oldDistance = m_sensorBoardData.getDistance(US_ID);
            }

            stateMachine++;

//            cerr << m_vehiclePosition.getTraveledPath() << ": " << m_sensorBoardData.getDistance(US_ID) << endl;
            cerr << endl;
            cerr << endl;
        }


        // Destroy connections to UDP_Server.
        OPENDAVINCI_CORE_DELETE_POINTER(connectionToUDPServer);

        // Stop receiving.
        connectionFromUDPServer->stop();
        connectionFromUDPServer->setStringListener(NULL);
        OPENDAVINCI_CORE_DELETE_POINTER(connectionFromUDPServer);

return ModuleState::OKAY;
    }

    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE Proxy::bodyOrg() {
        // Get configuration data.
        KeyValueConfiguration kv = getKeyValueConfiguration();
        m_debug = kv.getValue<uint32_t>("proxy.debug") == 1;
        const string host = kv.getValue<string>("proxy.host");
        const uint32_t server_port = kv.getValue<uint32_t>("proxy.port");
        const uint32_t client_port = server_port + 1;

// const int32_t STEERING_CENTER = kv.getValue<int32_t>("proxy.steering.center");
// const int32_t STEERING_MIN = kv.getValue<int32_t>("proxy.steering.min");
// const int32_t STEERING_MAX = kv.getValue<int32_t>("proxy.steering.max");
// const bool STEERING_INVERT = kv.getValue<int32_t>("proxy.steering.invert") == 1;

        const int32_t ACCELERATION_HYSTERESIS = kv.getValue<int32_t>("proxy.acceleration.hysteresis");
        const double ACCELERATION_MAX = kv.getValue<double>("proxy.acceleration.max");

        const string USER_START_BUTTON_FILE = kv.getValue<string>("proxy.user_start_button_file");
        const string BRAKE_LED_FILE = kv.getValue<string>("proxy.GPIO_brakeLED_file");
        const string TURN_LEFT_LED_FILE = kv.getValue<string>("proxy.GPIO_turnLeftLED_file");
        const string TURN_RIGHT_LED_FILE = kv.getValue<string>("proxy.GPIO_turnRightLED_file");

        // Setup all point sensors.
        for (uint32_t i = 0; i < getKeyValueConfiguration().getValue<uint32_t>("proxy.numberOfSensors"); i++) {
            stringstream sensorID;
            sensorID << "proxy.sensor" << i << ".id";
            uint16_t id(getKeyValueConfiguration().getValue<uint16_t>(sensorID.str()));

            stringstream sensorName;
            sensorName << "proxy.sensor" << i << ".name";
            string name(getKeyValueConfiguration().getValue<string>(sensorName.str()));

            stringstream sensorAddress;
            sensorAddress << "proxy.sensor" << i << ".address";
            uint16_t address(getKeyValueConfiguration().getValue<uint16_t>(sensorAddress.str()));

            stringstream sensorClampDistance;
            sensorClampDistance << "proxy.sensor" << i << ".clampDistance";
            double clampDistance(getKeyValueConfiguration().getValue<double>(sensorClampDistance.str()));

            PointSensor *ps = new PointSensor(id, name, address, clampDistance);

            if (ps != NULL) {
                // Save for later.
                m_mapOfPointSensors[ps->getAddress()] = ps;

                // Initialize m_sensorBoardData data structure for this sensor.
                m_sensorBoardData.update(ps->getID(), -1);

                if (m_debug) {
                    cerr << "Registered point sensor " << ps->getName() << "(" << ps->getID() << ")" << ": " << ps->getAddress() << endl;
                }
            }
        }

        // Create UDP sender.
        core::wrapper::UDPSender *connectionToUDPServer = core::wrapper::UDPFactory::createUDPSender(host, server_port);

        // Create UDP receiver.
        core::wrapper::UDPReceiver *connectionFromUDPServer = core::wrapper::UDPFactory::createUDPReceiver(host, client_port);
        // Register ourselves as string listeners so that we will receive any data send from UDP_Server.
        connectionFromUDPServer->setStringListener(this);
        // Start receiving.
        connectionFromUDPServer->start();

        // Create LIFO data store to receive containers.
core::base::LIFOQueue lifo;
addDataStoreFor(lifo);

        string payload = "";
        string packet = "";

        // The following lines are used to open the GPIO file specified in the configuration file to test if the user pressed the button to start.
        istream *in = NULL;
        const int16_t USER_START_BUTTON_PRESSED_VALUE = 0; // if the user pressed the button the value changes to 0.
        bool userButtonAvailable = false;
        bool userButtonPressed = false;
        bool userButtonReleased = false;
        bool userButtonPressedTS_set = false;
        TimeStamp userButtonPressedTS;
        TimeStamp userButtonReleasedTS;
        double userButtonPressedDuration = 0;
        // Distinguish between driver & parkingDriver. ForceControl.getUserData() == 1000 --> Driver, ForceControl.getUserData() == 2000 --> ParkingDriver

        bool useForceControlDriver = false;
        bool useForceControlParkingDriver = false;
        URL url_start_button_file(USER_START_BUTTON_FILE);
        try {
            in = &(StreamFactory::getInstance().getInputStream(url_start_button_file));
            userButtonAvailable = true;
        } catch(InvalidArgumentException &iae) {
            // The specified user start button file could not be opened. Assuming that there is not such file available and start right ahead.
            userButtonAvailable = false;

            cerr << "(Proxy) warning: '" << iae.toString() << "'. Enabling direct pass-through WITHOUT FILTERING USERDATA (ie. NEVER RUN DRIVER && PARKINGDRIVER IN PARALLEL (only on PandaBoard)!!!" << endl;
        }

        // The following lines are used to open the GPIO file specified in the configuration file to control the brake LEDs.
        ostream *outBrakeLED = NULL;
        const string BRAKE_LED_OFF = "0"; // Value to turn off the brake LEDs.
        const string BRAKE_LED_ON = "1"; // Value to turn off the brake LEDs.
        URL url_brake_LED_file(BRAKE_LED_FILE);
        try {
            outBrakeLED = &(StreamFactory::getInstance().getOutputStream(url_brake_LED_file));
        } catch(InvalidArgumentException &iae) {
            // The specified file could not be opened. Assuming that there is not such file available.
            outBrakeLED = NULL;

            cerr << "(Proxy) warning: '" << iae.toString() << "'." << endl;
        }

        // The following lines are used to open the GPIO file specified in the configuration file to control the left turning LEDs.
        ostream *outTurnLeftLED = NULL;
        const string TURN_LEFT_LED_OFF = "0"; // Value to turn off the turn left LEDs.
        const string TURN_LEFT_LED_ON = "1"; // Value to turn off the turn left LEDs.
        URL url_turn_left_LED_file(TURN_LEFT_LED_FILE);
        try {
            outTurnLeftLED = &(StreamFactory::getInstance().getOutputStream(url_turn_left_LED_file));
        } catch(InvalidArgumentException &iae) {
            // The specified file could not be opened. Assuming that there is not such file available.
            outTurnLeftLED = NULL;

            cerr << "(Proxy) warning: '" << iae.toString() << "'." << endl;
        }
        uint32_t turnLeftLEDcounter = 0;
        const uint32_t turnLeftLEDcounterMax = ((uint32_t)(getFrequency()) < 2 ? 2 : (uint32_t)(getFrequency()));

        // The following lines are used to open the GPIO file specified in the configuration file to control the right turning LEDs.
        ostream *outTurnRightLED = NULL;
        const string TURN_RIGHT_LED_OFF = "0"; // Value to turn off the turn left LEDs.
        const string TURN_RIGHT_LED_ON = "1"; // Value to turn off the turn left LEDs.
        URL url_turn_right_LED_file(TURN_RIGHT_LED_FILE);
        try {
            outTurnRightLED = &(StreamFactory::getInstance().getOutputStream(url_turn_right_LED_file));
        } catch(InvalidArgumentException &iae) {
            // The specified file could not be opened. Assuming that there is not such file available.
            outTurnRightLED = NULL;

            cerr << "(Proxy) warning: '" << iae.toString() << "'." << endl;
        }
        uint32_t turnRightLEDcounter = 0;
        const uint32_t turnRightLEDcounterMax = ((uint32_t)(getFrequency()) < 2 ? 2 : (uint32_t)(getFrequency()));
        

        // This counter switches between the various requests from the car.
        uint32_t query_counter = 0;

        // State machine to switch between sending steering/acceleration packets and querying sensor values.
        bool querySensors = false;

        // Defining driving direction: +1 forwards, -1 backwards.
        double drivingDirection = 1;
        while (getModuleState() == ModuleState::RUNNING) {
            // Try to wait for the "start" signal: The file USER_START_BUTTON_FILE/value changes from 1 --> 0.
            if (userButtonAvailable) {
                if (in != NULL) {
                    if (in->good()) {
                        int16_t input;
                        *in >> input;
                        if (userButtonPressed && (USER_START_BUTTON_PRESSED_VALUE != input)) {
                            userButtonPressed = false;
                            userButtonReleased = true;
                            if (m_debug) {
                                cout << "button released!" << endl;
                            }
                        }

                        if (!userButtonPressed && (USER_START_BUTTON_PRESSED_VALUE == input)) {
                            userButtonPressed = true;
                            userButtonReleased = false;
                            if (m_debug) {
                                cout << "button pressed!" << endl;
                            }
                        }
                    }

                    // Clear any error states of the stream.
                    in->clear();
                    // Seek to the beginning of the input stream.
                    in->seekg(ios::beg);

                    if (userButtonPressed && !userButtonReleased && !userButtonPressedTS_set) {
                        userButtonPressedTS = TimeStamp();
                        userButtonPressedTS_set = true;

                        // Turn off the turn left LEDs.
                        if (outTurnLeftLED != NULL) {
                            *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                            outTurnLeftLED->flush();
                        }
                        // Turn off the turn right LEDs.
                        if (outTurnRightLED != NULL) {
                            *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                            outTurnRightLED->flush();
                        }
                        // Turn off the brake LEDs.
                        if (outBrakeLED != NULL) {
                            *outBrakeLED << BRAKE_LED_OFF << endl;
                            outBrakeLED->flush();
                        }
                    }
                    if (userButtonPressed && !userButtonReleased && userButtonPressedTS_set) {
                        TimeStamp userButtonCurrent;
                        double userButtonCurrentDuration = (userButtonCurrent - userButtonPressedTS).getSeconds();

                        if (userButtonCurrentDuration > 10.0) {
                            // Turn on the brake LEDs.
                            if (outBrakeLED != NULL) {
                                *outBrakeLED << BRAKE_LED_ON << endl;
                                outBrakeLED->flush();
                            }
                            Thread::usleep(2*1000*1000);
                            // Turn off the turn right LEDs.
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                                outTurnRightLED->flush();
                            }
                        }
                        else if (userButtonCurrentDuration > 3.0) {
                            // Turn on the turn right LEDs.
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_ON << endl;
                                outTurnRightLED->flush();
                            }

                            // Turn off the turn left LEDs.
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                                outTurnLeftLED->flush();
                            }
                        }
                        else if (userButtonCurrentDuration > 1.0) {
                            // Turn on the turn left LEDs.
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_ON << endl;
                                outTurnLeftLED->flush();
                            }
                        }
                    }
                    if (!userButtonPressed && userButtonReleased) {
                        userButtonReleasedTS = TimeStamp();

                        // Reset TS for button pressed.
                        userButtonPressedTS_set = false;

                        // Reset buttonReleased flag to measure again.
                        userButtonReleased = false;

                        userButtonPressedDuration = (userButtonReleasedTS - userButtonPressedTS).getSeconds();

                        if (userButtonPressedDuration > 10.0) {
                            useForceControlDriver = false;
                            useForceControlParkingDriver = false;

                            if (m_debug) {
                                cout << "User button released - pressed longer than 10s" << endl;
                            }

                            // Turn on the brake LEDs.
                            if (outBrakeLED != NULL) {
                                *outBrakeLED << BRAKE_LED_ON << endl;
                                outBrakeLED->flush();
                            }
                            Thread::usleep(2*1000*1000);
                            // Turn off the brake LEDs.
                            if (outBrakeLED != NULL) {
                                *outBrakeLED << BRAKE_LED_OFF << endl;
                                outBrakeLED->flush();
                            }
                        }
                        else if (userButtonPressedDuration > 3.0) {
                            useForceControlDriver = false;
                            useForceControlParkingDriver = true;

                            if (m_debug) {
                                cout << "User button released - pressed longer than 3s - using ParkingDriver" << endl;
                            }

                            // Turn on the turn right LEDs.
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_ON << endl;
                                outTurnRightLED->flush();
                            }
                            Thread::usleep(2*1000*1000);
                            // Turn off the turn right LEDs.
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                                outTurnRightLED->flush();
                            }
                        }
                        else if (userButtonPressedDuration > 1.0) {
                            useForceControlDriver = true;
                            useForceControlParkingDriver = false;

                            if (m_debug) {
                                cout << "User button released - pressed longer than 1s - using Driver" << endl;
                            }

                            // Turn on the turn left LEDs.
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_ON << endl;
                                outTurnLeftLED->flush();
                            }
                            Thread::usleep(2*1000*1000);
                            // Turn off the turn left LEDs.
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                                outTurnLeftLED->flush();
                            }
                        }
                    }
                }
            }

             // Switch between querying sensors and sending steering/acceleration values.
            if (!querySensors) {
                // Next cycle do the sensor querying.
                querySensors = !querySensors;

                bool foundFilteredData = false;
                ForceControl fc;

                // Regular data processing: try to find matching containers.
                while (!lifo.isEmpty() && !foundFilteredData) {
                    // Read next received container.
                    Container con = lifo.pop();

                    if (con.getDataType() == Container::FORCECONTROL) {
                        // Otherwise, unpack container.
                        fc = con.getData<ForceControl>();
                        if (!userButtonAvailable) {
                            foundFilteredData = true;
                        }
                        else {
                            // If userButton is available, check which type of ForceControl shall be used.
                            if (!useForceControlDriver && !useForceControlParkingDriver) {
                                // Case: button available but not pressed yet.
                                // Action: Discard everything and continue.
                                lifo.clear();
                                break;
                            }

                            if (useForceControlDriver && !useForceControlParkingDriver) {
                                // Case: button available and use ForceControl from Driver.
                                // Action: Check userData == 1000
//                                if (fc.getUserData() == 1000) {
//                                    foundFilteredData = true;
//                                }
                            }

                            if (!useForceControlDriver && useForceControlParkingDriver) {
                                // Case: button available and use ForceControl from ParkingDriver.
                                // Action: Check userData == 2000
//                                if (fc.getUserData() == 2000) {
//                                    foundFilteredData = true;
//                                }
                            }

                            if (useForceControlDriver && useForceControlParkingDriver) {
                                // Case: button available and use ForceControl from Driver && ParkingDriver ==> error
                                // Action: Message and do nothing.
                                if (m_debug) {
                                    cout << "Error! useForceControlDriver && useForceControlParkingDriver." << endl;
                                }

                                lifo.clear();
                                break;
                            }
                        }
                    }
                }

                if (foundFilteredData) {
                    // Translate to UDP_Server specific format.
                    if (m_debug) {
                        cout << "Received: " << fc.toString() << endl;
                    }

                    // Check if we need to decorate ourselves with the LEDs.
                    if (fc.getBrakeLights()) {
                        if (m_debug) {
                            cout << "Turn ON brake lights." << endl;
                        }
                        // Turn on the brake LEDs.
                        if (outBrakeLED != NULL) {
                            *outBrakeLED << BRAKE_LED_ON << endl;
                            outBrakeLED->flush();
                        }
                    }
                    else {
                        if (m_debug) {
                            cout << "Turn OFF brake lights." << endl;
                        }
                        // Turn off the brake LEDs.
                        if (outBrakeLED != NULL) {
                            *outBrakeLED << BRAKE_LED_OFF << endl;
                            outBrakeLED->flush();
                        }
                    }

                    if (fc.getLeftFlashingLights()) {
                        if (turnLeftLEDcounter < (turnLeftLEDcounterMax/2)) {
                            if (m_debug) {
                                cout << "Left flashing OFF." << endl;
                            }
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                                outTurnLeftLED->flush();
                            }
                        }
                        if (turnLeftLEDcounter >= (turnLeftLEDcounterMax/2)) {
                            if (m_debug) {
                                cout << "Left flashing ON." << endl;
                            }
                            if (outTurnLeftLED != NULL) {
                                *outTurnLeftLED << TURN_LEFT_LED_ON << endl;
                                outTurnLeftLED->flush();
                            }
                        }
                        turnLeftLEDcounter++;
                        turnLeftLEDcounter %= turnLeftLEDcounterMax;
                    }
                    else {
                        if (m_debug) {
                            cout << "Turn OFF left flashing lights." << endl;
                        }
                        // Turn off the left turn LEDs.
                        if (outTurnLeftLED != NULL) {
                            *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                            outTurnLeftLED->flush();
                        }
                        turnLeftLEDcounter = 0;
                    }

                    if (fc.getRightFlashingLights()) {
                        if (turnRightLEDcounter < (turnRightLEDcounterMax/2)) {
                            if (m_debug) {
                                cout << "Right flashing OFF." << endl;
                            }
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                                outTurnRightLED->flush();
                            }
                        }
                        if (turnRightLEDcounter >= (turnRightLEDcounterMax/2)) {
                            if (m_debug) {
                                cout << "Right flashing ON." << endl;
                            }
                            if (outTurnRightLED != NULL) {
                                *outTurnRightLED << TURN_RIGHT_LED_ON << endl;
                                outTurnRightLED->flush();
                            }
                        }
                        turnRightLEDcounter++;
                        turnRightLEDcounter %= turnRightLEDcounterMax;
                    }
                    else {
                        if (m_debug) {
                            cout << "Turn OFF right flashing lights." << endl;
                        }
                        // Turn off the right turn LEDs.
                        if (outTurnRightLED != NULL) {
                            *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                            outTurnRightLED->flush();
                        }
                        turnRightLEDcounter = 0;
                    }

                    // Transform data from ForceControl to UDP_Server packet data.
                    // Transform fc.getSteeringForce() (-50 (max left) .. 0 (straight) .. 50 (max right)) to +255 (max left) .. 127 (straight) .. 0 (max right)
                    double steering = 0;
                    if (fabs(fc.getSteeringForce()) < 1e-2) {
                        // Straight forward.
                        steering = 127;
                    } else {
    steering = 127 - (fc.getSteeringForce() * 2.54);
    }

                    // Acceleration mapping: -127 .. -71 : backwards
                    // 71 .. 127 : forwards
                    double acceleration = 0;
                    if (fabs(fc.getBrakeForce()) > 5.9) {
                        acceleration = 0;
                    }
                    else {
                        acceleration = 80 - fabs(fc.getBrakeForce())/0.656;
                    }

                    // Determine driving direction but ONLY if we are standing still.
                    if (fabs(acceleration) < 1e-2) {
                        // If fc.getAccelerationForce() is negative, drive backwards, otherwise forwards.
                        drivingDirection = (fc.getAccelerationForce() < 0 ? -1 : +1);
                    }

                    if (fabs(acceleration) > ACCELERATION_HYSTERESIS) {
                        acceleration = (fabs(acceleration) - ACCELERATION_HYSTERESIS) * drivingDirection;
                    }
                    else {
                        acceleration = 0;
                    }
                    if (m_debug) {
                        cout << "acceleration: " << acceleration << endl;	
                    }

                    double speed = (ACCELERATION_MAX * acceleration) / (127.0 - ACCELERATION_HYSTERESIS);

                    // Create a sendable packet.
                    payload = createPayloadForSettingAccelerationAndSteering(speed, (uint8_t)steering);
                    packet = createPacket(payload);

                    connectionToUDPServer->send(packet);
                }
            }
            else {
           // Next cycle do the steering/acceleration sending.
           querySensors = !querySensors;

            // Now, query values from the UDP_Server: one at a time.
            query_counter++;
            switch (query_counter) {
                case 1:
                    // Query values from the vehicle.
                    payload = createPayloadForQueryOnboardData();
                break;

                case 2:
                    // Query ultra sonic values from the vehicle.
                    payload = createPayloadForQueryUltraSonicData();
                break;

                case 3:
                    // Query infra red values from the vehicle.
                    payload = createPayloadForQueryInfraRedData();
                break;

                case 4:
                    // Query position data from the vehicle.
                    payload = createPayloadForQueryPositionData();

                    // Reset counter. This call must be carried out in the last case of this switch statement.
                    query_counter = 0;
                break;

                default:
                    query_counter = 0;
            }

            // Send packet to car and wait for reply.
            packet = createPacket(payload);
            connectionToUDPServer->send(packet);
}
}
//Stop the car after stopping the Proxy
        const double STEERING_STRAIGHT = 127;
        const double SPEED_0 = 0;
payload = createPayloadForSettingAccelerationAndSteering(SPEED_0 , STEERING_STRAIGHT);
packet = createPacket(payload);
connectionToUDPServer->send(packet);

        // Destroy connections to UDP_Server.
        OPENDAVINCI_CORE_DELETE_POINTER(connectionToUDPServer);

        // Stop receiving.
        connectionFromUDPServer->stop();
        connectionFromUDPServer->setStringListener(NULL);
        OPENDAVINCI_CORE_DELETE_POINTER(connectionFromUDPServer);

return ModuleState::OKAY;
    }


} // carolocup
