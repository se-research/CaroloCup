/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <string>

#include "core/base/Lock.h"
#include "core/data/Constants.h"

#include "STM32F4Protocol.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::data::environment;

    STM32F4Protocol::STM32F4Protocol() :
        AbstractProtocol(),
        m_partialData(),
        m_dataListenerMutex(),
        m_dataListener(NULL),
        m_vehicleData() {}

    STM32F4Protocol::~STM32F4Protocol() {
        setSTM32F4DataListener(NULL);
    }
	std::string itos(int n)
	{
	   const int max_size = std::numeric_limits<int>::digits10 + 1 /*sign*/ + 1 /*0-terminator*/;
	   char buffer[max_size] = {0};
	   sprintf(buffer, "%d", n);
	   return std::string(buffer);
	}

    void STM32F4Protocol::setSTM32F4DataListener(STM32F4DataListener *listener) {
        Lock l(m_dataListenerMutex);
        m_dataListener = listener;
    }

    void STM32F4Protocol::request(const STM32F4Request &r, const double &speed, const double &steeringWheelAngle) {
        cout << "(STM32F4Protocol): request: " << r << ", speed: " << speed << ", steeringWheelAngle: " << steeringWheelAngle << endl;

        double tmpSpeed = speed;
        if (speed < -2.0) {
            tmpSpeed = -2.0;
        }
        if (speed > 2.0) {
            tmpSpeed = 2.0;
        }
        const double SPEED_RANGE = 2.0 * 2;
        const double BITS = 63;
        uint16_t mappedSpeed = (uint8_t)((tmpSpeed + 2.0)/(SPEED_RANGE/BITS));
            
        double tmpSteeringWheelAngle = steeringWheelAngle;
        if (steeringWheelAngle < -28.0) {
            tmpSteeringWheelAngle = -28.0;
        }
        if (steeringWheelAngle > 28.0) {
            tmpSteeringWheelAngle = 28.0;
        }
        const double STEERINGWHEELANGLE_RANGE = 28.0 * 2;
        uint16_t mappedSteeringWheelAngle = (uint8_t)((tmpSteeringWheelAngle + 2.0)/(STEERINGWHEELANGLE_RANGE/BITS));

        uint16_t data = 0;
        data = r | (mappedSpeed << 4) | (mappedSteeringWheelAngle << 10);
	
	sendByStringSender(itos(data));
    }

    void STM32F4Protocol::receivedPartialString(const string &s) {
        m_partialData.write(s.c_str(), s.length());

        // One packet has the size of 4 bytes.
        const uint32_t PACKET_SIZE = sizeof(uint32_t);

        // Get the size of received data.
        m_partialData.seekg(0, ios_base::end);
        uint32_t streamSize = m_partialData.tellg();
        m_partialData.seekg(0, ios_base::beg);

        // Decode as much as possible from the currently received data (currently assuming that we will not loose any data).
        while (streamSize >= PACKET_SIZE) {
            // Read next packet.
            uint32_t nextPacket = 0;
            m_partialData.read(reinterpret_cast<char*>(&nextPacket), sizeof(uint32_t));
            nextPacket = ntohl(nextPacket);

            // TODO: Ensure that we have a valid packet (request header & packet terminator).

            handlePacket(nextPacket);

            streamSize -= PACKET_SIZE;
        }
        // Put the write pointer to the end of the stream.
        m_partialData.seekp(0, ios_base::end);
    }

    uint32_t STM32F4Protocol::getValueFromPacket(const uint32_t &packet, const uint32_t &startBit, const uint32_t &length) const {
        uint32_t retVal = 0;
        if ( (startBit < 32) && ((startBit + length) < 33) ) {
            retVal = (packet >> startBit) & (uint32_t)(pow(2, length) - 1);
        }
        return retVal;
    }

    void STM32F4Protocol::handlePacket(const uint32_t &packet) {
        STM32F4Request r = (STM32F4Request)getValueFromPacket(packet, 0, 4);
        uint32_t packetTerminator = getValueFromPacket(packet, 30, 2);

        cout << "(STM32F4Protocol): request: " << (uint32_t)(r) << ", packetTerminator: " << packetTerminator << endl;

        switch (r) {
            case NoData:
                cout << "(STM32F4Protocol): NoData" << endl;
            break;
            case AllInfrareds:
            {
                cout << "(STM32F4Protocol): AllInfrareds" << endl;
                uint32_t IR1 = getValueFromPacket(packet, 4, 5);
                uint32_t IR2 = getValueFromPacket(packet, 9, 5);
                uint32_t IR3 = getValueFromPacket(packet, 14, 5);

                cout << "(STM32F4Protocol): IR1: " << IR1 << ", IR2: " << IR2 << ", IR3: " << IR3 << endl;

                InfraredSensorMeasurement i1;
                i1.address = 200;
                i1.value = (IR1 == 31) ? -1 : (int32_t)(IR1);

                InfraredSensorMeasurement i2;
                i2.address = 201;
                i2.value = (IR2 == 31) ? -1 : (int32_t)(IR2);

                InfraredSensorMeasurement i3;
                i3.address = 202;
                i3.value = (IR3 == 31) ? -1 : (int32_t)(IR3);

                vector<InfraredSensorMeasurement> listOfIRMeasurements;
                listOfIRMeasurements.push_back(i1);
                listOfIRMeasurements.push_back(i2);
                listOfIRMeasurements.push_back(i3);

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(listOfIRMeasurements);
                    }
                }
            }
            break;
            case AllUltrasonics:
            {
                cout << "(STM32F4Protocol): AllUltrasonics" << endl;
                uint32_t US1 = getValueFromPacket(packet, 4, 6);
                uint32_t US1_multiplier = getValueFromPacket(packet, 10, 2);

                uint32_t US2 = getValueFromPacket(packet, 12, 6);
                uint32_t US2_multiplier = getValueFromPacket(packet, 18, 2);

                uint32_t US3 = getValueFromPacket(packet, 20, 6);
                uint32_t US3_multiplier = getValueFromPacket(packet, 26, 2);

                cout << "(STM32F4Protocol): US1: " << US1 << " (multiplier: " << US1_multiplier << "), "
                     << "US2: " << US2 << " (multiplier: " << US2_multiplier << "), " 
                     << "US3: " << US3 << " (multiplier: " << US3_multiplier << ") " << endl;

                UltrasonicSensorMeasurement u1;
                u1.address = 100;
                u1.value = US1 * getMultiplier(US1_multiplier);
                u1.value = (u1.value == 630) ? -1 : u1.value;

                UltrasonicSensorMeasurement u2;
                u2.address = 101;
                u2.value = US2 * getMultiplier(US2_multiplier);
                u2.value = (u2.value == 630) ? -1 : u2.value;

                UltrasonicSensorMeasurement u3;
                u3.address = 102;
                u3.value = US3 * getMultiplier(US3_multiplier);
                u3.value = (u3.value == 630) ? -1 : u3.value;

                vector<UltrasonicSensorMeasurement> listOfUSMeasurements;
                listOfUSMeasurements.push_back(u1);
                listOfUSMeasurements.push_back(u2);
                listOfUSMeasurements.push_back(u3);

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(listOfUSMeasurements);
                    }
                }
            }
            break;
            case YawData:
            {
                cout << "(STM32F4Protocol): YawData" << endl;

                RazorMeasurement measurement;

                // TODO: Discussion about out-of-range.
                measurement.yaw = getValueFromPacket(packet, 4, 8);
                int32_t sign_yaw = getValueFromPacket(packet, 12, 1) ? -1 : 1;
                measurement.pitch = 0;
                measurement.roll = 0;

                cout << "(STM32F4Protocol): yaw: " << measurement.yaw << " (sign: " << sign_yaw << ")" << endl;

                measurement.yaw *= sign_yaw;

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(measurement);
                    }
                }
            }
            break;
            case Magnetometer:
            {
                cout << "(STM32F4Protocol): Magnetometer" << endl;

                RazorMeasurement measurement;

                // TODO: Discussion about out-of-range.
                measurement.mag_X = getValueFromPacket(packet, 4, 8);
                int32_t sign_X = getValueFromPacket(packet, 12, 1) ? -1 : 1;
                measurement.mag_Y = getValueFromPacket(packet, 13, 8);
                int32_t sign_Y = getValueFromPacket(packet, 21, 1) ? -1 : 1;
                measurement.mag_Z = 0;

                cout << "(STM32F4Protocol): Magnetometer: MAG_X: " << measurement.mag_X << " (sign: " << sign_X << "), "
                                                              << " MAG_Y: " << measurement.mag_Y << " (sign: " << sign_Y << "), "
                                                              << " MAG_Z: " << measurement.mag_Z << endl;

                measurement.mag_X *= sign_X;
                measurement.mag_Y *= sign_Y;

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(measurement);
                    }
                }
            }
            break;
            case Gyroscope:
            {
                cout << "(STM32F4Protocol): Gyroscope" << endl;

                RazorMeasurement measurement;

                // TODO: Discussion about out-of-range.
                measurement.gyro_X = getValueFromPacket(packet, 4, 8);
                int32_t sign_X = getValueFromPacket(packet, 12, 1) ? -1 : 1;
                measurement.gyro_Y = getValueFromPacket(packet, 13, 8);
                int32_t sign_Y = getValueFromPacket(packet, 21, 1) ? -1 : 1;
                measurement.gyro_Z = 0;

                cout << "(STM32F4Protocol): Gyroscope: gyro_X: " << measurement.gyro_X << " (sign: " << sign_X << "), "
                                                              << " gyro_Y: " << measurement.gyro_Y << " (sign: " << sign_Y << "), "
                                                              << " gyro_Z: " << measurement.gyro_Z << endl;

                measurement.gyro_X *= sign_X;
                measurement.gyro_Y *= sign_Y;

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(measurement);
                    }
                }
            }
            break;
            case AccelerometerRazor:
            {
                cout << "(STM32F4Protocol): AccelerometerRazor" << endl;

                RazorMeasurement measurement;

                // TODO: Discussion about out-of-range.
                measurement.acc_X = getValueFromPacket(packet, 4, 8);
                int32_t sign_X = getValueFromPacket(packet, 12, 1) ? -1 : 1;
                measurement.acc_Y = getValueFromPacket(packet, 13, 8);
                int32_t sign_Y = getValueFromPacket(packet, 21, 1) ? -1 : 1;
                measurement.acc_Z = 0;

                cout << "(STM32F4Protocol): Accelerometer: ACC_X: " << measurement.acc_X << " (sign: " << sign_X << "), "
                                                              << " ACC_Y: " << measurement.acc_Y << " (sign: " << sign_Y << "), "
                                                              << " ACC_Z: " << measurement.acc_Z << endl;

                measurement.acc_X *= sign_X;
                measurement.acc_Y *= sign_Y;

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(measurement);
                    }
                }
            }
            break;
            case AccelerometerSTM32:
            {
                cout << "(STM32F4Protocol): AccelerometerSTM32" << endl;

                STM32F4AccelerometerMeasurement measurement;

                // TODO: Discussion about out-of-range.
                measurement.acc_X = getValueFromPacket(packet, 4, 8);
                int32_t sign_X = getValueFromPacket(packet, 12, 1) ? -1 : 1;
                measurement.acc_Y = getValueFromPacket(packet, 13, 8);
                int32_t sign_Y = getValueFromPacket(packet, 21, 1) ? -1 : 1;
                measurement.acc_Z = 0;

                cout << "(STM32F4Protocol): STM32F4-Accelerometer: ACC_X: " << measurement.acc_X << " (sign: " << sign_X << "), "
                                                              << " ACC_Y: " << measurement.acc_Y << " (sign: " << sign_Y << "), "
                                                              << " ACC_Z: " << measurement.acc_Z << endl;

                measurement.acc_X *= sign_X;
                measurement.acc_Y *= sign_Y;

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(measurement);
                    }
                }
            }
            break;
            case RazorAll:
                cout << "(STM32F4Protocol): RazorAll" << endl;
            break;
            case RazorAllSTM32:
                cout << "(STM32F4Protocol): RazorAllSTM32" << endl;
            break;
            case CurrentPosition:
            {
                cout << "(STM32F4Protocol): CurrentPosition" << endl;

                // TODO: Discussion about out-of-range.
                int32_t positionX = getValueFromPacket(packet, 4, 12);
                int32_t sign_X = getValueFromPacket(packet, 16, 1) ? -1 : 1;
                int32_t positionY = getValueFromPacket(packet, 17, 12);
                int32_t sign_Y = getValueFromPacket(packet, 29, 1) ? -1 : 1;

                Point3 pos(positionX * sign_X/100.0, positionY * sign_Y/100.0, 0);

                cout << "(STM32F4Protocol): STM32F4-CurrentPositon: positionX: " << positionX << " (sign: " << sign_X << "), "
                                                               << " positionY: " << positionY << " (sign: " << sign_Y << "): "
                                                               << pos.toString() << endl;

                m_vehicleData.setPosition(pos);

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(m_vehicleData);
                    }
                }
            }
            break;
            case TraveledPath:
            {
                cout << "(STM32F4Protocol): TraveledPath" << endl;

                uint32_t relTraveledPath = getValueFromPacket(packet, 4, 6);
                uint32_t absTraveledPath = getValueFromPacket(packet, 10, 16);

                cout << "(STM32F4Protocol): STM32F4-TraveledPath: relTraveledPath: " << relTraveledPath
                                                            << ", absTraveledPath: " << absTraveledPath << endl;

                m_vehicleData.setRelTraveledPath(relTraveledPath/2.0/100.0);
                m_vehicleData.setAbsTraveledPath(absTraveledPath/2.0/100.0);

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(m_vehicleData);
                    }
                }
            }
            break;
            case Velocity:
            {
                cout << "(STM32F4Protocol): Velocity" << endl;

                // TODO: Discussion about out-of-range.
                // TODO: Discussion about out-of-range.
                int32_t velocityX = getValueFromPacket(packet, 4, 9);
                int32_t sign_X = getValueFromPacket(packet, 13, 1) ? -1 : 1;
                int32_t velocityY = getValueFromPacket(packet, 14, 9);
                int32_t sign_Y = getValueFromPacket(packet, 23, 1) ? -1 : 1;

                Point3 vel(velocityX * sign_X/100.0, velocityY * sign_Y/100.0, 0);

                cout << "(STM32F4Protocol): STM32F4-Velocity: velocityX: " << velocityX << " (sign: " << sign_X << "), "
                                                               << " velocityY: " << velocityY << " (sign: " << sign_Y << "): "
                                                               << vel.toString() << endl;

                m_vehicleData.setVelocity(vel);

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(m_vehicleData);
                    }
                }
            }
            break;
            case Orientation:
            {
                cout << "(STM32F4Protocol): Orientation" << endl;

                // TODO: Discussion about out-of-range.
                int32_t orientation = getValueFromPacket(packet, 4, 11);

                double heading = fmod(orientation/1000.0 * Constants::PI, 2 * Constants::PI);

                m_vehicleData.setHeading(heading);

                cout << "(STM32F4Protocol): STM32F4-Orientation: " << orientation << ", heading: " << m_vehicleData.getHeading() << endl;

                {
                    Lock l(m_dataListenerMutex);
                    if (m_dataListener != NULL) {
                        m_dataListener->nextMeasurement(m_vehicleData);
                    }
                }
            }
            break;
            case Reserved14:
                cout << "(STM32F4Protocol): Reserved14" << endl;
            break;
            case Reserved15:
                cout << "(STM32F4Protocol): Reserved15" << endl;
            break;
        }
    }

    uint32_t STM32F4Protocol::getMultiplier(const uint32_t &m) {
        uint32_t retVal = 1;

        switch (m) {
            case 0:
                retVal = 1;
            break;
            case 1:
                retVal = 3;
            break;
            case 2:
                retVal = 5;
            break;
            case 3:
                retVal = 10;
            break;
            default:
                retVal = 1;
        }

        return retVal;
    }


} // msv

