/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_STM32F4PROTOCOLTESTSUITE_H_
#define CORE_STM32F4PROTOCOLTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "core/data/Constants.h"

// Include local header files.
#include "../include/STM32F4Protocol.h"
#include "../include/STM32F4DataListener.h"

using namespace std;
using namespace msv;

class StringProtocolTest : public CxxTest::TestSuite, public core::wrapper::StringSender, public STM32F4DataListener {
    private:
        string m_dataToBeSent;
        vector<InfraredSensorMeasurement> m_IR;
        vector<UltrasonicSensorMeasurement> m_US;
        RazorMeasurement m_razor;
        STM32F4AccelerometerMeasurement m_acc;
        core::data::environment::VehicleData m_VD;

    public:
        StringProtocolTest() :
            m_dataToBeSent(""),
            m_IR(),
            m_US(),
            m_razor(),
            m_acc(),
            m_VD() {}

        void send(const string& data) {
            m_dataToBeSent = data;
        }

        std::string itos(uint32_t n)
	{
	   const int max_size = std::numeric_limits<int>::digits10 + 1 /*sign*/ + 1 /*0-terminator*/;
	   char buffer[max_size] = {0};
	   sprintf(buffer, "%d", n);
	   return std::string(buffer);
	}

	uint16_t stoi(std::string str)
	{
	   int i=0;
           uint16_t sum=0;
           while(str[i]!='\0'){
		if(str[i]<48 || str[i] > 57)
		   return 0;
		else{
		   sum = sum*10 + (str[i]-48);
		   i++;
		}
	   }
	   return sum;
	}

        virtual void nextMeasurement(const vector<InfraredSensorMeasurement> &measurement) {
            m_IR = measurement;
        }

        virtual void nextMeasurement(const vector<UltrasonicSensorMeasurement> &measurement) {
            m_US = measurement;
        }

        virtual void nextMeasurement(const RazorMeasurement &measurement) {
            m_razor = measurement;
        }

        virtual void nextMeasurement(const STM32F4AccelerometerMeasurement &measurement) {
            m_acc = measurement;
        }

        virtual void nextMeasurement(const core::data::environment::VehicleData &vehicleData) {
            m_VD = vehicleData;
        }

        uint32_t putValueAt(const uint32_t &value, const uint32_t &startBit) {
            return (value << startBit);
        }

        void testSTM32F4ProtocolRequest_1() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setStringSender(this);

            // In this string, the protocol will encode the data to be sent by a connection.
            m_dataToBeSent = "";
            TS_ASSERT(m_dataToBeSent.length() == 0); 

            // Actually send the data.
            protocol.request(STM32F4Protocol::NoData, 0, 0);

            // Check if the data was encoded correctly.
            uint16_t data = 0;
            data = stoi(m_dataToBeSent);

            // Unpack the data from the two bytes according to the protocol specification.
            uint8_t request = (data & 0xF);
            uint8_t speed = (data >> 4) & 0x3F;
            uint8_t steeringWheelAngle = (data >> 10) & 0x3F;

            // Assert the received data.
            TS_ASSERT(request == 0);
            TS_ASSERT(speed == 31);
            TS_ASSERT(steeringWheelAngle == 2);
        }

        void testSTM32F4ProtocolRequest_2() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setStringSender(this);

            // In this string, the protocol will encode the data to be sent by a connection.
            m_dataToBeSent = "";
            TS_ASSERT(m_dataToBeSent.length() == 0); 

            // Actually send the data.
            protocol.request(STM32F4Protocol::AllInfrareds, 2, 0);

            // Check if the data was encoded correctly.
            uint16_t data = 0;
            data = stoi(m_dataToBeSent);

            // Unpack the data from the two bytes according to the protocol specification.
            uint8_t request = (data & 0xF);
            uint8_t speed = (data >> 4) & 0x3F;
            uint8_t steeringWheelAngle = (data >> 10) & 0x3F;

            // Assert the received data.
            TS_ASSERT(request == 1);
            TS_ASSERT(speed == 63);
            TS_ASSERT(steeringWheelAngle == 2);
        }

        void testSTM32F4ProtocolRequest_3() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_razor = RazorMeasurement();

            stringstream dataStream;

            TS_ASSERT_DELTA(m_razor.yaw, 0, 1e-5);
            TS_ASSERT_DELTA(m_razor.roll, 0, 1e-5);
            TS_ASSERT_DELTA(m_razor.pitch, 0, 1e-5);

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 3;
            uint8_t YAW = 255;
            uint8_t SIGN_YAW = 1;
            data = putValueAt(request, 0) |
                   putValueAt(YAW, 4) |
                   putValueAt(SIGN_YAW, 12) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT_DELTA(m_razor.yaw, -1 * YAW, 1e-5);
            TS_ASSERT_DELTA(m_razor.roll, 0, 1e-5);
            TS_ASSERT_DELTA(m_razor.pitch, 0, 1e-5);
        }

        void testSTM32F4ProtocolRequest_4() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_razor = RazorMeasurement();

            stringstream dataStream;

            TS_ASSERT_DELTA(m_razor.mag_X, 0, 1e-5);
            TS_ASSERT_DELTA(m_razor.mag_Y, 0, 1e-5);
            TS_ASSERT_DELTA(m_razor.mag_Z, 0, 1e-5);

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 4;
            uint8_t MAG_X = 255;
            uint8_t SIGN_MAG_X = 0;
            uint8_t MAG_Y = 255;
            uint8_t SIGN_MAG_Y = 1;
            data = putValueAt(request, 0) |
                   putValueAt(MAG_X, 4) |
                   putValueAt(SIGN_MAG_X, 12) |
                   putValueAt(MAG_Y, 13) |
                   putValueAt(SIGN_MAG_Y, 21) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT_DELTA(m_razor.mag_X, MAG_X, 1e-5);
            TS_ASSERT_DELTA(m_razor.mag_Y, -1 * MAG_Y, 1e-5);
            TS_ASSERT_DELTA(m_razor.mag_Z, 0, 1e-5);
        }

        void testSTM32F4ProtocolRequest_5() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_razor = RazorMeasurement();

            stringstream dataStream;

            TS_ASSERT_DELTA(m_razor.gyro_X, 0, 1e-5);
            TS_ASSERT_DELTA(m_razor.gyro_Y, 0, 1e-5);
            TS_ASSERT_DELTA(m_razor.gyro_Z, 0, 1e-5);

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 5;
            uint8_t GYRO_X = 255;
            uint8_t SIGN_GYRO_X = 0;
            uint8_t GYRO_Y = 255;
            uint8_t SIGN_GYRO_Y = 1;
            data = putValueAt(request, 0) |
                   putValueAt(GYRO_X, 4) |
                   putValueAt(SIGN_GYRO_X, 12) |
                   putValueAt(GYRO_Y, 13) |
                   putValueAt(SIGN_GYRO_Y, 21) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT_DELTA(m_razor.gyro_X, GYRO_X, 1e-5);
            TS_ASSERT_DELTA(m_razor.gyro_Y, -1 * GYRO_Y, 1e-5);
            TS_ASSERT_DELTA(m_razor.gyro_Z, 0, 1e-5);
        }

        void testSTM32F4ProtocolRequest_6() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_razor = RazorMeasurement();

            stringstream dataStream;

            TS_ASSERT_DELTA(m_razor.acc_X, 0, 1e-5);
            TS_ASSERT_DELTA(m_razor.acc_Y, 0, 1e-5);
            TS_ASSERT_DELTA(m_razor.acc_Z, 0, 1e-5);

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 6;
            uint8_t ACC_X = 255;
            uint8_t SIGN_ACC_X = 0;
            uint8_t ACC_Y = 255;
            uint8_t SIGN_ACC_Y = 1;
            data = putValueAt(request, 0) |
                   putValueAt(ACC_X, 4) |
                   putValueAt(SIGN_ACC_X, 12) |
                   putValueAt(ACC_Y, 13) |
                   putValueAt(SIGN_ACC_Y, 21) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT_DELTA(m_razor.acc_X, ACC_X, 1e-5);
            TS_ASSERT_DELTA(m_razor.acc_Y, -1 * ACC_Y, 1e-5);
            TS_ASSERT_DELTA(m_razor.acc_Z, 0, 1e-5);
        }

        void testSTM32F4ProtocolRequest_7() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_acc = STM32F4AccelerometerMeasurement();

            stringstream dataStream;

            TS_ASSERT_DELTA(m_acc.acc_X, 0, 1e-5);
            TS_ASSERT_DELTA(m_acc.acc_Y, 0, 1e-5);
            TS_ASSERT_DELTA(m_acc.acc_Z, 0, 1e-5);

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 7;
            int16_t ACC_X = 255;
            uint8_t SIGN_ACC_X = 0;
            int16_t ACC_Y = 255;
            uint8_t SIGN_ACC_Y = 1;
            data = putValueAt(request, 0) |
                   putValueAt(ACC_X, 4) |
                   putValueAt(SIGN_ACC_X, 12) |
                   putValueAt(ACC_Y, 13) |
                   putValueAt(SIGN_ACC_Y, 21) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT_DELTA(m_acc.acc_X, ACC_X, 1e-5);
            TS_ASSERT_DELTA(m_acc.acc_Y, -1 * ACC_Y, 1e-5);
            TS_ASSERT_DELTA(m_acc.acc_Z, 0, 1e-5);
        }

        void testSTM32F4ProtocolRequest_10() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_VD = core::data::environment::VehicleData();

            stringstream dataStream;

            TS_ASSERT_DELTA(m_VD.getPosition().getX(), 0, 1e-5);
            TS_ASSERT_DELTA(m_VD.getPosition().getY(), 0, 1e-5);
            TS_ASSERT_DELTA(m_VD.getPosition().getZ(), 0, 1e-5);

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 10;
            int32_t POS_X = 4095;
            uint8_t SIGN_POS_X = 0;
            int32_t POS_Y = 4095;
            uint8_t SIGN_POS_Y = 1;
            data = putValueAt(request, 0) |
                   putValueAt(POS_X, 4) |
                   putValueAt(SIGN_POS_X, 16) |
                   putValueAt(POS_Y, 17) |
                   putValueAt(SIGN_POS_Y, 29) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT_DELTA(m_VD.getPosition().getX(), POS_X/100.0, 1e-5);
            TS_ASSERT_DELTA(m_VD.getPosition().getY(), POS_Y * (-1)/100.0, 1e-5);
            TS_ASSERT_DELTA(m_VD.getPosition().getZ(), 0, 1e-5);
        }

        void testSTM32F4ProtocolRequest_11() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_VD = core::data::environment::VehicleData();

            stringstream dataStream;

            TS_ASSERT_DELTA(m_VD.getRelTraveledPath(), 0, 1e-5);
            TS_ASSERT_DELTA(m_VD.getAbsTraveledPath(), 0, 1e-5);

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 11;
            uint32_t REL = 63;
            uint32_t ABS = 65535;
            data = putValueAt(request, 0) |
                   putValueAt(REL, 4) |
                   putValueAt(ABS, 10) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT_DELTA(m_VD.getRelTraveledPath(), 0.315, 1e-5);
            TS_ASSERT_DELTA(m_VD.getAbsTraveledPath(), 327.675, 1e-5);
        }

        void testSTM32F4ProtocolRequest_12() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_VD = core::data::environment::VehicleData();

            stringstream dataStream;

            TS_ASSERT_DELTA(m_VD.getVelocity().getX(), 0, 1e-5);
            TS_ASSERT_DELTA(m_VD.getVelocity().getY(), 0, 1e-5);
            TS_ASSERT_DELTA(m_VD.getVelocity().getZ(), 0, 1e-5);

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 12;
            int32_t VEL_X = 511;
            uint8_t SIGN_VEL_X = 0;
            int32_t VEL_Y = 511;
            uint8_t SIGN_VEL_Y = 1;
            data = putValueAt(request, 0) |
                   putValueAt(VEL_X, 4) |
                   putValueAt(SIGN_VEL_X, 13) |
                   putValueAt(VEL_Y, 14) |
                   putValueAt(SIGN_VEL_Y, 23) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT_DELTA(m_VD.getVelocity().getX(), VEL_X/100.0, 1e-5);
            TS_ASSERT_DELTA(m_VD.getVelocity().getY(), VEL_Y * (-1)/100.0, 1e-5);
            TS_ASSERT_DELTA(m_VD.getVelocity().getZ(), 0, 1e-5);
        }

        void testSTM32F4ProtocolRequest_13() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_VD = core::data::environment::VehicleData();

            stringstream dataStream;

            TS_ASSERT_DELTA(m_VD.getRelTraveledPath(), 0, 1e-5);
            TS_ASSERT_DELTA(m_VD.getAbsTraveledPath(), 0, 1e-5);

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 13;
            int32_t HEADING = 2047;
            data = putValueAt(request, 0) |
                   putValueAt(HEADING, 4) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT_DELTA(m_VD.getHeading(), fmod(2047/1000.0*core::data::Constants::PI, 2*core::data::Constants::PI), 1e-5);
        }

        void testSTM32F4Protocol_HandleRequest_Distances() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_IR.clear();
            m_US.clear();

            TS_ASSERT(m_IR.size() == 0);
            TS_ASSERT(m_US.size() == 0);

            stringstream dataStream;

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 1;
            uint8_t IR1 = 30;
            uint8_t IR2 = 30;
            uint8_t IR3 = 30;

            data = putValueAt(request, 0) |
                   putValueAt(IR1, 4) |
                   putValueAt(IR2, 9) |
                   putValueAt(IR3, 14) |
                   putValueAt(1, 30);

            data = request | (IR1 << 4) | (IR2 << 9) | (IR3 << 14) | (1<<30);
            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));
            // Check if the data was encoded correctly.
            data = 0;
            request = 2;
            uint32_t US1 = 63; uint32_t US1_multiplier = 0;
            uint32_t US2 = 63; uint32_t US2_multiplier = 1;
            uint32_t US3 = 63; uint32_t US3_multiplier = 2;

            data = putValueAt(request, 0) |
                   putValueAt(US1, 4) |
                   putValueAt(US1_multiplier, 10) |
                   putValueAt(US2, 12) |
                   putValueAt(US2_multiplier, 18) |
                   putValueAt(US3, 20) |
                   putValueAt(US3_multiplier, 26) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT(m_IR.size() == 3);
            TS_ASSERT(m_IR.at(0).address == 200);
            TS_ASSERT(m_IR.at(0).value == 30);
            TS_ASSERT(m_IR.at(1).address == 201);
            TS_ASSERT(m_IR.at(1).value == 30);
            TS_ASSERT(m_IR.at(2).address == 202);
            TS_ASSERT(m_IR.at(2).value == 30);

            TS_ASSERT(m_US.size() == 3);
            TS_ASSERT(m_US.at(0).address == 100);
            TS_ASSERT(m_US.at(0).value == 63*1);
            TS_ASSERT(m_US.at(1).address == 101);
            TS_ASSERT(m_US.at(1).value == 63*3);
            TS_ASSERT(m_US.at(2).address == 102);
            TS_ASSERT(m_US.at(2).value == 63*5);
        }

        void testSTM32F4Protocol_HandleRequest_Distances_Infinity() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            m_IR.clear();
            m_US.clear();

            TS_ASSERT(m_IR.size() == 0);
            TS_ASSERT(m_US.size() == 0);

            stringstream dataStream;

            // Check if the data was encoded correctly.
            uint32_t data = 0;
            uint8_t request = 1;
            uint8_t IR1 = 31;
            uint8_t IR2 = 31;
            uint8_t IR3 = 31;

            data = putValueAt(request, 0) |
                   putValueAt(IR1, 4) |
                   putValueAt(IR2, 9) |
                   putValueAt(IR3, 14) |
                   putValueAt(1, 30);

            data = request | (IR1 << 4) | (IR2 << 9) | (IR3 << 14) | (1<<30);
            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));
            // Check if the data was encoded correctly.
            data = 0;
            request = 2;
            uint32_t US1 = 63; uint32_t US1_multiplier = 3;
            uint32_t US2 = 63; uint32_t US2_multiplier = 3;
            uint32_t US3 = 63; uint32_t US3_multiplier = 3;

            data = putValueAt(request, 0) |
                   putValueAt(US1, 4) |
                   putValueAt(US1_multiplier, 10) |
                   putValueAt(US2, 12) |
                   putValueAt(US2_multiplier, 18) |
                   putValueAt(US3, 20) |
                   putValueAt(US3_multiplier, 26) |
                   putValueAt(1, 30);

            data = ntohl(data);
            dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

            protocol.receivedPartialString(dataStream.str());

            TS_ASSERT(m_IR.size() == 3);
            TS_ASSERT(m_IR.at(0).address == 200);
            TS_ASSERT(m_IR.at(0).value == -1);
            TS_ASSERT(m_IR.at(1).address == 201);
            TS_ASSERT(m_IR.at(1).value == -1);
            TS_ASSERT(m_IR.at(2).address == 202);
            TS_ASSERT(m_IR.at(2).value == -1);

            TS_ASSERT(m_US.size() == 3);
            TS_ASSERT(m_US.at(0).address == 100);
            TS_ASSERT(m_US.at(0).value == -1);
            TS_ASSERT(m_US.at(1).address == 101);
            TS_ASSERT(m_US.at(1).value == -1);
            TS_ASSERT(m_US.at(2).address == 102);
            TS_ASSERT(m_US.at(2).value == -1);
        }

        void testSTM32F4Protocol_IterateAllIR() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            // Check if the data was encoded correctly.
            for(uint8_t IR1 = 0; IR1 < 29; IR1++) {
                uint8_t IR2 = IR1+1;
                uint8_t IR3 = IR1+2;
                m_IR.clear();
                m_US.clear();

                TS_ASSERT(m_IR.size() == 0);
                TS_ASSERT(m_US.size() == 0);

                stringstream dataStream;

                uint32_t data = 0;
                uint8_t request = 1;

                data = putValueAt(request, 0) |
                       putValueAt(IR1, 4) |
                       putValueAt(IR2, 9) |
                       putValueAt(IR3, 14) |
                       putValueAt(1, 30);

                data = ntohl(data);
                dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));

                protocol.receivedPartialString(dataStream.str());

                TS_ASSERT(m_IR.size() == 3);
                TS_ASSERT(m_IR.at(0).address == 200);
                TS_ASSERT(m_IR.at(0).value == IR1);
                TS_ASSERT(m_IR.at(1).address == 201);
                TS_ASSERT(m_IR.at(1).value == IR2);
                TS_ASSERT(m_IR.at(2).address == 202);
                TS_ASSERT(m_IR.at(2).value == IR3);
            }
        }

        void testSTM32F4Protocol_IterateAllUS() {
            // Instantiate the actual protocol.
            STM32F4Protocol protocol;
            protocol.setSTM32F4DataListener(this);

            // Check if the data was encoded correctly.
            for(uint8_t US1 = 0; US1 < 61; US1++) {
                uint8_t US2 = US1+1;
                uint8_t US3 = US1+2;
                m_IR.clear();
                m_US.clear();

                TS_ASSERT(m_IR.size() == 0);
                TS_ASSERT(m_US.size() == 0);

                stringstream dataStream;

                uint8_t request = 2;
                uint32_t US1_multiplier = 0;
                uint32_t US2_multiplier = 1;
                uint32_t US3_multiplier = 2;
                uint32_t data = putValueAt(request, 0) |
                       putValueAt(US1, 4) |
                       putValueAt(US1_multiplier, 10) |
                       putValueAt(US2, 12) |
                       putValueAt(US2_multiplier, 18) |
                       putValueAt(US3, 20) |
                       putValueAt(US3_multiplier, 26) |
                       putValueAt(2, 30);

                data = ntohl(data);
                dataStream.write(reinterpret_cast<char*>(&data), sizeof(uint32_t));
		
                protocol.receivedPartialString(dataStream.str());

                TS_ASSERT(m_US.size() == 3);
                TS_ASSERT(m_US.at(0).address == 100);
                TS_ASSERT(m_US.at(0).value == US1*1);
                TS_ASSERT(m_US.at(1).address == 101);
                TS_ASSERT(m_US.at(1).value == US2*3);
                TS_ASSERT(m_US.at(2).address == 102);
                TS_ASSERT(m_US.at(2).value == US3*5);
            }
        }

};

#endif /*CORE_STM32F4PROTOCOLTESTSUITE_H_*/
