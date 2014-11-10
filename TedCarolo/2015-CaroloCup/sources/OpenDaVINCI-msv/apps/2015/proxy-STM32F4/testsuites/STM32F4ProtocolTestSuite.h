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
            TS_ASSERT(m_dataToBeSent == "27:This is my data to be sent.,");
        }

};

#endif /*CORE_STM32F4PROTOCOLTESTSUITE_H_*/
