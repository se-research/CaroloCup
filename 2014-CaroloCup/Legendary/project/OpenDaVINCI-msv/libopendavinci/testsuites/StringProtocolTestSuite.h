/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_STRINGPROTOCOLTESTSUITE_H_
#define CORE_STRINGPROTOCOLTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <iostream>
#include <sstream>
#include <string>

#include "core/wrapper/StringProtocol.h"
#include "core/wrapper/StringListener.h"

using namespace std;

class StringProtocolTest : public CxxTest::TestSuite, public core::wrapper::StringListener, public core::wrapper::StringSender {
    private:
        string m_receivedData;
        string m_dataToBeSent;

    public:
        StringProtocolTest() :
            m_receivedData(""),
            m_dataToBeSent("") {}

        void send(const string& data) {
            m_dataToBeSent = data;
        }

        void nextString(const string &s) {
            m_receivedData = s;
        }

        void testStringProtocolSend() {
            core::wrapper::StringProtocol sp;
            sp.setStringListener(this);
            sp.setStringSender(this);

            m_dataToBeSent = "";
            TS_ASSERT(m_dataToBeSent.length() == 0); 

            string testDataToBeSent("HelloWorldSend!");
            sp.send(testDataToBeSent);

            stringstream sstr;
            const uint32_t dataSize = htonl(testDataToBeSent.length());
            stringstream dataStream;
            dataStream.write(reinterpret_cast<const char*>(&dataSize), sizeof(uint32_t));
            dataStream << testDataToBeSent;

            TS_ASSERT(m_dataToBeSent.length() > 0); 
            TS_ASSERT(m_dataToBeSent.compare(dataStream.str()) == 0); 
        }

        void testStringProtocolReceive() {
            core::wrapper::StringProtocol sp;
            sp.setStringListener(this);
            sp.setStringSender(this);

            m_receivedData = "";
            TS_ASSERT(m_receivedData.length() == 0); 

            string testDataToBeSent("HelloWorldReceive!");

            stringstream sstr;
            const uint32_t dataSize = htonl(testDataToBeSent.length());
            stringstream dataStream;
            dataStream.write(reinterpret_cast<const char*>(&dataSize), sizeof(uint32_t));
            dataStream << testDataToBeSent;

            sp.receivedPartialString(dataStream.str());

            TS_ASSERT(m_receivedData.length() > 0); 
            TS_ASSERT(m_receivedData.compare(testDataToBeSent) == 0); 
        }
};

#endif /*CORE_STRINGPROTOCOLTESTSUITE_H_*/
