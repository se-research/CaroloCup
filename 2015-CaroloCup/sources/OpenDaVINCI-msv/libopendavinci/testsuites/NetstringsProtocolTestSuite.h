/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_NETSTRINGSPROTOCOLTESTSUITE_H_
#define CORE_NETSTRINGSPROTOCOLTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <iostream>
#include <sstream>
#include <string>

#include "core/wrapper/NetstringsProtocol.h"
#include "core/wrapper/StringListener.h"

using namespace std;

class NetstringsProtocolTest : public CxxTest::TestSuite, public core::wrapper::StringListener, public core::wrapper::StringSender {
    private:
        string m_receivedData;
        string m_dataToBeSent;

    public:
        NetstringsProtocolTest() :
            m_receivedData(""),
            m_dataToBeSent("") {}

        void send(const string& data) {
            m_dataToBeSent = data;
        }

        void nextString(const string &s) {
            m_receivedData = s;
        }

        void testNetstringsProtocolSend() {
            core::wrapper::NetstringsProtocol nsp;
            nsp.setStringListener(this);
            nsp.setStringSender(this);

            m_dataToBeSent = "";
            TS_ASSERT(m_dataToBeSent.length() == 0); 

            string testDataToBeSent("HelloWorldSend!");
            nsp.send(testDataToBeSent);

            stringstream dataStream;
            dataStream << "15:" << testDataToBeSent << ",";

            TS_ASSERT(m_dataToBeSent.length() > 0); 
            TS_ASSERT(m_dataToBeSent.compare(dataStream.str()) == 0); 
        }

        void testNetstringsProtocolReceive() {
            core::wrapper::NetstringsProtocol nsp;
            nsp.setStringListener(this);
            nsp.setStringSender(this);

            m_receivedData = "";
            TS_ASSERT(m_receivedData.length() == 0); 

            string testDataToBeSent("HelloWorldReceive!");

            stringstream dataStream;
            dataStream << "18:" << testDataToBeSent << ",";

            nsp.receivedPartialString(dataStream.str());

            TS_ASSERT(m_receivedData.length() > 0); 
            TS_ASSERT(m_receivedData.compare(testDataToBeSent) == 0); 
        }

        void testNetstringsProtocolPartialReceive1() {
            core::wrapper::NetstringsProtocol nsp;
            nsp.setStringListener(this);
            nsp.setStringSender(this);

            m_receivedData = "";
            TS_ASSERT(m_receivedData.length() == 0); 

            string testDataToBeSent("Hello");

            stringstream dataStream;
            dataStream << "18:" << testDataToBeSent;

            nsp.receivedPartialString(dataStream.str());

            TS_ASSERT(m_receivedData.length() == 0);
        }

        void testNetstringsProtocolPartialReceive2() {
            core::wrapper::NetstringsProtocol nsp;
            nsp.setStringListener(this);
            nsp.setStringSender(this);

            m_receivedData = "";
            TS_ASSERT(m_receivedData.length() == 0); 

            string testDataToBeSent("Hello");

            stringstream dataStream;
            dataStream << "5:" << testDataToBeSent << "," << "18:" << "Adieu";

            nsp.receivedPartialString(dataStream.str());

            TS_ASSERT(m_receivedData.length() > 0); 
            TS_ASSERT(m_receivedData.compare(testDataToBeSent) == 0); 
        }

};

#endif /*CORE_NETSTRINGSPROTOCOLTESTSUITE_H_*/
