/*
 * Copyright (c) Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_WRAPPER_TCPTESTSUITE_H_
#define CORE_WRAPPER_TCPTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <memory>
#include <sstream>
#include <string>
#include <iostream>
#include <arpa/inet.h>

#include "core/native.h"
#include "core/SharedPointer.h"
#include "core/base/Condition.h"
#include "core/base/Lock.h"
#include "core/base/Mutex.h"
#include "core/base/Thread.h"

#ifndef WIN32

#include "core/mocks/ConnectionListenerMock.h"
#include "core/mocks/StringListenerMock.h"
#include "core/mocks/TCPAcceptorListenerMock.h"

#include "core/wrapper/StringListener.h"
#include "core/wrapper/Boost/BoostTCPFactory.h"
#include "core/wrapper/POSIX/POSIXTCPFactory.h"

#endif

#include "core/wrapper/TCPAcceptorListener.h"
#include "core/wrapper/TCPConnection.h"
#include "core/wrapper/TCPAcceptor.h"
#include "core/wrapper/TCPFactory.h"

using namespace std;
using namespace core;
using namespace core::base;

class TCPAcceptorTestsuite : public CxxTest::TestSuite,
            public wrapper::TCPFactory {
    public:
        void testAcceptBoost() {
#ifndef WIN32
            clog << endl << "TCPAcceptorTestsuite::testAcceptBoost" << endl;
            SharedPointer<TCPFactory> factoryBoost(createBoostFactory());
            acceptTest(factoryBoost);
#endif
        }

        void testAcceptPosix() {
#ifndef WIN32
            clog << endl << "TCPAcceptorTestsuite::testAcceptPosix" << endl;
            SharedPointer<TCPFactory> factoryPosix(createPOSIXFactory());
            acceptTest(factoryPosix);
#endif
        }

        void acceptTest(SharedPointer<TCPFactory> factory) {
#ifndef WIN32
            mocks::TCPAcceptorListenerMock ah;

            SharedPointer<wrapper::TCPAcceptor> acceptor(factory->createTCPAcceptor(20000));
            acceptor->setAcceptorListener(&ah);
            acceptor->start();

            SharedPointer<wrapper::TCPConnection> connection(factory->createTCPConnectionTo("127.0.0.1", 20000));
            connection->start();

            TS_ASSERT(ah.CALLWAITER_onNewConnection.wait());
#endif
        }

        void testMultipleAcceptBoost() {
#ifndef WIN32
            clog << endl << "TCPAcceptorTestsuite::testMultipleAcceptBoost" << endl;
            SharedPointer<TCPFactory> factoryBoost(createBoostFactory());
            multipleAcceptTest(factoryBoost);
#endif
        }

        void testMultipleAcceptPosix() {
#ifndef WIN32
            clog << endl << "TCPAcceptorTestsuite::testMultipleAcceptPosix" << endl;
            SharedPointer<TCPFactory> factoryPosix(createPOSIXFactory());
            multipleAcceptTest(factoryPosix);
#endif
        }

        void multipleAcceptTest(SharedPointer<TCPFactory> factory) {
#ifndef WIN32
            mocks::TCPAcceptorListenerMock ah1;
            mocks::TCPAcceptorListenerMock ah2;

            SharedPointer<wrapper::TCPAcceptor> acceptor(factory->createTCPAcceptor(20000));
            acceptor->setAcceptorListener(&ah1);
            acceptor->start();

            SharedPointer<wrapper::TCPConnection> connection(factory->createTCPConnectionTo("127.0.0.1", 20000));
            connection->start();

            TS_ASSERT(ah1.CALLWAITER_onNewConnection.wait());

            acceptor->setAcceptorListener(&ah2);

            SharedPointer<wrapper::TCPConnection> connection2(factory->createTCPConnectionTo("127.0.0.1", 20000));
            connection->start();

            TS_ASSERT(ah2.CALLWAITER_onNewConnection.wait());
#endif
        }

        void testNoAcceptBoost() {
#ifndef WIN32
            clog << endl << "TCPAcceptorTestsuite::testNoAcceptBoost" << endl;
            SharedPointer<TCPFactory> factoryBoost(createBoostFactory());
            noAcceptTest(factoryBoost);
#endif
        }

        void testNoAcceptPosix() {
#ifndef WIN32
            clog << endl << "TCPAcceptorTestsuite::testNoAcceptPosix" << endl;
            SharedPointer<TCPFactory> factoryPosix(createPOSIXFactory());
            noAcceptTest(factoryPosix);
#endif
        }

        void noAcceptTest(SharedPointer<TCPFactory> factory) {
#ifndef WIN32
            mocks::TCPAcceptorListenerMock am1;
            mocks::TCPAcceptorListenerMock am2;

            SharedPointer<wrapper::TCPAcceptor> acceptor(factory->createTCPAcceptor(20000));
            acceptor->setAcceptorListener(&am1);
            acceptor->start();

            SharedPointer<wrapper::TCPConnection> connection(factory->createTCPConnectionTo("127.0.0.1", 20000));
            connection->start();

            TS_ASSERT(am1.CALLWAITER_onNewConnection.wait());

            acceptor->setAcceptorListener(&am2);
            acceptor->stop();
            bool exceptionCaught = false;
            clog << "Test:" << endl;
            wrapper::TCPConnection* connection2 = NULL;
            try {
                connection2 = factory->createTCPConnectionTo("127.0.0.1", 20000);
                connection2->start();
            } catch (string &/*s*/) {
                exceptionCaught = true;
                delete connection2;
            }

            TS_ASSERT(exceptionCaught);
//            TS_ASSERT(am1.hasConnection());
            TS_ASSERT(!am2.CALLWAITER_onNewConnection.wasCalled());
#endif
        }

        virtual wrapper::TCPAcceptor* createTCPAcceptor(const uint32_t& /*port*/) {return NULL;};
        virtual wrapper::TCPConnection* createTCPConnectionTo(const std::string& /*ip*/, const uint32_t& /*port*/) {return NULL;};
};



class TCPConnectionTestSuite  : public CxxTest::TestSuite,
            public wrapper::TCPFactory {
    public:
        void testTransferBoost() {
#ifndef WIN32
            clog << endl << "TCPConnectionTestSuite::testTransferBoost" << endl;
            auto_ptr<TCPFactory> factoryBoost(createBoostFactory());
            transferTest(factoryBoost);
#endif
        }

        void testTransferPosix() {
#ifndef WIN32
            clog << endl << "TCPConnectionTestSuite::testTransferPosix" << endl;
            auto_ptr<TCPFactory> factoryPosix(createPOSIXFactory());
            transferTest(factoryPosix);
#endif
        }

        void transferTest(auto_ptr<TCPFactory>& factory) {
#ifndef WIN32
            mocks::TCPAcceptorListenerMock am;

            auto_ptr<wrapper::TCPAcceptor> acceptor(factory->createTCPAcceptor(20000));
            acceptor->setAcceptorListener(&am);
            acceptor->start();

            auto_ptr<wrapper::TCPConnection> connection(factory->createTCPConnectionTo("127.0.0.1", 20000));
            connection->start();

            TS_ASSERT(am.CALLWAITER_onNewConnection.wait());
            am.getConnection()->start();

            //Prepare StringListenerMock for connected TCPConnection
            mocks::StringListenerMock stmConnection;
            stmConnection.VALUES_nextString.addItem("TRANSFER ACCEPTED->CONNECTED");
            stmConnection.VALUES_nextString.prepare();
            connection->setStringListener(&stmConnection);

            //Prepare StringListenerMock for accepted TCPConnection
            mocks::StringListenerMock stmAcceptedConnection;
            stmAcceptedConnection.VALUES_nextString.addItem("TRANSFER CONNECTED->ACCEPTED");
            stmAcceptedConnection.VALUES_nextString.prepare();
            am.getConnection()->setStringListener(&stmAcceptedConnection);

            cerr << "Sending from connected to accepted TCPConnection..." << endl;
            connection->send("TRANSFER CONNECTED->ACCEPTED");

            TS_ASSERT( stmAcceptedConnection.CALLWAITER_nextString.wait() );
            TS_ASSERT( stmAcceptedConnection.correctCalled() );
            TS_ASSERT( !stmConnection.CALLWAITER_nextString.wasCalled() );
            stmAcceptedConnection.CALLWAITER_nextString.reset();

            cerr << "Sending from accepted to connected TCPConnection..." << endl;
            am.getConnection()->send("TRANSFER ACCEPTED->CONNECTED");

            TS_ASSERT( stmConnection.CALLWAITER_nextString.wait() );
            TS_ASSERT( stmConnection.correctCalled() );
            TS_ASSERT( !stmAcceptedConnection.CALLWAITER_nextString.wasCalled() );
            stmAcceptedConnection.CALLWAITER_nextString.reset();
#endif
        }

        void testErrorBoost() {
#ifndef WIN32
            clog << endl << "TCPConnectionTestSuite::testErrorBoost" << endl;
            auto_ptr<TCPFactory> factoryBoost(createBoostFactory());
            errorTest(factoryBoost);
#endif
        }

        void testErrorPosix() {
#ifndef WIN32
            clog << endl << "TCPConnectionTestSuite::testErrorPosix" << endl;
            auto_ptr<TCPFactory> factoryPosix(createPOSIXFactory());
            errorTest(factoryPosix);
#endif
        }

        void errorTest(auto_ptr<TCPFactory>& factory) {
#ifndef WIN32
            bool failed = true;
            try {
                mocks::TCPAcceptorListenerMock acceptorListenerMock1;

                auto_ptr<wrapper::TCPAcceptor> acceptor(factory->createTCPAcceptor(20000));
                acceptor->setAcceptorListener(&acceptorListenerMock1);
                acceptor->start();

                // 1. Fall: Akzeptierten Teil der Verbindung löschen
                auto_ptr<wrapper::TCPConnection> connection1(factory->createTCPConnectionTo("127.0.0.1", 20000));
                connection1->start();

                TS_ASSERT(acceptorListenerMock1.CALLWAITER_onNewConnection.wait());
//                TS_ASSERT(acceptorListenerMock1.hasConnection());
                acceptorListenerMock1.getConnection()->start();

                mocks::ConnectionListenerMock connectionListenerMock1;
                connection1->setConnectionListener(&connectionListenerMock1);
                delete acceptorListenerMock1.getConnection();
                acceptorListenerMock1.dontDeleteConnection();

                TS_ASSERT(connectionListenerMock1.CALLWAITER_handleConnectionError.wait());

                // 2. Fall: Verbindenden Teil der Verbindung löschen
                mocks::TCPAcceptorListenerMock acceptorListenerMock2;
                acceptor->setAcceptorListener(&acceptorListenerMock2);
                wrapper::TCPConnection* connection2(factory->createTCPConnectionTo("127.0.0.1", 20000));
                connection2->start();

                TS_ASSERT(acceptorListenerMock2.CALLWAITER_onNewConnection.wait());
                acceptorListenerMock2.getConnection()->start();

                mocks::ConnectionListenerMock connectionListenerMock2;
                acceptorListenerMock2.getConnection()->setConnectionListener(&connectionListenerMock2);
                delete connection2;

                TS_ASSERT(connectionListenerMock2.CALLWAITER_handleConnectionError.wait());
                failed = false;
            } catch (string &s) {
                TS_WARN(s);
                failed = true;
            } catch (...) {
                TS_WARN("Undefined error.");
                failed = true;
            }
            TS_ASSERT(!failed);
#endif
        }

        virtual wrapper::TCPAcceptor* createTCPAcceptor(const uint32_t& /*port*/) {return NULL;};
        virtual wrapper::TCPConnection* createTCPConnectionTo(const std::string& /*ip*/, const uint32_t& /*port*/) {return NULL;};
};

class TCPGathererTestSuite : public CxxTest::TestSuite, public wrapper::TCPConnection
{
    public:
        virtual void sendImplementation(const string& data)
        {
            m_sentData = data;
        }

        virtual void start()
        {}

        virtual void stop()
        {}

        /**
         * 1. Testfall: Die empfangenen Daten entsprechen einem
         * vollständigen Packet.
         */
        void testNoGathering()
        {
            clog << endl << "TCPConnectionTestSuite::testNoGathering" << endl;
            const string testData = createTestData("Die Testdaten");

            mocks::StringListenerMock mockListener;
            setStringListener(&mockListener);
            mockListener.VALUES_nextString.addItem("Die Testdaten");
            mockListener.VALUES_nextString.prepare();

            receivedString(testData);

            TS_ASSERT( mockListener.CALLWAITER_nextString.wait() );
            TS_ASSERT( mockListener.correctCalled() );
        }

        /**
         * 2. Testfall: Die Daten sind in zwei Teile geteilt.
         */
        void testGathering()
        {
            clog << endl << "TCPConnectionTestSuite::testGathering" << endl;
            const string testData = createTestData("Die Testdaten");

            // StringListenerMock zur Überprüfung der empfangenen Daten
            // vorbereiten...
            mocks::StringListenerMock mockListener;
            setStringListener(&mockListener);
            mockListener.VALUES_nextString.addItem("Die Testdaten");
            mockListener.VALUES_nextString.prepare();

            const string dataString1 = testData.substr(0, testData.length()/2);
            const string dataString2 = testData.substr(testData.length()/2);

            receivedString(dataString1);
            TS_ASSERT( !mockListener.CALLWAITER_nextString.wait() );

            receivedString(dataString2);
            TS_ASSERT( mockListener.CALLWAITER_nextString.wait() );
            TS_ASSERT( mockListener.correctCalled() );
        }

        /**
         * 3. Testfall: Zwei verschiedene gesendete Daten sind auf zwei Teile
         *              verteilt. Im zweiten Teil findet eine Vermischung der
         *              zwei gesendeten Daten statt.
         */
        void testMixedGathering()
        {
            clog << endl << "TCPConnectionTestSuite::testMixedGathering" << endl;
            stringstream dataStream;

            const string data1("Die ersten Testdaten");
            const string data2("Die zweiten Testdaten");

            const string testData1 = createTestData(data1);
            const string testData2 = createTestData(data2);

            // StringListenerMock zur Überprüfung der empfangenen Daten
            // vorbereiten...
            mocks::StringListenerMock mockListener;
            setStringListener(&mockListener);
            mockListener.VALUES_nextString.addItem(data1);
            mockListener.VALUES_nextString.addItem(data2);
            mockListener.VALUES_nextString.prepare();

            const string dataString1 = testData1 + testData2.substr(0, testData2.length()/2);
            const string dataString2 = testData2.substr(testData2.length()/2);

            receivedString(dataString1);
            TS_ASSERT( mockListener.CALLWAITER_nextString.wait() );
            TS_ASSERT( mockListener.correctCalled());
            mockListener.CALLWAITER_nextString.reset();

            receivedString(dataString2);
            TS_ASSERT( mockListener.CALLWAITER_nextString.wait() );
            TS_ASSERT( mockListener.correctCalled() );
        }

        string createTestData(const string& data)
        {
            const uint32_t dataSize = htons(data.length());
            stringstream dataStream;
            dataStream.write(reinterpret_cast<const char*>(&dataSize), sizeof(uint32_t));
            dataStream << data;

            return dataStream.str();
        }

    private:
        string m_sentData;
};

#endif /*CORE_WRAPPER_TCPTESTSUITE_H_*/
