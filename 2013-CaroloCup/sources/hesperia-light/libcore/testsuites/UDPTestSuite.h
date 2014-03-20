/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_WRAPPER_UDPTESTSUITE_H_
#define CORE_WRAPPER_UDPTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>
#include <string>
#include <iostream>

#include "core/wrapper/StringListener.h"

#include "core/wrapper/UDPFactory.h"
#include "core/wrapper/UDPSender.h"
#include "core/wrapper/UDPSender.h"

#ifndef WIN32

#include "core/wrapper/Boost/BoostUDPFactory.h"
#include "core/wrapper/Boost/BoostUDPReceiver.h"
#include "core/wrapper/Boost/BoostUDPSender.h"
#include "core/wrapper/POSIX/POSIXUDPFactory.h"
#include "core/wrapper/POSIX/POSIXUDPReceiver.h"
#include "core/wrapper/POSIX/POSIXUDPSender.h"

#include "core/mocks/StringListenerMock.h"

#endif

using namespace std;

#ifndef WIN32
class UDPFactory : public core::wrapper::POSIX::POSIXUDPFactory,
            public core::wrapper::Boost::BoostUDPFactory {

    public:
        core::wrapper::UDPSender* createPOSIXUDPSender(const string &group, const uint32_t &port) {
            return core::wrapper::POSIX::POSIXUDPFactory::createUDPSender(group, port);
        }

        core::wrapper::UDPReceiver* createPOSIXUDPReceiver(const string &group, const uint32_t &port, const bool &isMulticast) {
            return core::wrapper::POSIX::POSIXUDPFactory::createUDPReceiver(group, port, isMulticast);
        }

        core::wrapper::UDPSender* createBoostUDPSender(const string &group, const uint32_t &port) {
            return core::wrapper::Boost::BoostUDPFactory::createUDPSender(group, port);
        }

        core::wrapper::UDPReceiver* createBoostUDPReceiver(const string &group, const uint32_t &port, const bool &isMulticast) {
            return core::wrapper::Boost::BoostUDPFactory::createUDPReceiver(group, port, isMulticast);
        }
};
#endif

class UDPTest : public CxxTest::TestSuite {
    public:

        void testPOSIX() {
#ifndef WIN32
            clog << endl << "UDPTest::testPOSIX" << endl;
            const string group = "225.0.0.13";
            const uint32_t port = 4567;

            UDPFactory uf;

            core::wrapper::UDPReceiver* receiver = uf.createPOSIXUDPReceiver(group, port, true);
            core::wrapper::UDPSender* sender = uf.createPOSIXUDPSender(group, port);

            executeTest(receiver, sender);

            delete receiver;
            delete sender;
#endif
        }

        void testBOOST() {
#ifndef WIN32
            clog << endl << "UDPTest::testBOOST" << endl;
            const string group = "225.0.0.13";
            const uint32_t port = 4567;

            UDPFactory uf;

            core::wrapper::UDPReceiver* receiver = uf.createBoostUDPReceiver(group, port, true);
            core::wrapper::UDPSender* sender = uf.createBoostUDPSender(group, port);

            executeTest(receiver, sender);

            delete receiver;
            delete sender;
#endif
        }

#ifndef WIN32
        void executeTest(core::wrapper::UDPReceiver* receiver, core::wrapper::UDPSender* sender) {
            receiver->start();

            mocks::StringListenerMock mock;
            mock.VALUES_nextString.addItem("Hallo UDPMulticast");
            mock.VALUES_nextString.prepare();
            receiver->setStringListener(&mock);

            sender->send("Hallo UDPMulticast");

            TS_ASSERT( mock.CALLWAITER_nextString.wait() );
            TS_ASSERT( mock.correctCalled() );

            receiver->setStringListener(NULL);
            receiver->stop();
        }
#endif

};

#endif /*CORE_WRAPPER_UDPTESTSUITE_H_*/
