/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_WRAPPER_UDPTESTSUITE_H_
#define CORE_WRAPPER_UDPTESTSUITE_H_

#include "cxxtest/TestSuite.h"
#include "core/platform.h"
#include "core/SharedPointer.h"

#include "core/wrapper/UDPSender.h"
#include "core/wrapper/UDPReceiver.h"
#include "core/wrapper/NetworkLibraryProducts.h"
#include "core/wrapper/UDPFactoryWorker.h"

#include "core/wrapper/StringListener.h"
#include "mocks/StringListenerMock.h"

using namespace std;

#ifndef WIN32
    #include "core/wrapper/POSIX/POSIXUDPFactoryWorker.h"
	#include "core/wrapper/POSIX/POSIXUDPReceiver.h"
	#include "core/wrapper/POSIX/POSIXUDPSender.h"

    struct UDPTestPOSIX
    {
        static void test(mocks::StringListenerMock &mock)
        {
            clog << endl << "UDPTestPOSIX" << endl;
            const string group = "225.0.0.13";
            const uint32_t port = 4567;

            core::SharedPointer<core::wrapper::UDPReceiver> receiver(
                    core::wrapper::UDPFactoryWorker<core::wrapper::NetworkLibraryPosix>::createUDPReceiver(group, port));

            core::SharedPointer<core::wrapper::UDPSender> sender(
                                core::wrapper::UDPFactoryWorker<core::wrapper::NetworkLibraryPosix>::createUDPSender(group, port));

            receiver->start();

            mock.VALUES_nextString.addItem("Hello UDPMulticast");
            mock.VALUES_nextString.prepare();
            receiver->setStringListener(&mock);

            mock.CALLWAITER_nextString.wait();

            sender->send("Hello UDPMulticast");

            mock.CALLWAITER_nextString.wait();

            receiver->setStringListener(NULL);
            receiver->stop();
        }
    };

#endif

#ifdef WIN32
    #include "core/wrapper/WIN32/WIN32UDPFactoryWorker.h"
	#include "core/wrapper/WIN32/WIN32UDPReceiver.h"
	#include "core/wrapper/WIN32/WIN32UDPSender.h"

    struct UDPTestWin32
    {
        static void test(mocks::StringListenerMock &mock)
        {
            clog << endl << "UDPTestWin32" << endl;
            const string group = "225.0.0.13";
            const uint32_t port = 4567;

            core::SharedPointer<core::wrapper::UDPReceiver> receiver(
                    core::wrapper::UDPFactoryWorker<core::wrapper::NetworkLibraryWin32>::createUDPReceiver(group, port));

            core::SharedPointer<core::wrapper::UDPSender> sender(
                                core::wrapper::UDPFactoryWorker<core::wrapper::NetworkLibraryWin32>::createUDPSender(group, port));

            receiver->start();

            mock.VALUES_nextString.addItem("Hello UDPMulticast");
            mock.VALUES_nextString.prepare();
            receiver->setStringListener(&mock);

            mock.CALLWAITER_nextString.wait();

            sender->send("Hello UDPMulticast");

            mock.CALLWAITER_nextString.wait();

            receiver->setStringListener(NULL);
            receiver->stop();
        }
    };
#endif


    class UDPTestsuite : public CxxTest::TestSuite
    {
        public:
            void testDataExchange()
            {
                mocks::StringListenerMock mock;

                #ifndef WIN32
                UDPTestPOSIX::test(mock);
                #endif

                #ifdef WIN32
                UDPTestWin32::test(mock);
                #endif

                TS_ASSERT( mock.CALLWAITER_nextString.wasCalled() );
                TS_ASSERT( mock.correctCalled() );
            }
    };


#endif /*CORE_WRAPPER_UDPTESTSUITE_H_*/
