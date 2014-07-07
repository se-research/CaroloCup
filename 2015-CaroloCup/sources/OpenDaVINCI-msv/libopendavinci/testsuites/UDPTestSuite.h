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
        static void test()
        {
            clog << endl << "UDPTestPOSIX" << endl;
            const string group = "225.0.0.13";
            const uint32_t port = 4567;

            core::SharedPointer<core::wrapper::UDPReceiver> receiver(
                    core::wrapper::UDPFactoryWorker<core::wrapper::NetworkLibraryPosix>::createUDPReceiver(group, port));

            core::SharedPointer<core::wrapper::UDPSender> sender(
                                core::wrapper::UDPFactoryWorker<core::wrapper::NetworkLibraryPosix>::createUDPSender(group, port));

            executeTest(receiver, sender);
        }
    };

#endif

#ifdef HAVE_BOOST_LIBRARIES
    #include "core/wrapper/Boost/BoostUDPFactoryWorker.h"
	#include "core/wrapper/Boost/BoostUDPReceiver.h"
	#include "core/wrapper/Boost/BoostUDPSender.h"

    struct UDPTestBoostAsio
    {
        static void test()
        {
            clog << endl << "UDPTestBoostAsio" << endl;
            const string group = "225.0.0.13";
            const uint32_t port = 4567;

            core::SharedPointer<core::wrapper::UDPReceiver> receiver(
                    core::wrapper::UDPFactoryWorker<core::wrapper::NetworkLibraryBoost>::createUDPReceiver(group, port));

            core::SharedPointer<core::wrapper::UDPSender> sender(
                                core::wrapper::UDPFactoryWorker<core::wrapper::NetworkLibraryBoost>::createUDPSender(group, port));

            executeTest(receiver, sender);
        }
    };
#endif


    class UDPTestsuite : public CxxTest::TestSuite
    {
        public:
            void test()
            {
                #ifndef WIN32
                UDPTestPOSIX::test();
                #endif

                #ifdef HAVE_BOOST_LIBRARIES
                UDPTestBoost::test();
                #endif
            }

            void executeTest(core::SharedPointer<core::wrapper::UDPReceiver> receiver,
                             core::SharedPointer<core::wrapper::UDPSender> sender)
            {
                receiver->start();

                mocks::StringListenerMock mock;
                mock.VALUES_nextString.addItem("Hello UDPMulticast");
                mock.VALUES_nextString.prepare();
                receiver->setStringListener(&mock);

                sender->send("Hello UDPMulticast");

                TS_ASSERT( mock.CALLWAITER_nextString.wait() );
                TS_ASSERT( mock.correctCalled() );

                receiver->setStringListener(NULL);
                receiver->stop();
            }
    };


#endif /*CORE_WRAPPER_UDPTESTSUITE_H_*/
