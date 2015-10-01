/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef ProxyTESTSUITE_H_
#define ProxyTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include "core/base/ProtoSerializerVisitor.h"
#include "core/base/ProtoDeserializerVisitor.h"

#include "core/reflection/Message.h"
#include "core/reflection/MessageFromVisitableVisitor.h"
#include "core/reflection/MessagePrettyPrinterVisitor.h"

#include "GeneratedHeaders_AutomotiveData.h"

// Include local header files.
#include "../include/Proxy.h"

using namespace std;
using namespace core::base;
using namespace core::reflection;
using namespace core::data;
using namespace msv;

/**
 * This class derives from SensorBoard to allow access to protected methods.
 */
class ProxyTestling : public Proxy {
    private:
        ProxyTestling();
    
    public:
        ProxyTestling(const int32_t &argc, char **argv) :
            Proxy(argc, argv) {}

        // Here, you need to add all methods which are protected in Proxy and which are needed for the test cases.
};

/**
 * The actual testsuite starts here.
 */
class ProxyTest : public CxxTest::TestSuite {
    private:
        ProxyTestling *dt;

    public:
        /**
         * This method will be called before each testXYZ-method.
         */
        void setUp() {
            // Prepare the data that would be available from commandline.
            string argv0("proxy-STM32F4");
            string argv1("--cid=100");
            int32_t argc = 2;
            char **argv;
            argv = new char*[2];
            argv[0] = const_cast<char*>(argv0.c_str());
            argv[1] = const_cast<char*>(argv1.c_str());

            // Create an instance of sensorboard through SensorBoardTestling which will be deleted in tearDown().
            dt = new ProxyTestling(argc, argv);
        }

        /**
         * This method will be called after each testXYZ-method.
         */
        void tearDown() {
            delete dt;
            dt = NULL;
        }

        ////////////////////////////////////////////////////////////////////////////////////
        // Below this line the actual testcases are defined.
        ////////////////////////////////////////////////////////////////////////////////////

        void testProxySuccessfullyCreated() {
            TS_ASSERT(dt != NULL);
        }

        void testProto() {
            automotive::carolocup::Sensors sensorData;
            sensorData.setUsFront(10);
            sensorData.setUsRear(12);
            sensorData.setIrFrontRight(14);
            sensorData.setIrRearRight(16);
            sensorData.setIrBackLeft(18);
            sensorData.setIrBackRight(20);
            sensorData.setWheelFrontLeft(22);
            sensorData.setWheelRearRight(24);

            // Create a Proto serialization visitor.
            ProtoSerializerVisitor protoSerializerVisitor;
            sensorData.accept(protoSerializerVisitor);

            // Write the data to a stringstream.
            stringstream out;
            protoSerializerVisitor.getSerializedData(out);

            // Now, the data is in out.

            {
                // Some illustrative statements for demo purposes.
                cout << "Proto data: '" << out.str() << "'" << endl;

                // Create generic representation from data structure.
                MessageFromVisitableVisitor mfvv;
                sensorData.accept(mfvv);
                Message msg = mfvv.getMessage();

                // Pretty print generic representation.
                MessagePrettyPrinterVisitor mpp2;
                msg.accept(mpp2);
                mpp2.getOutput(cout);
            }

            // Create a Proto deserialization visitor.
            ProtoDeserializerVisitor protoDeserializerVisitor;
            protoDeserializerVisitor.deserializeDataFrom(out);

            // Read back the data by using the visitor.
            automotive::carolocup::Sensors sensorData2;
            sensorData2.accept(protoDeserializerVisitor);

            TS_ASSERT(sensorData.getUsFront() == sensorData2.getUsFront());
            TS_ASSERT(sensorData.getUsRear() == sensorData2.getUsRear());
            TS_ASSERT(sensorData.getIrFrontRight() == sensorData2.getIrFrontRight());
            TS_ASSERT(sensorData.getIrRearRight() == sensorData2.getIrRearRight());
            TS_ASSERT(sensorData.getIrBackLeft() == sensorData2.getIrBackLeft());
            TS_ASSERT(sensorData.getIrBackRight() == sensorData2.getIrBackRight());
            TS_ASSERT(sensorData.getWheelFrontLeft() == sensorData2.getWheelFrontLeft());
            TS_ASSERT(sensorData.getWheelRearRight() == sensorData2.getWheelRearRight());
        }

        void testProtoNoHeader() {
            automotive::carolocup::Sensors sensorData;
            sensorData.setUsFront(10);
            sensorData.setUsRear(12);
            sensorData.setIrFrontRight(14);
            sensorData.setIrRearRight(16);
            sensorData.setIrBackLeft(18);
            sensorData.setIrBackRight(20);
            sensorData.setWheelFrontLeft(22);
            sensorData.setWheelRearRight(24);

            // Create a Proto serialization visitor.
            ProtoSerializerVisitor protoSerializerVisitor;
            sensorData.accept(protoSerializerVisitor);

            // Write the data to a stringstream.
            stringstream out;
            protoSerializerVisitor.getSerializedDataNoHeader(out);

            // Now, the data is in out.

            {
                // Some illustrative statements for demo purposes.
                cout << "Proto data: '" << out.str() << "'" << endl;

                // Create generic representation from data structure.
                MessageFromVisitableVisitor mfvv;
                sensorData.accept(mfvv);
                Message msg = mfvv.getMessage();

                // Pretty print generic representation.
                MessagePrettyPrinterVisitor mpp2;
                msg.accept(mpp2);
                mpp2.getOutput(cout);
            }

            // Create a Proto deserialization visitor.
            ProtoDeserializerVisitor protoDeserializerVisitor;
            protoDeserializerVisitor.deserializeDataFromNoHeader(out);

            // Read back the data by using the visitor.
            automotive::carolocup::Sensors sensorData2;
            sensorData2.accept(protoDeserializerVisitor);

            TS_ASSERT(sensorData.getUsFront() == sensorData2.getUsFront());
            TS_ASSERT(sensorData.getUsRear() == sensorData2.getUsRear());
            TS_ASSERT(sensorData.getIrFrontRight() == sensorData2.getIrFrontRight());
            TS_ASSERT(sensorData.getIrRearRight() == sensorData2.getIrRearRight());
            TS_ASSERT(sensorData.getIrBackLeft() == sensorData2.getIrBackLeft());
            TS_ASSERT(sensorData.getIrBackRight() == sensorData2.getIrBackRight());
            TS_ASSERT(sensorData.getWheelFrontLeft() == sensorData2.getWheelFrontLeft());
            TS_ASSERT(sensorData.getWheelRearRight() == sensorData2.getWheelRearRight());
        }

        ////////////////////////////////////////////////////////////////////////////////////
        // Below this line the necessary constructor for initializing the pointer variables,
        // and the forbidden copy constructor and assignment operator are declared.
        //
        // These functions are normally not changed.
        ////////////////////////////////////////////////////////////////////////////////////

    public:
        /**
         * This constructor is only necessary to initialize the pointer variable.
         */
        ProxyTest() : dt(NULL) {}

    private:
        /**
         * "Forbidden" copy constructor. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the copy constructor.
         *
         * @param obj Reference to an object of this class.
         */
        ProxyTest(const ProxyTest &/*obj*/);

        /**
         * "Forbidden" assignment operator. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the assignment operator.
         *
         * @param obj Reference to an object of this class.
         * @return Reference to this instance.
         */
        ProxyTest& operator=(const ProxyTest &/*obj*/);

};

#endif /*ProxyTESTSUITE_H_*/

