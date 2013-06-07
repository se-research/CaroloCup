/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_CANMESSAGETESTSUITE_H_
#define CORE_CANMESSAGETESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>

#include "core/data/Container.h"
#include "hesperia/data/can/CANMessage.h"

using namespace std;
using namespace core::data;
using namespace hesperia::data::can;

class CANMessageTest : public CxxTest::TestSuite {
    public:
        void testEmptyCANMessageData() {
            CANMessage cm;
            Container c(Container::CANMESSAGE, cm);

            stringstream s;
            s << c;
            s.flush();

            Container c2;
            s >> c2;
            CANMessage cm2;
            cm2 = c2.getData<CANMessage>();

            TS_ASSERT(cm.toString() == cm2.toString());
        }

        void testCANMessageData() {
            unsigned char data[8];
            data[0] = 'A';
            data[1] = 'B';
            data[2] = 'C';
            data[3] = 'D';
            data[4] = 'E';
            data[5] = 'F';
            data[6] = 'G';
            data[7] = 'H';
            CANMessage cm(42, 8, data, 43);
            Container c(Container::CANMESSAGE, cm);

            stringstream s;
            s << c;
            s.flush();

            Container c2;
            s >> c2;
            CANMessage cm2;
            cm2 = c2.getData<CANMessage>();

            TS_ASSERT(cm.toString() == cm2.toString());
        }
};

#endif /*CORE_CANMESSAGETESTSUITE_H_*/
