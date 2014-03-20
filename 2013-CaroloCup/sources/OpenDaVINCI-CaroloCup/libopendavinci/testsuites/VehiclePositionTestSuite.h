/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_VEHICLEPOSITIONTESTSUITE_H_
#define CORE_VEHICLEPOSITIONTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>

#include "core/data/Container.h"
#include "core/data/environment/VehiclePosition.h"

using namespace std;
using namespace core::data;
using namespace core::data::environment;

class VehiclePositionTest : public CxxTest::TestSuite {
    public:
        void testVPData() {
            VehiclePosition vp;
            Container c(Container::USER_DATA_3, vp);

            stringstream s;
            s << c;
            s.flush();

            Container c2;
            s >> c2;
            VehiclePosition vp2;
            vp2 = c2.getData<VehiclePosition>();

            TS_ASSERT(vp.toString() == vp2.toString());
        }

        void testVPData_1() {
            VehiclePosition vp(1, 2, 3);
            Container c(Container::USER_DATA_3, vp);

            stringstream s;
            s << c;
            s.flush();

            Container c2;
            s >> c2;
            VehiclePosition vp2;
            vp2 = c2.getData<VehiclePosition>();

            TS_ASSERT(vp.toString() == vp2.toString());
        }

        void testVPData_2() {
            VehiclePosition vp(1, 2, 3);
            vp.setTraveledPath(4);
            Container c(Container::USER_DATA_3, vp);

            stringstream s;
            s << c;
            s.flush();

            Container c2;
            s >> c2;
            VehiclePosition vp2;
            vp2 = c2.getData<VehiclePosition>();

            TS_ASSERT(vp.toString() == vp2.toString());
        }
};

#endif /*CORE_VEHICLEPOSITIONTESTSUITE_H_*/
