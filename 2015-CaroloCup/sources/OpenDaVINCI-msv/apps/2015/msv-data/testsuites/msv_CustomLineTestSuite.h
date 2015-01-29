/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_CUSTOMLINE_TESTSUITE_H
#define MSV_CUSTOMLINE_TESTSUITE_H

#include "cxxtest/TestSuite.h"

#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"
#include "core/StringToolbox.h"

#include "GeneratedHeaders_msv.h"


class msv_CustomLine_TestSuite : public CxxTest::TestSuite {

	public:
		void testCreateObject() {
			using namespace msv;
	
			CustomLine obj1;
			TS_ASSERT_DELTA(obj1.getSlope(), 0, 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == -1);
	
			obj1.setSlope(2.5);
			obj1.setPolygonIndex(-3);
	
			TS_ASSERT_DELTA(obj1.getSlope(), 2.5, 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == -3);
		}
	
		void testCreateAndCopyObject() {
			using namespace msv;
	
			CustomLine obj1;
			TS_ASSERT_DELTA(obj1.getSlope(), 0, 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == -1);
	
			CustomLine obj2(obj1);
			TS_ASSERT_DELTA(obj1.getSlope(), obj2.getSlope(), 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == obj2.getPolygonIndex());
	
			obj1.setSlope(2.5);
			obj1.setPolygonIndex(-3);
	
			TS_ASSERT_DELTA(obj1.getSlope(), 2.5, 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == -3);
	
			CustomLine obj3(obj1);
			TS_ASSERT_DELTA(obj1.getSlope(), obj3.getSlope(), 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == obj3.getPolygonIndex());
	
			TS_ASSERT_DELTA(obj3.getSlope(), 2.5, 1e-5);
			TS_ASSERT(obj3.getPolygonIndex() == -3);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj3.toString()));
		}
	
		void testCreateAndAssignObject() {
			using namespace msv;
	
			CustomLine obj1;
			TS_ASSERT_DELTA(obj1.getSlope(), 0, 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == -1);
	
			CustomLine obj2;
			TS_ASSERT_DELTA(obj2.getSlope(), 0, 1e-5);
			TS_ASSERT(obj2.getPolygonIndex() == -1);
	
			obj1.setSlope(2.5);
			obj1.setPolygonIndex(-3);
	
			TS_ASSERT_DELTA(obj1.getSlope(), 2.5, 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == -3);
	
			obj2 = obj1;
			TS_ASSERT_DELTA(obj1.getSlope(), obj2.getSlope(), 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == obj2.getPolygonIndex());
	
			TS_ASSERT_DELTA(obj2.getSlope(), 2.5, 1e-5);
			TS_ASSERT(obj2.getPolygonIndex() == -3);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}
	
		void testCreateAndSerializeObject() {
			using namespace msv;
	
			CustomLine obj1;
			TS_ASSERT_DELTA(obj1.getSlope(), 0, 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == -1);
	
			CustomLine obj2;
			TS_ASSERT_DELTA(obj2.getSlope(), 0, 1e-5);
			TS_ASSERT(obj2.getPolygonIndex() == -1);
	
			obj1.setSlope(2.5);
			obj1.setPolygonIndex(-3);
	
			TS_ASSERT_DELTA(obj1.getSlope(), 2.5, 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == -3);
	
			stringstream sstr;
			sstr << obj1;
			sstr >> obj2;
	
			TS_ASSERT_DELTA(obj1.getSlope(), obj2.getSlope(), 1e-5);
			TS_ASSERT(obj1.getPolygonIndex() == obj2.getPolygonIndex());
	
			TS_ASSERT_DELTA(obj2.getSlope(), 2.5, 1e-5);
			TS_ASSERT(obj2.getPolygonIndex() == -3);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}

};

#endif /*MSV_CUSTOMLINE_TESTSUITE_H*/
