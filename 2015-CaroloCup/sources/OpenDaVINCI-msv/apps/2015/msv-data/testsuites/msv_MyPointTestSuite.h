/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_MYPOINT_TESTSUITE_H
#define MSV_MYPOINT_TESTSUITE_H

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


class msv_MyPoint_TestSuite : public CxxTest::TestSuite {

	public:
		void testCreateObject() {
			using namespace msv;
	
			MyPoint obj1;
			TS_ASSERT_DELTA(obj1.getX(), 0.0, 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), 0.0, 1e-5);
	
			obj1.setX(1.0);
			obj1.setY(1.0);
	
			TS_ASSERT_DELTA(obj1.getX(), 1.0, 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), 1.0, 1e-5);
		}
	
		void testCreateAndCopyObject() {
			using namespace msv;
	
			MyPoint obj1;
			TS_ASSERT_DELTA(obj1.getX(), 0.0, 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), 0.0, 1e-5);
	
			MyPoint obj2(obj1);
			TS_ASSERT_DELTA(obj1.getX(), obj2.getX(), 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), obj2.getY(), 1e-5);
	
			obj1.setX(1.0);
			obj1.setY(1.0);
	
			TS_ASSERT_DELTA(obj1.getX(), 1.0, 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), 1.0, 1e-5);
	
			MyPoint obj3(obj1);
			TS_ASSERT_DELTA(obj1.getX(), obj3.getX(), 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), obj3.getY(), 1e-5);
	
			TS_ASSERT_DELTA(obj3.getX(), 1.0, 1e-5);
			TS_ASSERT_DELTA(obj3.getY(), 1.0, 1e-5);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj3.toString()));
		}
	
		void testCreateAndAssignObject() {
			using namespace msv;
	
			MyPoint obj1;
			TS_ASSERT_DELTA(obj1.getX(), 0.0, 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), 0.0, 1e-5);
	
			MyPoint obj2;
			TS_ASSERT_DELTA(obj2.getX(), 0.0, 1e-5);
			TS_ASSERT_DELTA(obj2.getY(), 0.0, 1e-5);
	
			obj1.setX(1.0);
			obj1.setY(1.0);
	
			TS_ASSERT_DELTA(obj1.getX(), 1.0, 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), 1.0, 1e-5);
	
			obj2 = obj1;
			TS_ASSERT_DELTA(obj1.getX(), obj2.getX(), 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), obj2.getY(), 1e-5);
	
			TS_ASSERT_DELTA(obj2.getX(), 1.0, 1e-5);
			TS_ASSERT_DELTA(obj2.getY(), 1.0, 1e-5);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}
	
		void testCreateAndSerializeObject() {
			using namespace msv;
	
			MyPoint obj1;
			TS_ASSERT_DELTA(obj1.getX(), 0.0, 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), 0.0, 1e-5);
	
			MyPoint obj2;
			TS_ASSERT_DELTA(obj2.getX(), 0.0, 1e-5);
			TS_ASSERT_DELTA(obj2.getY(), 0.0, 1e-5);
	
			obj1.setX(1.0);
			obj1.setY(1.0);
	
			TS_ASSERT_DELTA(obj1.getX(), 1.0, 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), 1.0, 1e-5);
	
			stringstream sstr;
			sstr << obj1;
			sstr >> obj2;
	
			TS_ASSERT_DELTA(obj1.getX(), obj2.getX(), 1e-5);
			TS_ASSERT_DELTA(obj1.getY(), obj2.getY(), 1e-5);
	
			TS_ASSERT_DELTA(obj2.getX(), 1.0, 1e-5);
			TS_ASSERT_DELTA(obj2.getY(), 1.0, 1e-5);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}

};

#endif /*MSV_MYPOINT_TESTSUITE_H*/
