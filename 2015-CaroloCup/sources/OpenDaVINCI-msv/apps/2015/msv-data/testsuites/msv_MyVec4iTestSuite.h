/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_MYVEC4I_TESTSUITE_H
#define MSV_MYVEC4I_TESTSUITE_H

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


class msv_MyVec4i_TestSuite : public CxxTest::TestSuite {

	public:
		void testCreateObject() {
			using namespace msv;
	
			MyVec4i obj1;
			TS_ASSERT(obj1.getX() == 0);
			TS_ASSERT(obj1.getY() == 0);
			TS_ASSERT(obj1.getZ() == 0);
			TS_ASSERT(obj1.getW() == 0);
	
			obj1.setX(-3);
			obj1.setY(-3);
			obj1.setZ(-3);
			obj1.setW(-3);
	
			TS_ASSERT(obj1.getX() == -3);
			TS_ASSERT(obj1.getY() == -3);
			TS_ASSERT(obj1.getZ() == -3);
			TS_ASSERT(obj1.getW() == -3);
		}
	
		void testCreateAndCopyObject() {
			using namespace msv;
	
			MyVec4i obj1;
			TS_ASSERT(obj1.getX() == 0);
			TS_ASSERT(obj1.getY() == 0);
			TS_ASSERT(obj1.getZ() == 0);
			TS_ASSERT(obj1.getW() == 0);
	
			MyVec4i obj2(obj1);
			TS_ASSERT(obj1.getX() == obj2.getX());
			TS_ASSERT(obj1.getY() == obj2.getY());
			TS_ASSERT(obj1.getZ() == obj2.getZ());
			TS_ASSERT(obj1.getW() == obj2.getW());
	
			obj1.setX(-3);
			obj1.setY(-3);
			obj1.setZ(-3);
			obj1.setW(-3);
	
			TS_ASSERT(obj1.getX() == -3);
			TS_ASSERT(obj1.getY() == -3);
			TS_ASSERT(obj1.getZ() == -3);
			TS_ASSERT(obj1.getW() == -3);
	
			MyVec4i obj3(obj1);
			TS_ASSERT(obj1.getX() == obj3.getX());
			TS_ASSERT(obj1.getY() == obj3.getY());
			TS_ASSERT(obj1.getZ() == obj3.getZ());
			TS_ASSERT(obj1.getW() == obj3.getW());
	
			TS_ASSERT(obj3.getX() == -3);
			TS_ASSERT(obj3.getY() == -3);
			TS_ASSERT(obj3.getZ() == -3);
			TS_ASSERT(obj3.getW() == -3);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj3.toString()));
		}
	
		void testCreateAndAssignObject() {
			using namespace msv;
	
			MyVec4i obj1;
			TS_ASSERT(obj1.getX() == 0);
			TS_ASSERT(obj1.getY() == 0);
			TS_ASSERT(obj1.getZ() == 0);
			TS_ASSERT(obj1.getW() == 0);
	
			MyVec4i obj2;
			TS_ASSERT(obj2.getX() == 0);
			TS_ASSERT(obj2.getY() == 0);
			TS_ASSERT(obj2.getZ() == 0);
			TS_ASSERT(obj2.getW() == 0);
	
			obj1.setX(-3);
			obj1.setY(-3);
			obj1.setZ(-3);
			obj1.setW(-3);
	
			TS_ASSERT(obj1.getX() == -3);
			TS_ASSERT(obj1.getY() == -3);
			TS_ASSERT(obj1.getZ() == -3);
			TS_ASSERT(obj1.getW() == -3);
	
			obj2 = obj1;
			TS_ASSERT(obj1.getX() == obj2.getX());
			TS_ASSERT(obj1.getY() == obj2.getY());
			TS_ASSERT(obj1.getZ() == obj2.getZ());
			TS_ASSERT(obj1.getW() == obj2.getW());
	
			TS_ASSERT(obj2.getX() == -3);
			TS_ASSERT(obj2.getY() == -3);
			TS_ASSERT(obj2.getZ() == -3);
			TS_ASSERT(obj2.getW() == -3);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}
	
		void testCreateAndSerializeObject() {
			using namespace msv;
	
			MyVec4i obj1;
			TS_ASSERT(obj1.getX() == 0);
			TS_ASSERT(obj1.getY() == 0);
			TS_ASSERT(obj1.getZ() == 0);
			TS_ASSERT(obj1.getW() == 0);
	
			MyVec4i obj2;
			TS_ASSERT(obj2.getX() == 0);
			TS_ASSERT(obj2.getY() == 0);
			TS_ASSERT(obj2.getZ() == 0);
			TS_ASSERT(obj2.getW() == 0);
	
			obj1.setX(-3);
			obj1.setY(-3);
			obj1.setZ(-3);
			obj1.setW(-3);
	
			TS_ASSERT(obj1.getX() == -3);
			TS_ASSERT(obj1.getY() == -3);
			TS_ASSERT(obj1.getZ() == -3);
			TS_ASSERT(obj1.getW() == -3);
	
			stringstream sstr;
			sstr << obj1;
			sstr >> obj2;
	
			TS_ASSERT(obj1.getX() == obj2.getX());
			TS_ASSERT(obj1.getY() == obj2.getY());
			TS_ASSERT(obj1.getZ() == obj2.getZ());
			TS_ASSERT(obj1.getW() == obj2.getW());
	
			TS_ASSERT(obj2.getX() == -3);
			TS_ASSERT(obj2.getY() == -3);
			TS_ASSERT(obj2.getZ() == -3);
			TS_ASSERT(obj2.getW() == -3);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}

};

#endif /*MSV_MYVEC4I_TESTSUITE_H*/
