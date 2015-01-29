/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_STM32F4CONTROL_TESTSUITE_H
#define MSV_STM32F4CONTROL_TESTSUITE_H

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


class msv_STM32F4Control_TestSuite : public CxxTest::TestSuite {

	public:
		void testCreateObject() {
			using namespace msv;
	
			STM32F4Control obj1;
			TS_ASSERT(obj1.getDataFeed() == 0);
	
			obj1.setDataFeed(4);
	
			TS_ASSERT(obj1.getDataFeed() == 4);
		}
	
		void testCreateAndCopyObject() {
			using namespace msv;
	
			STM32F4Control obj1;
			TS_ASSERT(obj1.getDataFeed() == 0);
	
			STM32F4Control obj2(obj1);
			TS_ASSERT(obj1.getDataFeed() == obj2.getDataFeed());
	
			obj1.setDataFeed(4);
	
			TS_ASSERT(obj1.getDataFeed() == 4);
	
			STM32F4Control obj3(obj1);
			TS_ASSERT(obj1.getDataFeed() == obj3.getDataFeed());
	
			TS_ASSERT(obj3.getDataFeed() == 4);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj3.toString()));
		}
	
		void testCreateAndAssignObject() {
			using namespace msv;
	
			STM32F4Control obj1;
			TS_ASSERT(obj1.getDataFeed() == 0);
	
			STM32F4Control obj2;
			TS_ASSERT(obj2.getDataFeed() == 0);
	
			obj1.setDataFeed(4);
	
			TS_ASSERT(obj1.getDataFeed() == 4);
	
			obj2 = obj1;
			TS_ASSERT(obj1.getDataFeed() == obj2.getDataFeed());
	
			TS_ASSERT(obj2.getDataFeed() == 4);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}
	
		void testCreateAndSerializeObject() {
			using namespace msv;
	
			STM32F4Control obj1;
			TS_ASSERT(obj1.getDataFeed() == 0);
	
			STM32F4Control obj2;
			TS_ASSERT(obj2.getDataFeed() == 0);
	
			obj1.setDataFeed(4);
	
			TS_ASSERT(obj1.getDataFeed() == 4);
	
			stringstream sstr;
			sstr << obj1;
			sstr >> obj2;
	
			TS_ASSERT(obj1.getDataFeed() == obj2.getDataFeed());
	
			TS_ASSERT(obj2.getDataFeed() == 4);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}

};

#endif /*MSV_STM32F4CONTROL_TESTSUITE_H*/
