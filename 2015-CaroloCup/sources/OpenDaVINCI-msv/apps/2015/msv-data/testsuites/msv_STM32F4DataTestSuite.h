/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_STM32F4DATA_TESTSUITE_H
#define MSV_STM32F4DATA_TESTSUITE_H

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


class msv_STM32F4Data_TestSuite : public CxxTest::TestSuite {

	public:
		void testCreateObject() {
			using namespace msv;
	
			STM32F4Data obj1;
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), ""));
	
			obj1.setRawData("Hello World!");
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), "Hello World!"));
		}
	
		void testCreateAndCopyObject() {
			using namespace msv;
	
			STM32F4Data obj1;
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), ""));
	
			STM32F4Data obj2(obj1);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), obj2.getRawData()));
	
			obj1.setRawData("Hello World!");
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), "Hello World!"));
	
			STM32F4Data obj3(obj1);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), obj3.getRawData()));
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj3.getRawData(), "Hello World!"));
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj3.toString()));
		}
	
		void testCreateAndAssignObject() {
			using namespace msv;
	
			STM32F4Data obj1;
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), ""));
	
			STM32F4Data obj2;
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj2.getRawData(), ""));
	
			obj1.setRawData("Hello World!");
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), "Hello World!"));
	
			obj2 = obj1;
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), obj2.getRawData()));
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj2.getRawData(), "Hello World!"));
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}
	
		void testCreateAndSerializeObject() {
			using namespace msv;
	
			STM32F4Data obj1;
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), ""));
	
			STM32F4Data obj2;
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj2.getRawData(), ""));
	
			obj1.setRawData("Hello World!");
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), "Hello World!"));
	
			stringstream sstr;
			sstr << obj1;
			sstr >> obj2;
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getRawData(), obj2.getRawData()));
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj2.getRawData(), "Hello World!"));
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}

};

#endif /*MSV_STM32F4DATA_TESTSUITE_H*/
