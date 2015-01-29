/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_LANEDETECTORDATA_TESTSUITE_H
#define MSV_LANEDETECTORDATA_TESTSUITE_H

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


class msv_LaneDetectorData_TestSuite : public CxxTest::TestSuite {

	public:
		void testCreateObject() {
			using namespace msv;
	
			LaneDetectorData obj1;
			TS_ASSERT(obj1.getFrameCount() == 0);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), ""));
	
			obj1.setFrameCount(4);
			obj1.setClassification("Hello World!");
	
			TS_ASSERT(obj1.getFrameCount() == 4);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), "Hello World!"));
		}
	
		void testCreateAndCopyObject() {
			using namespace msv;
	
			LaneDetectorData obj1;
			TS_ASSERT(obj1.getFrameCount() == 0);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), ""));
	
			LaneDetectorData obj2(obj1);
			TS_ASSERT(obj1.getFrameCount() == obj2.getFrameCount());
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), obj2.getClassification()));
	
			obj1.setFrameCount(4);
			obj1.setClassification("Hello World!");
	
			TS_ASSERT(obj1.getFrameCount() == 4);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), "Hello World!"));
	
			LaneDetectorData obj3(obj1);
			TS_ASSERT(obj1.getFrameCount() == obj3.getFrameCount());
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), obj3.getClassification()));
	
			TS_ASSERT(obj3.getFrameCount() == 4);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj3.getClassification(), "Hello World!"));
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj3.toString()));
		}
	
		void testCreateAndAssignObject() {
			using namespace msv;
	
			LaneDetectorData obj1;
			TS_ASSERT(obj1.getFrameCount() == 0);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), ""));
	
			LaneDetectorData obj2;
			TS_ASSERT(obj2.getFrameCount() == 0);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj2.getClassification(), ""));
	
			obj1.setFrameCount(4);
			obj1.setClassification("Hello World!");
	
			TS_ASSERT(obj1.getFrameCount() == 4);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), "Hello World!"));
	
			obj2 = obj1;
			TS_ASSERT(obj1.getFrameCount() == obj2.getFrameCount());
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), obj2.getClassification()));
	
			TS_ASSERT(obj2.getFrameCount() == 4);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj2.getClassification(), "Hello World!"));
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}
	
		void testCreateAndSerializeObject() {
			using namespace msv;
	
			LaneDetectorData obj1;
			TS_ASSERT(obj1.getFrameCount() == 0);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), ""));
	
			LaneDetectorData obj2;
			TS_ASSERT(obj2.getFrameCount() == 0);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj2.getClassification(), ""));
	
			obj1.setFrameCount(4);
			obj1.setClassification("Hello World!");
	
			TS_ASSERT(obj1.getFrameCount() == 4);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), "Hello World!"));
	
			stringstream sstr;
			sstr << obj1;
			sstr >> obj2;
	
			TS_ASSERT(obj1.getFrameCount() == obj2.getFrameCount());
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.getClassification(), obj2.getClassification()));
	
			TS_ASSERT(obj2.getFrameCount() == 4);
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj2.getClassification(), "Hello World!"));
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}

};

#endif /*MSV_LANEDETECTORDATA_TESTSUITE_H*/
