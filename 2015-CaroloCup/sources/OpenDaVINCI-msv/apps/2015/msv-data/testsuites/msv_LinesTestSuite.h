/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_LINES_TESTSUITE_H
#define MSV_LINES_TESTSUITE_H

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


class msv_Lines_TestSuite : public CxxTest::TestSuite {

	public:
		void testCreateObject() {
			using namespace msv;
	
			Lines obj1;
			TS_ASSERT(obj1.getPGain() == 0);
			TS_ASSERT(obj1.getIntGain() == 0);
			TS_ASSERT(obj1.getDerGain() == 0);
			TS_ASSERT(obj1.getSpeed() == 0);
			TS_ASSERT(obj1.getWidth() == 0);
			TS_ASSERT(obj1.getHeight() == 0);
			TS_ASSERT(obj1.getStartLineHeight() == 0);
			TS_ASSERT(obj1.getStopLineHeight() == 0);
			TS_ASSERT(obj1.getConfidenceLevel() == 0);
	
			obj1.setPGain(-3);
			obj1.setIntGain(-3);
			obj1.setDerGain(-3);
			obj1.setSpeed(-3);
			obj1.setWidth(-3);
			obj1.setHeight(-3);
			obj1.setStartLineHeight(-3);
			obj1.setStopLineHeight(-3);
			obj1.setConfidenceLevel(-3);
	
			TS_ASSERT(obj1.getPGain() == -3);
			TS_ASSERT(obj1.getIntGain() == -3);
			TS_ASSERT(obj1.getDerGain() == -3);
			TS_ASSERT(obj1.getSpeed() == -3);
			TS_ASSERT(obj1.getWidth() == -3);
			TS_ASSERT(obj1.getHeight() == -3);
			TS_ASSERT(obj1.getStartLineHeight() == -3);
			TS_ASSERT(obj1.getStopLineHeight() == -3);
			TS_ASSERT(obj1.getConfidenceLevel() == -3);
		}
	
		void testCreateAndCopyObject() {
			using namespace msv;
	
			Lines obj1;
			TS_ASSERT(obj1.getPGain() == 0);
			TS_ASSERT(obj1.getIntGain() == 0);
			TS_ASSERT(obj1.getDerGain() == 0);
			TS_ASSERT(obj1.getSpeed() == 0);
			TS_ASSERT(obj1.getWidth() == 0);
			TS_ASSERT(obj1.getHeight() == 0);
			TS_ASSERT(obj1.getStartLineHeight() == 0);
			TS_ASSERT(obj1.getStopLineHeight() == 0);
			TS_ASSERT(obj1.getConfidenceLevel() == 0);
	
			Lines obj2(obj1);
			TS_ASSERT(obj1.getPGain() == obj2.getPGain());
			TS_ASSERT(obj1.getIntGain() == obj2.getIntGain());
			TS_ASSERT(obj1.getDerGain() == obj2.getDerGain());
			TS_ASSERT(obj1.getSpeed() == obj2.getSpeed());
			TS_ASSERT(obj1.getWidth() == obj2.getWidth());
			TS_ASSERT(obj1.getHeight() == obj2.getHeight());
			TS_ASSERT(obj1.getStartLineHeight() == obj2.getStartLineHeight());
			TS_ASSERT(obj1.getStopLineHeight() == obj2.getStopLineHeight());
			TS_ASSERT(obj1.getConfidenceLevel() == obj2.getConfidenceLevel());
	
			obj1.setPGain(-3);
			obj1.setIntGain(-3);
			obj1.setDerGain(-3);
			obj1.setSpeed(-3);
			obj1.setWidth(-3);
			obj1.setHeight(-3);
			obj1.setStartLineHeight(-3);
			obj1.setStopLineHeight(-3);
			obj1.setConfidenceLevel(-3);
	
			TS_ASSERT(obj1.getPGain() == -3);
			TS_ASSERT(obj1.getIntGain() == -3);
			TS_ASSERT(obj1.getDerGain() == -3);
			TS_ASSERT(obj1.getSpeed() == -3);
			TS_ASSERT(obj1.getWidth() == -3);
			TS_ASSERT(obj1.getHeight() == -3);
			TS_ASSERT(obj1.getStartLineHeight() == -3);
			TS_ASSERT(obj1.getStopLineHeight() == -3);
			TS_ASSERT(obj1.getConfidenceLevel() == -3);
	
			Lines obj3(obj1);
			TS_ASSERT(obj1.getPGain() == obj3.getPGain());
			TS_ASSERT(obj1.getIntGain() == obj3.getIntGain());
			TS_ASSERT(obj1.getDerGain() == obj3.getDerGain());
			TS_ASSERT(obj1.getSpeed() == obj3.getSpeed());
			TS_ASSERT(obj1.getWidth() == obj3.getWidth());
			TS_ASSERT(obj1.getHeight() == obj3.getHeight());
			TS_ASSERT(obj1.getStartLineHeight() == obj3.getStartLineHeight());
			TS_ASSERT(obj1.getStopLineHeight() == obj3.getStopLineHeight());
			TS_ASSERT(obj1.getConfidenceLevel() == obj3.getConfidenceLevel());
	
			TS_ASSERT(obj3.getPGain() == -3);
			TS_ASSERT(obj3.getIntGain() == -3);
			TS_ASSERT(obj3.getDerGain() == -3);
			TS_ASSERT(obj3.getSpeed() == -3);
			TS_ASSERT(obj3.getWidth() == -3);
			TS_ASSERT(obj3.getHeight() == -3);
			TS_ASSERT(obj3.getStartLineHeight() == -3);
			TS_ASSERT(obj3.getStopLineHeight() == -3);
			TS_ASSERT(obj3.getConfidenceLevel() == -3);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj3.toString()));
		}
	
		void testCreateAndAssignObject() {
			using namespace msv;
	
			Lines obj1;
			TS_ASSERT(obj1.getPGain() == 0);
			TS_ASSERT(obj1.getIntGain() == 0);
			TS_ASSERT(obj1.getDerGain() == 0);
			TS_ASSERT(obj1.getSpeed() == 0);
			TS_ASSERT(obj1.getWidth() == 0);
			TS_ASSERT(obj1.getHeight() == 0);
			TS_ASSERT(obj1.getStartLineHeight() == 0);
			TS_ASSERT(obj1.getStopLineHeight() == 0);
			TS_ASSERT(obj1.getConfidenceLevel() == 0);
	
			Lines obj2;
			TS_ASSERT(obj2.getPGain() == 0);
			TS_ASSERT(obj2.getIntGain() == 0);
			TS_ASSERT(obj2.getDerGain() == 0);
			TS_ASSERT(obj2.getSpeed() == 0);
			TS_ASSERT(obj2.getWidth() == 0);
			TS_ASSERT(obj2.getHeight() == 0);
			TS_ASSERT(obj2.getStartLineHeight() == 0);
			TS_ASSERT(obj2.getStopLineHeight() == 0);
			TS_ASSERT(obj2.getConfidenceLevel() == 0);
	
			obj1.setPGain(-3);
			obj1.setIntGain(-3);
			obj1.setDerGain(-3);
			obj1.setSpeed(-3);
			obj1.setWidth(-3);
			obj1.setHeight(-3);
			obj1.setStartLineHeight(-3);
			obj1.setStopLineHeight(-3);
			obj1.setConfidenceLevel(-3);
	
			TS_ASSERT(obj1.getPGain() == -3);
			TS_ASSERT(obj1.getIntGain() == -3);
			TS_ASSERT(obj1.getDerGain() == -3);
			TS_ASSERT(obj1.getSpeed() == -3);
			TS_ASSERT(obj1.getWidth() == -3);
			TS_ASSERT(obj1.getHeight() == -3);
			TS_ASSERT(obj1.getStartLineHeight() == -3);
			TS_ASSERT(obj1.getStopLineHeight() == -3);
			TS_ASSERT(obj1.getConfidenceLevel() == -3);
	
			obj2 = obj1;
			TS_ASSERT(obj1.getPGain() == obj2.getPGain());
			TS_ASSERT(obj1.getIntGain() == obj2.getIntGain());
			TS_ASSERT(obj1.getDerGain() == obj2.getDerGain());
			TS_ASSERT(obj1.getSpeed() == obj2.getSpeed());
			TS_ASSERT(obj1.getWidth() == obj2.getWidth());
			TS_ASSERT(obj1.getHeight() == obj2.getHeight());
			TS_ASSERT(obj1.getStartLineHeight() == obj2.getStartLineHeight());
			TS_ASSERT(obj1.getStopLineHeight() == obj2.getStopLineHeight());
			TS_ASSERT(obj1.getConfidenceLevel() == obj2.getConfidenceLevel());
	
			TS_ASSERT(obj2.getPGain() == -3);
			TS_ASSERT(obj2.getIntGain() == -3);
			TS_ASSERT(obj2.getDerGain() == -3);
			TS_ASSERT(obj2.getSpeed() == -3);
			TS_ASSERT(obj2.getWidth() == -3);
			TS_ASSERT(obj2.getHeight() == -3);
			TS_ASSERT(obj2.getStartLineHeight() == -3);
			TS_ASSERT(obj2.getStopLineHeight() == -3);
			TS_ASSERT(obj2.getConfidenceLevel() == -3);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}
	
		void testCreateAndSerializeObject() {
			using namespace msv;
	
			Lines obj1;
			TS_ASSERT(obj1.getPGain() == 0);
			TS_ASSERT(obj1.getIntGain() == 0);
			TS_ASSERT(obj1.getDerGain() == 0);
			TS_ASSERT(obj1.getSpeed() == 0);
			TS_ASSERT(obj1.getWidth() == 0);
			TS_ASSERT(obj1.getHeight() == 0);
			TS_ASSERT(obj1.getStartLineHeight() == 0);
			TS_ASSERT(obj1.getStopLineHeight() == 0);
			TS_ASSERT(obj1.getConfidenceLevel() == 0);
	
			Lines obj2;
			TS_ASSERT(obj2.getPGain() == 0);
			TS_ASSERT(obj2.getIntGain() == 0);
			TS_ASSERT(obj2.getDerGain() == 0);
			TS_ASSERT(obj2.getSpeed() == 0);
			TS_ASSERT(obj2.getWidth() == 0);
			TS_ASSERT(obj2.getHeight() == 0);
			TS_ASSERT(obj2.getStartLineHeight() == 0);
			TS_ASSERT(obj2.getStopLineHeight() == 0);
			TS_ASSERT(obj2.getConfidenceLevel() == 0);
	
			obj1.setPGain(-3);
			obj1.setIntGain(-3);
			obj1.setDerGain(-3);
			obj1.setSpeed(-3);
			obj1.setWidth(-3);
			obj1.setHeight(-3);
			obj1.setStartLineHeight(-3);
			obj1.setStopLineHeight(-3);
			obj1.setConfidenceLevel(-3);
	
			TS_ASSERT(obj1.getPGain() == -3);
			TS_ASSERT(obj1.getIntGain() == -3);
			TS_ASSERT(obj1.getDerGain() == -3);
			TS_ASSERT(obj1.getSpeed() == -3);
			TS_ASSERT(obj1.getWidth() == -3);
			TS_ASSERT(obj1.getHeight() == -3);
			TS_ASSERT(obj1.getStartLineHeight() == -3);
			TS_ASSERT(obj1.getStopLineHeight() == -3);
			TS_ASSERT(obj1.getConfidenceLevel() == -3);
	
			stringstream sstr;
			sstr << obj1;
			sstr >> obj2;
	
			TS_ASSERT(obj1.getPGain() == obj2.getPGain());
			TS_ASSERT(obj1.getIntGain() == obj2.getIntGain());
			TS_ASSERT(obj1.getDerGain() == obj2.getDerGain());
			TS_ASSERT(obj1.getSpeed() == obj2.getSpeed());
			TS_ASSERT(obj1.getWidth() == obj2.getWidth());
			TS_ASSERT(obj1.getHeight() == obj2.getHeight());
			TS_ASSERT(obj1.getStartLineHeight() == obj2.getStartLineHeight());
			TS_ASSERT(obj1.getStopLineHeight() == obj2.getStopLineHeight());
			TS_ASSERT(obj1.getConfidenceLevel() == obj2.getConfidenceLevel());
	
			TS_ASSERT(obj2.getPGain() == -3);
			TS_ASSERT(obj2.getIntGain() == -3);
			TS_ASSERT(obj2.getDerGain() == -3);
			TS_ASSERT(obj2.getSpeed() == -3);
			TS_ASSERT(obj2.getWidth() == -3);
			TS_ASSERT(obj2.getHeight() == -3);
			TS_ASSERT(obj2.getStartLineHeight() == -3);
			TS_ASSERT(obj2.getStopLineHeight() == -3);
			TS_ASSERT(obj2.getConfidenceLevel() == -3);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}

};

#endif /*MSV_LINES_TESTSUITE_H*/
