/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_LANEDETECTORDATATODRIVER_TESTSUITE_H
#define MSV_LANEDETECTORDATATODRIVER_TESTSUITE_H

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

#include "generated/msv/CustomLine.h"
#include "generated/msv/CustomLine.h"

class msv_LaneDetectorDataToDriver_TestSuite : public CxxTest::TestSuite {

	public:
		void testCreateObject() {
			using namespace msv;
	
			LaneDetectorDataToDriver obj1;
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.getleftGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_leftGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_leftGoalLines());
			TS_ASSERT(obj1.getrightGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_rightGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_rightGoalLines());
			TS_ASSERT(obj1.getNoTrajectory() == false);
	
			{
				std::vector<int32_t> myExternalList;
				myExternalList.push_back(-3);
				myExternalList.push_back(-4);
				myExternalList.push_back(-5);
			
				obj1.clear_ListOfSwitchPointsLeftGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
				obj1.setListOfSwitchPointsLeftGoalLines(myExternalList);
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
				TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
				obj1.clear_ListOfSwitchPointsLeftGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			}
			obj1.addTo_ListOfSwitchPointsLeftGoalLines(-3);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 1);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 1);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
			obj1.addTo_ListOfSwitchPointsLeftGoalLines(-4);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 2);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 2);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
			obj1.insertTo_ListOfSwitchPointsLeftGoalLines(-5);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			{
				std::vector<int32_t> myExternalList;
				myExternalList.push_back(-3);
				myExternalList.push_back(-4);
				myExternalList.push_back(-5);
			
				obj1.clear_ListOfSwitchPointsRightGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
				obj1.setListOfSwitchPointsRightGoalLines(myExternalList);
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
				TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
				obj1.clear_ListOfSwitchPointsRightGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			}
			obj1.addTo_ListOfSwitchPointsRightGoalLines(-3);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 1);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 1);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
			obj1.addTo_ListOfSwitchPointsRightGoalLines(-4);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 2);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 2);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
			obj1.insertTo_ListOfSwitchPointsRightGoalLines(-5);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			obj1.setNoTrajectory(true);
	
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			TS_ASSERT(obj1.getNoTrajectory() == true);
		}
	
		void testCreateAndCopyObject() {
			using namespace msv;
	
			LaneDetectorDataToDriver obj1;
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.getleftGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_leftGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_leftGoalLines());
			TS_ASSERT(obj1.getrightGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_rightGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_rightGoalLines());
			TS_ASSERT(obj1.getNoTrajectory() == false);
	
			LaneDetectorDataToDriver obj2(obj1);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == obj2.getSize_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines() == obj2.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == obj2.getSize_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines() == obj2.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.getSize_leftGoalLines() == obj2.getSize_leftGoalLines());
			TS_ASSERT(obj1.isEmpty_leftGoalLines() == obj2.isEmpty_leftGoalLines());
			TS_ASSERT(obj1.getSize_rightGoalLines() == obj2.getSize_rightGoalLines());
			TS_ASSERT(obj1.isEmpty_rightGoalLines() == obj2.isEmpty_rightGoalLines());
			TS_ASSERT(obj1.getNoTrajectory() == obj2.getNoTrajectory());
	
			{
				std::vector<int32_t> myExternalList;
				myExternalList.push_back(-3);
				myExternalList.push_back(-4);
				myExternalList.push_back(-5);
			
				obj1.clear_ListOfSwitchPointsLeftGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
				obj1.setListOfSwitchPointsLeftGoalLines(myExternalList);
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
				TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
				obj1.clear_ListOfSwitchPointsLeftGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			}
			obj1.addTo_ListOfSwitchPointsLeftGoalLines(-3);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 1);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 1);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
			obj1.addTo_ListOfSwitchPointsLeftGoalLines(-4);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 2);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 2);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
			obj1.insertTo_ListOfSwitchPointsLeftGoalLines(-5);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			{
				std::vector<int32_t> myExternalList;
				myExternalList.push_back(-3);
				myExternalList.push_back(-4);
				myExternalList.push_back(-5);
			
				obj1.clear_ListOfSwitchPointsRightGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
				obj1.setListOfSwitchPointsRightGoalLines(myExternalList);
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
				TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
				obj1.clear_ListOfSwitchPointsRightGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			}
			obj1.addTo_ListOfSwitchPointsRightGoalLines(-3);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 1);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 1);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
			obj1.addTo_ListOfSwitchPointsRightGoalLines(-4);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 2);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 2);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
			obj1.insertTo_ListOfSwitchPointsRightGoalLines(-5);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			obj1.setNoTrajectory(true);
	
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			TS_ASSERT(obj1.getNoTrajectory() == true);
	
			LaneDetectorDataToDriver obj3(obj1);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == obj3.getSize_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines() == obj3.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == obj3.getSize_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines() == obj3.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.getSize_leftGoalLines() == obj3.getSize_leftGoalLines());
			TS_ASSERT(obj1.isEmpty_leftGoalLines() == obj3.isEmpty_leftGoalLines());
			TS_ASSERT(obj1.getSize_rightGoalLines() == obj3.getSize_rightGoalLines());
			TS_ASSERT(obj1.isEmpty_rightGoalLines() == obj3.isEmpty_rightGoalLines());
			TS_ASSERT(obj1.getNoTrajectory() == obj3.getNoTrajectory());
	
			TS_ASSERT(obj3.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj3.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj3.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj3.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj3.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj3.contains_ListOfSwitchPointsLeftGoalLines(-5));
			TS_ASSERT(obj3.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj3.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj3.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj3.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj3.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj3.contains_ListOfSwitchPointsRightGoalLines(-5));
			TS_ASSERT(obj3.getNoTrajectory() == true);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj3.toString()));
		}
	
		void testCreateAndAssignObject() {
			using namespace msv;
	
			LaneDetectorDataToDriver obj1;
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.getleftGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_leftGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_leftGoalLines());
			TS_ASSERT(obj1.getrightGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_rightGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_rightGoalLines());
			TS_ASSERT(obj1.getNoTrajectory() == false);
	
			LaneDetectorDataToDriver obj2;
			TS_ASSERT(obj2.getListOfSwitchPointsLeftGoalLines().size() == 0);
			TS_ASSERT(obj2.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
			TS_ASSERT(obj2.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj2.getListOfSwitchPointsRightGoalLines().size() == 0);
			TS_ASSERT(obj2.getSize_ListOfSwitchPointsRightGoalLines() == 0);
			TS_ASSERT(obj2.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj2.getleftGoalLines().size() == 0);
			TS_ASSERT(obj2.getSize_leftGoalLines() == 0);
			TS_ASSERT(obj2.isEmpty_leftGoalLines());
			TS_ASSERT(obj2.getrightGoalLines().size() == 0);
			TS_ASSERT(obj2.getSize_rightGoalLines() == 0);
			TS_ASSERT(obj2.isEmpty_rightGoalLines());
			TS_ASSERT(obj2.getNoTrajectory() == false);
	
			{
				std::vector<int32_t> myExternalList;
				myExternalList.push_back(-3);
				myExternalList.push_back(-4);
				myExternalList.push_back(-5);
			
				obj1.clear_ListOfSwitchPointsLeftGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
				obj1.setListOfSwitchPointsLeftGoalLines(myExternalList);
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
				TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
				obj1.clear_ListOfSwitchPointsLeftGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			}
			obj1.addTo_ListOfSwitchPointsLeftGoalLines(-3);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 1);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 1);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
			obj1.addTo_ListOfSwitchPointsLeftGoalLines(-4);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 2);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 2);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
			obj1.insertTo_ListOfSwitchPointsLeftGoalLines(-5);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			{
				std::vector<int32_t> myExternalList;
				myExternalList.push_back(-3);
				myExternalList.push_back(-4);
				myExternalList.push_back(-5);
			
				obj1.clear_ListOfSwitchPointsRightGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
				obj1.setListOfSwitchPointsRightGoalLines(myExternalList);
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
				TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
				obj1.clear_ListOfSwitchPointsRightGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			}
			obj1.addTo_ListOfSwitchPointsRightGoalLines(-3);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 1);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 1);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
			obj1.addTo_ListOfSwitchPointsRightGoalLines(-4);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 2);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 2);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
			obj1.insertTo_ListOfSwitchPointsRightGoalLines(-5);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			obj1.setNoTrajectory(true);
	
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			TS_ASSERT(obj1.getNoTrajectory() == true);
	
			obj2 = obj1;
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == obj2.getSize_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines() == obj2.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == obj2.getSize_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines() == obj2.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.getSize_leftGoalLines() == obj2.getSize_leftGoalLines());
			TS_ASSERT(obj1.isEmpty_leftGoalLines() == obj2.isEmpty_leftGoalLines());
			TS_ASSERT(obj1.getSize_rightGoalLines() == obj2.getSize_rightGoalLines());
			TS_ASSERT(obj1.isEmpty_rightGoalLines() == obj2.isEmpty_rightGoalLines());
			TS_ASSERT(obj1.getNoTrajectory() == obj2.getNoTrajectory());
	
			TS_ASSERT(obj2.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj2.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj2.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj2.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj2.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj2.contains_ListOfSwitchPointsLeftGoalLines(-5));
			TS_ASSERT(obj2.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj2.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj2.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj2.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj2.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj2.contains_ListOfSwitchPointsRightGoalLines(-5));
			TS_ASSERT(obj2.getNoTrajectory() == true);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}
	
		void testCreateAndSerializeObject() {
			using namespace msv;
	
			LaneDetectorDataToDriver obj1;
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.getleftGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_leftGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_leftGoalLines());
			TS_ASSERT(obj1.getrightGoalLines().size() == 0);
			TS_ASSERT(obj1.getSize_rightGoalLines() == 0);
			TS_ASSERT(obj1.isEmpty_rightGoalLines());
			TS_ASSERT(obj1.getNoTrajectory() == false);
	
			LaneDetectorDataToDriver obj2;
			TS_ASSERT(obj2.getListOfSwitchPointsLeftGoalLines().size() == 0);
			TS_ASSERT(obj2.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
			TS_ASSERT(obj2.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj2.getListOfSwitchPointsRightGoalLines().size() == 0);
			TS_ASSERT(obj2.getSize_ListOfSwitchPointsRightGoalLines() == 0);
			TS_ASSERT(obj2.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj2.getleftGoalLines().size() == 0);
			TS_ASSERT(obj2.getSize_leftGoalLines() == 0);
			TS_ASSERT(obj2.isEmpty_leftGoalLines());
			TS_ASSERT(obj2.getrightGoalLines().size() == 0);
			TS_ASSERT(obj2.getSize_rightGoalLines() == 0);
			TS_ASSERT(obj2.isEmpty_rightGoalLines());
			TS_ASSERT(obj2.getNoTrajectory() == false);
	
			{
				std::vector<int32_t> myExternalList;
				myExternalList.push_back(-3);
				myExternalList.push_back(-4);
				myExternalList.push_back(-5);
			
				obj1.clear_ListOfSwitchPointsLeftGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
				obj1.setListOfSwitchPointsLeftGoalLines(myExternalList);
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
				TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
				obj1.clear_ListOfSwitchPointsLeftGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			}
			obj1.addTo_ListOfSwitchPointsLeftGoalLines(-3);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 1);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 1);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
			obj1.addTo_ListOfSwitchPointsLeftGoalLines(-4);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 2);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 2);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			
			obj1.insertTo_ListOfSwitchPointsLeftGoalLines(-5);
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			{
				std::vector<int32_t> myExternalList;
				myExternalList.push_back(-3);
				myExternalList.push_back(-4);
				myExternalList.push_back(-5);
			
				obj1.clear_ListOfSwitchPointsRightGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
				obj1.setListOfSwitchPointsRightGoalLines(myExternalList);
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
				TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
				obj1.clear_ListOfSwitchPointsRightGoalLines();
				TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 0);
				TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 0);
				TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
				TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			}
			obj1.addTo_ListOfSwitchPointsRightGoalLines(-3);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 1);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 1);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
			obj1.addTo_ListOfSwitchPointsRightGoalLines(-4);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 2);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 2);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(!obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			
			obj1.insertTo_ListOfSwitchPointsRightGoalLines(-5);
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			obj1.setNoTrajectory(true);
	
			TS_ASSERT(obj1.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsLeftGoalLines(-5));
			TS_ASSERT(obj1.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj1.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj1.contains_ListOfSwitchPointsRightGoalLines(-5));
			TS_ASSERT(obj1.getNoTrajectory() == true);
	
			stringstream sstr;
			sstr << obj1;
			sstr >> obj2;
	
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsLeftGoalLines() == obj2.getSize_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsLeftGoalLines() == obj2.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj1.getSize_ListOfSwitchPointsRightGoalLines() == obj2.getSize_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.isEmpty_ListOfSwitchPointsRightGoalLines() == obj2.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj1.getSize_leftGoalLines() == obj2.getSize_leftGoalLines());
			TS_ASSERT(obj1.isEmpty_leftGoalLines() == obj2.isEmpty_leftGoalLines());
			TS_ASSERT(obj1.getSize_rightGoalLines() == obj2.getSize_rightGoalLines());
			TS_ASSERT(obj1.isEmpty_rightGoalLines() == obj2.isEmpty_rightGoalLines());
			TS_ASSERT(obj1.getNoTrajectory() == obj2.getNoTrajectory());
	
			TS_ASSERT(obj2.getListOfSwitchPointsLeftGoalLines().size() == 3);
			TS_ASSERT(obj2.getSize_ListOfSwitchPointsLeftGoalLines() == 3);
			TS_ASSERT(!obj2.isEmpty_ListOfSwitchPointsLeftGoalLines());
			TS_ASSERT(obj2.contains_ListOfSwitchPointsLeftGoalLines(-3));
			TS_ASSERT(obj2.contains_ListOfSwitchPointsLeftGoalLines(-4));
			TS_ASSERT(obj2.contains_ListOfSwitchPointsLeftGoalLines(-5));
			TS_ASSERT(obj2.getListOfSwitchPointsRightGoalLines().size() == 3);
			TS_ASSERT(obj2.getSize_ListOfSwitchPointsRightGoalLines() == 3);
			TS_ASSERT(!obj2.isEmpty_ListOfSwitchPointsRightGoalLines());
			TS_ASSERT(obj2.contains_ListOfSwitchPointsRightGoalLines(-3));
			TS_ASSERT(obj2.contains_ListOfSwitchPointsRightGoalLines(-4));
			TS_ASSERT(obj2.contains_ListOfSwitchPointsRightGoalLines(-5));
			TS_ASSERT(obj2.getNoTrajectory() == true);
	
			TS_ASSERT(core::StringToolbox::equalsIgnoreCase(obj1.toString(), obj2.toString()));
		}

};

#endif /*MSV_LANEDETECTORDATATODRIVER_TESTSUITE_H*/
