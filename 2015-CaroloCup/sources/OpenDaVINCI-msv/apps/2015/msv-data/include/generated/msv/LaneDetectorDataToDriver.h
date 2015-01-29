/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_LANEDETECTORDATATODRIVER_H
#define MSV_LANEDETECTORDATATODRIVER_H

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <vector>

#include "core/data/SerializableData.h"

#include "generated/msv/CustomLine.h"
#include "generated/msv/CustomLine.h"
#include "generated/msv/CustomLine.h"

namespace msv {
	using namespace std;
	
	class LaneDetectorDataToDriver : public core::data::SerializableData {
		public:
			LaneDetectorDataToDriver();
	
			virtual ~LaneDetectorDataToDriver();
	
			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			LaneDetectorDataToDriver(const LaneDetectorDataToDriver &obj);
	
			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			LaneDetectorDataToDriver& operator=(const LaneDetectorDataToDriver &obj);
	
		public:
			/**
			 * @return switchPointsLeftGoalLines.
			 */
			std::vector<int32_t> getListOfSwitchPointsLeftGoalLines() const;
		
			/**
			 * This method sets switchPointsLeftGoalLines.
			 *
			 * @param val Value for switchPointsLeftGoalLines.
			 */
			void setListOfSwitchPointsLeftGoalLines(const std::vector<int32_t> &val);
		
			/**
			 * This method clears the list of switchPointsLeftGoalLines.
			 */
			void clear_ListOfSwitchPointsLeftGoalLines();
		
			/**
			 * @return the size of the list of switchPointsLeftGoalLines.
			 */
			uint32_t getSize_ListOfSwitchPointsLeftGoalLines() const;
		
			/**
			 * @return true if the list of switchPointsLeftGoalLines is empty.
			 */
			bool isEmpty_ListOfSwitchPointsLeftGoalLines() const;
		
			/**
			 * This method adds an element to the end of the list of switchPointsLeftGoalLines.
			 *
			 * @param val Value to be added to the end of the list of switchPointsLeftGoalLines.
			 */
			void addTo_ListOfSwitchPointsLeftGoalLines(const int32_t &val);
		
			/**
			 * This method adds an element to the beginning of the list of switchPointsLeftGoalLines.
			 *
			 * @param val Value to be inserted to the beginning of the list of switchPointsLeftGoalLines.
			 */
			void insertTo_ListOfSwitchPointsLeftGoalLines(const int32_t &val);
		
			/**
			 * @return true if the list of switchPointsLeftGoalLines is contains the element val.
			 *              If the list has a complex data type, the entries are compared using their .toString() method.
			 */
			bool contains_ListOfSwitchPointsLeftGoalLines(const int32_t &val) const;
		
			/**
			 * @return Pair of iterators for the begin and end of the list of switchPointsLeftGoalLines.
			 */
			std::pair<std::vector<int32_t>::iterator, std::vector<int32_t>::iterator> iteratorPair_ListOfSwitchPointsLeftGoalLines();
		public:
			/**
			 * @return switchPointsRightGoalLines.
			 */
			std::vector<int32_t> getListOfSwitchPointsRightGoalLines() const;
		
			/**
			 * This method sets switchPointsRightGoalLines.
			 *
			 * @param val Value for switchPointsRightGoalLines.
			 */
			void setListOfSwitchPointsRightGoalLines(const std::vector<int32_t> &val);
		
			/**
			 * This method clears the list of switchPointsRightGoalLines.
			 */
			void clear_ListOfSwitchPointsRightGoalLines();
		
			/**
			 * @return the size of the list of switchPointsRightGoalLines.
			 */
			uint32_t getSize_ListOfSwitchPointsRightGoalLines() const;
		
			/**
			 * @return true if the list of switchPointsRightGoalLines is empty.
			 */
			bool isEmpty_ListOfSwitchPointsRightGoalLines() const;
		
			/**
			 * This method adds an element to the end of the list of switchPointsRightGoalLines.
			 *
			 * @param val Value to be added to the end of the list of switchPointsRightGoalLines.
			 */
			void addTo_ListOfSwitchPointsRightGoalLines(const int32_t &val);
		
			/**
			 * This method adds an element to the beginning of the list of switchPointsRightGoalLines.
			 *
			 * @param val Value to be inserted to the beginning of the list of switchPointsRightGoalLines.
			 */
			void insertTo_ListOfSwitchPointsRightGoalLines(const int32_t &val);
		
			/**
			 * @return true if the list of switchPointsRightGoalLines is contains the element val.
			 *              If the list has a complex data type, the entries are compared using their .toString() method.
			 */
			bool contains_ListOfSwitchPointsRightGoalLines(const int32_t &val) const;
		
			/**
			 * @return Pair of iterators for the begin and end of the list of switchPointsRightGoalLines.
			 */
			std::pair<std::vector<int32_t>::iterator, std::vector<int32_t>::iterator> iteratorPair_ListOfSwitchPointsRightGoalLines();
		public:
			/**
			 * @return leftGoalLines.
			 */
			std::vector<CustomLine> getleftGoalLines() const;
		
			/**
			 * This method sets leftGoalLines.
			 *
			 * @param val Value for leftGoalLines.
			 */
			void setleftGoalLines(const std::vector<CustomLine> &val);
		
			/**
			 * This method clears the list of leftGoalLines.
			 */
			void clear_leftGoalLines();
		
			/**
			 * @return the size of the list of leftGoalLines.
			 */
			uint32_t getSize_leftGoalLines() const;
		
			/**
			 * @return true if the list of leftGoalLines is empty.
			 */
			bool isEmpty_leftGoalLines() const;
		
			/**
			 * This method adds an element to the end of the list of leftGoalLines.
			 *
			 * @param val Value to be added to the end of the list of leftGoalLines.
			 */
			void addTo_leftGoalLines(const CustomLine &val);
		
			/**
			 * This method adds an element to the beginning of the list of leftGoalLines.
			 *
			 * @param val Value to be inserted to the beginning of the list of leftGoalLines.
			 */
			void insertTo_leftGoalLines(const CustomLine &val);
		
			/**
			 * @return true if the list of leftGoalLines is contains the element val.
			 *              If the list has a complex data type, the entries are compared using their .toString() method.
			 */
			bool contains_leftGoalLines(const CustomLine &val) const;
		
			/**
			 * @return Pair of iterators for the begin and end of the list of leftGoalLines.
			 */
			std::pair<std::vector<CustomLine>::iterator, std::vector<CustomLine>::iterator> iteratorPair_leftGoalLines();
		public:
			/**
			 * @return rightGoalLines.
			 */
			std::vector<CustomLine> getrightGoalLines() const;
		
			/**
			 * This method sets rightGoalLines.
			 *
			 * @param val Value for rightGoalLines.
			 */
			void setrightGoalLines(const std::vector<CustomLine> &val);
		
			/**
			 * This method clears the list of rightGoalLines.
			 */
			void clear_rightGoalLines();
		
			/**
			 * @return the size of the list of rightGoalLines.
			 */
			uint32_t getSize_rightGoalLines() const;
		
			/**
			 * @return true if the list of rightGoalLines is empty.
			 */
			bool isEmpty_rightGoalLines() const;
		
			/**
			 * This method adds an element to the end of the list of rightGoalLines.
			 *
			 * @param val Value to be added to the end of the list of rightGoalLines.
			 */
			void addTo_rightGoalLines(const CustomLine &val);
		
			/**
			 * This method adds an element to the beginning of the list of rightGoalLines.
			 *
			 * @param val Value to be inserted to the beginning of the list of rightGoalLines.
			 */
			void insertTo_rightGoalLines(const CustomLine &val);
		
			/**
			 * @return true if the list of rightGoalLines is contains the element val.
			 *              If the list has a complex data type, the entries are compared using their .toString() method.
			 */
			bool contains_rightGoalLines(const CustomLine &val) const;
		
			/**
			 * @return Pair of iterators for the begin and end of the list of rightGoalLines.
			 */
			std::pair<std::vector<CustomLine>::iterator, std::vector<CustomLine>::iterator> iteratorPair_rightGoalLines();
		public:
			/**
			 * @return currentLine.
			 */
			CustomLine getCurrentLine() const;
			
			/**
			 * This method sets currentLine.
			 *
			 * @param val Value for currentLine.
			 */
			void setCurrentLine(const CustomLine &val);
		public:
			/**
			 * @return noTrajectory.
			 */
			bool getNoTrajectory() const;
			
			/**
			 * This method sets noTrajectory.
			 *
			 * @param val Value for noTrajectory.
			 */
			void setNoTrajectory(const bool &val);
	
		public:
			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);
	
			virtual const string toString() const;
	
		
			std::vector<int32_t> listOfSwitchPointsLeftGoalLines;
		
			std::vector<int32_t> listOfSwitchPointsRightGoalLines;
		
			std::vector<CustomLine> leftGoalLines;
		
			std::vector<CustomLine> rightGoalLines;
		
			CustomLine currentLine;
		
			bool noTrajectory;
	};
} // msv

#endif /*MSV_LANEDETECTORDATATODRIVER_H*/
