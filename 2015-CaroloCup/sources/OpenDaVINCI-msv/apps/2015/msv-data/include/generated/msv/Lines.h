/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_LINES_H
#define MSV_LINES_H

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"


#include "core/data/SerializableData.h"

#include "generated/msv/MyVec4i.h"
#include "generated/msv/MyVec4i.h"
#include "generated/msv/MyVec4i.h"
#include "generated/msv/CustomLine.h"
#include "generated/msv/CustomLine.h"
#include "generated/msv/CustomLine.h"

namespace msv {
	using namespace std;
	
	class Lines : public core::data::SerializableData {
		public:
			enum RoadState {
				NORMAL = 1,
				INTERSECTION = 2,
				NOT_SET = 0,
			};
		public:
			Lines();
	
			virtual ~Lines();
	
			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			Lines(const Lines &obj);
	
			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			Lines& operator=(const Lines &obj);
	
		public:
		public:
			/**
			 * @return leftLine.
			 */
			MyVec4i getLeftLine() const;
			
			/**
			 * This method sets leftLine.
			 *
			 * @param val Value for leftLine.
			 */
			void setLeftLine(const MyVec4i &val);
		public:
			/**
			 * @return rightLine.
			 */
			MyVec4i getRightLine() const;
			
			/**
			 * This method sets rightLine.
			 *
			 * @param val Value for rightLine.
			 */
			void setRightLine(const MyVec4i &val);
		public:
			/**
			 * @return dashedLine.
			 */
			MyVec4i getDashedLine() const;
			
			/**
			 * This method sets dashedLine.
			 *
			 * @param val Value for dashedLine.
			 */
			void setDashedLine(const MyVec4i &val);
		public:
			/**
			 * @return goalLine.
			 */
			CustomLine getGoalLine() const;
			
			/**
			 * This method sets goalLine.
			 *
			 * @param val Value for goalLine.
			 */
			void setGoalLine(const CustomLine &val);
		public:
			/**
			 * @return goalLineLeft.
			 */
			CustomLine getGoalLineLeft() const;
			
			/**
			 * This method sets goalLineLeft.
			 *
			 * @param val Value for goalLineLeft.
			 */
			void setGoalLineLeft(const CustomLine &val);
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
			 * @return pGain.
			 */
			int32_t getPGain() const;
			
			/**
			 * This method sets pGain.
			 *
			 * @param val Value for pGain.
			 */
			void setPGain(const int32_t &val);
		public:
			/**
			 * @return intGain.
			 */
			int32_t getIntGain() const;
			
			/**
			 * This method sets intGain.
			 *
			 * @param val Value for intGain.
			 */
			void setIntGain(const int32_t &val);
		public:
			/**
			 * @return derGain.
			 */
			int32_t getDerGain() const;
			
			/**
			 * This method sets derGain.
			 *
			 * @param val Value for derGain.
			 */
			void setDerGain(const int32_t &val);
		public:
			/**
			 * @return speed.
			 */
			int32_t getSpeed() const;
			
			/**
			 * This method sets speed.
			 *
			 * @param val Value for speed.
			 */
			void setSpeed(const int32_t &val);
		public:
			/**
			 * @return width.
			 */
			int32_t getWidth() const;
			
			/**
			 * This method sets width.
			 *
			 * @param val Value for width.
			 */
			void setWidth(const int32_t &val);
		public:
			/**
			 * @return height.
			 */
			int32_t getHeight() const;
			
			/**
			 * This method sets height.
			 *
			 * @param val Value for height.
			 */
			void setHeight(const int32_t &val);
		public:
			/**
			 * @return startLineHeight.
			 */
			int32_t getStartLineHeight() const;
			
			/**
			 * This method sets startLineHeight.
			 *
			 * @param val Value for startLineHeight.
			 */
			void setStartLineHeight(const int32_t &val);
		public:
			/**
			 * @return stopLineHeight.
			 */
			int32_t getStopLineHeight() const;
			
			/**
			 * This method sets stopLineHeight.
			 *
			 * @param val Value for stopLineHeight.
			 */
			void setStopLineHeight(const int32_t &val);
		public:
			/**
			 * @return roadState.
			 */
			RoadState getRoadState() const;
			
			/**
			 * This method sets roadState.
			 *
			 * @param val Value for roadState.
			 */
			void setRoadState(const RoadState &val);
		public:
			/**
			 * @return confidenceLevel.
			 */
			int32_t getConfidenceLevel() const;
			
			/**
			 * This method sets confidenceLevel.
			 *
			 * @param val Value for confidenceLevel.
			 */
			void setConfidenceLevel(const int32_t &val);
	
		public:
			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);
	
			virtual const string toString() const;
	
		
		
			MyVec4i leftLine;
		
			MyVec4i rightLine;
		
			MyVec4i dashedLine;
		
			CustomLine goalLine;
		
			CustomLine goalLineLeft;
		
			CustomLine currentLine;
		
			int32_t pGain;
		
			int32_t intGain;
		
			int32_t derGain;
		
			int32_t speed;
		
			int32_t width;
		
			int32_t height;
		
			int32_t startLineHeight;
		
			int32_t stopLineHeight;
		
			RoadState roadState;
		
			int32_t confidenceLevel;
	};
} // msv

#endif /*MSV_LINES_H*/
