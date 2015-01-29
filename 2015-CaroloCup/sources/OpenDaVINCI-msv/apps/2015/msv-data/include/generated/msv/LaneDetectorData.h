/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_LANEDETECTORDATA_H
#define MSV_LANEDETECTORDATA_H

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"


#include "core/data/SerializableData.h"

#include "generated/msv/Lines.h"
#include "generated/msv/LaneDetectorDataToDriver.h"

namespace msv {
	using namespace std;
	
	class LaneDetectorData : public core::data::SerializableData {
		public:
			LaneDetectorData();
	
			virtual ~LaneDetectorData();
	
			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			LaneDetectorData(const LaneDetectorData &obj);
	
			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			LaneDetectorData& operator=(const LaneDetectorData &obj);
	
		public:
			/**
			 * @return m_frame_count.
			 */
			uint32_t getFrameCount() const;
			
			/**
			 * This method sets m_frame_count.
			 *
			 * @param val Value for m_frame_count.
			 */
			void setFrameCount(const uint32_t &val);
		public:
			/**
			 * @return m_lines.
			 */
			Lines getLaneDetectorData() const;
			
			/**
			 * This method sets m_lines.
			 *
			 * @param val Value for m_lines.
			 */
			void setM_lines(const Lines &val);
		public:
			/**
			 * @return m_classification.
			 */
			std::string getClassification() const;
			
			/**
			 * This method sets m_classification.
			 *
			 * @param val Value for m_classification.
			 */
			void setClassification(const std::string &val);
		public:
			/**
			 * @return m_dataToDriver.
			 */
			LaneDetectorDataToDriver getLaneDetectorDataDriver() const;
			
			/**
			 * This method sets m_dataToDriver.
			 *
			 * @param val Value for m_dataToDriver.
			 */
			void setM_dataToDriver(const LaneDetectorDataToDriver &val);
	
		public:
			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);
	
			virtual const string toString() const;
	
		
			uint32_t m_frame_count;
		
			Lines m_lines;
		
			std::string m_classification;
		
			LaneDetectorDataToDriver m_dataToDriver;
	};
} // msv

#endif /*MSV_LANEDETECTORDATA_H*/
