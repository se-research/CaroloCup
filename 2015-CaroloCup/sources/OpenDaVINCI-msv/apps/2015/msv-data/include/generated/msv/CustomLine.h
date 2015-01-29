/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_CUSTOMLINE_H
#define MSV_CUSTOMLINE_H

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"


#include "core/data/SerializableData.h"

#include "generated/msv/MyPoint.h"
#include "generated/msv/MyPoint.h"

namespace msv {
	using namespace std;
	
	class CustomLine : public core::data::SerializableData {
		public:
			CustomLine();
	
			virtual ~CustomLine();
	
			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			CustomLine(const CustomLine &obj);
	
			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			CustomLine& operator=(const CustomLine &obj);
	
		public:
			/**
			 * @return p1.
			 */
			MyPoint getP1() const;
			
			/**
			 * This method sets p1.
			 *
			 * @param val Value for p1.
			 */
			void setP1(const MyPoint &val);
		public:
			/**
			 * @return p2.
			 */
			MyPoint getP2() const;
			
			/**
			 * This method sets p2.
			 *
			 * @param val Value for p2.
			 */
			void setP2(const MyPoint &val);
		public:
			/**
			 * @return slope.
			 */
			float getSlope() const;
			
			/**
			 * This method sets slope.
			 *
			 * @param val Value for slope.
			 */
			void setSlope(const float &val);
		public:
			/**
			 * @return polygonIndex.
			 */
			int32_t getPolygonIndex() const;
			
			/**
			 * This method sets polygonIndex.
			 *
			 * @param val Value for polygonIndex.
			 */
			void setPolygonIndex(const int32_t &val);
	
		public:
			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);
	
			virtual const string toString() const;
	
		
			MyPoint p1;
		
			MyPoint p2;
		
			float slope;
		
			int32_t polygonIndex;
	};
} // msv

#endif /*MSV_CUSTOMLINE_H*/
