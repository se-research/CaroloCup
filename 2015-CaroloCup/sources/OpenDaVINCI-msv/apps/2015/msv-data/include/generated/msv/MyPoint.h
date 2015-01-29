/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_MYPOINT_H
#define MSV_MYPOINT_H

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"


#include "core/data/SerializableData.h"


namespace msv {
	using namespace std;
	
	class MyPoint : public core::data::SerializableData {
		public:
			MyPoint();
	
			virtual ~MyPoint();
	
			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			MyPoint(const MyPoint &obj);
	
			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			MyPoint& operator=(const MyPoint &obj);
	
		public:
			/**
			 * @return x.
			 */
			double getX() const;
			
			/**
			 * This method sets x.
			 *
			 * @param val Value for x.
			 */
			void setX(const double &val);
		public:
			/**
			 * @return y.
			 */
			double getY() const;
			
			/**
			 * This method sets y.
			 *
			 * @param val Value for y.
			 */
			void setY(const double &val);
	
		public:
			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);
	
			virtual const string toString() const;
	
		
			double x;
		
			double y;
	};
} // msv

#endif /*MSV_MYPOINT_H*/
