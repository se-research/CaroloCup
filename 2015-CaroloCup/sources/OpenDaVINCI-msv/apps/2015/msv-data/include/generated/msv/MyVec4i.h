/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_MYVEC4I_H
#define MSV_MYVEC4I_H

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"


#include "core/data/SerializableData.h"


namespace msv {
	using namespace std;
	
	class MyVec4i : public core::data::SerializableData {
		public:
			MyVec4i();
	
			virtual ~MyVec4i();
	
			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			MyVec4i(const MyVec4i &obj);
	
			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			MyVec4i& operator=(const MyVec4i &obj);
	
		public:
			/**
			 * @return x.
			 */
			int32_t getX() const;
			
			/**
			 * This method sets x.
			 *
			 * @param val Value for x.
			 */
			void setX(const int32_t &val);
		public:
			/**
			 * @return y.
			 */
			int32_t getY() const;
			
			/**
			 * This method sets y.
			 *
			 * @param val Value for y.
			 */
			void setY(const int32_t &val);
		public:
			/**
			 * @return z.
			 */
			int32_t getZ() const;
			
			/**
			 * This method sets z.
			 *
			 * @param val Value for z.
			 */
			void setZ(const int32_t &val);
		public:
			/**
			 * @return w.
			 */
			int32_t getW() const;
			
			/**
			 * This method sets w.
			 *
			 * @param val Value for w.
			 */
			void setW(const int32_t &val);
	
		public:
			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);
			virtual int32_t operator[](int i) const;
			virtual const string toString() const;
	
		
			int32_t x;
		
			int32_t y;
		
			int32_t z;

			int32_t w;
	};
} // msv

#endif /*MSV_MYVEC4I_H*/
