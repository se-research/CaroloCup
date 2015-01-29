/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_STM32F4DATA_H
#define MSV_STM32F4DATA_H

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"


#include "core/data/SerializableData.h"


namespace msv {
	using namespace std;
	
	class STM32F4Data : public core::data::SerializableData {
		public:
			STM32F4Data();
	
			virtual ~STM32F4Data();
	
			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			STM32F4Data(const STM32F4Data &obj);
	
			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			STM32F4Data& operator=(const STM32F4Data &obj);
	
		public:
			/**
			 * @return rawData.
			 */
			std::string getRawData() const;
			
			/**
			 * This method sets rawData.
			 *
			 * @param val Value for rawData.
			 */
			void setRawData(const std::string &val);
	
		public:
			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);
	
			virtual const string toString() const;
	
		private:
			std::string m_rawData;
	};
} // msv

#endif /*MSV_STM32F4DATA_H*/
