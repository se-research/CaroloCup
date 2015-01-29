/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#ifndef MSV_STM32F4CONTROL_H
#define MSV_STM32F4CONTROL_H

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"


#include "core/data/SerializableData.h"


namespace msv {
	using namespace std;
	
	class STM32F4Control : public core::data::SerializableData {
		public:
			STM32F4Control();
	
			virtual ~STM32F4Control();
	
			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			STM32F4Control(const STM32F4Control &obj);
	
			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			STM32F4Control& operator=(const STM32F4Control &obj);
	
		public:
			/**
			 * @return dataFeed.
			 */
			uint32_t getDataFeed() const;
			
			/**
			 * This method sets dataFeed.
			 *
			 * @param val Value for dataFeed.
			 */
			void setDataFeed(const uint32_t &val);
	
		public:
			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);
	
			virtual const string toString() const;
	
		private:
			uint32_t m_dataFeed;
	};
} // msv

#endif /*MSV_STM32F4CONTROL_H*/
