/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef STM32F4DATA_H_
#define STM32F4DATA_H_

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

            /**
             * This method returns the received raw string from the STM32F4 board.
             *
             * @return STM32F4 board raw string.
             */
            string getRawData() const;

            /**
             * This method sets the raw data.
             *
             * @param s Raw data.
             */
            void setRawData(const string &s);


			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);

			virtual const string toString() const;

		private:
            string m_rawData;
	};

} // msv

#endif /*STM32F4DATA_H_*/
