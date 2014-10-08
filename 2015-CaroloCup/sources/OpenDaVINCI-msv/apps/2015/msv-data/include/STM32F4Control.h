/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef STM32F4CONTROL_H_
#define STM32F4CONTROL_H_

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

            /**
             * This method returns the desired data feed STM32F4 board.
             *
             * @return Desired STM32F4 board data feed.
             */
            uint32_t getDataFeed() const;

            /**
             * This method sets the raw data.
             *
             * @param f Desired data feed.
             */
            void setDataFeed(const uint32_t &f);

			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);

			virtual const string toString() const;

		private:
            uint32_t m_dataFeed;
	};

} // msv

#endif /*STM32F4CONTROL_H_*/
