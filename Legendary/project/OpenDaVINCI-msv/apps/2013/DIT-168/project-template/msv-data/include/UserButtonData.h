/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef USERBUTTONDATA_H_
#define USERBUTTONDATA_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

namespace msv {

	using namespace std;

	class UserButtonData : public core::data::SerializableData {
        public:
            enum BUTTONSTATUS {
                UNAVAILABLE = -1,
                RELEASED = 0,
                PRESSED = 1
            };

		public:
			UserButtonData();

			virtual ~UserButtonData();

			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			UserButtonData(const UserButtonData &obj);

			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			UserButtonData& operator=(const UserButtonData &obj);

			/**
			 * This method returns the current user button status.
			 *
			 * @return Button status.
			 */
			BUTTONSTATUS getButtonStatus() const;

			/**
			 * This method sets the current user button status.
			 *
			 * @param b Button status.
			 */
			void setButtonStatus(const BUTTONSTATUS &b);

            /**
             * This method returns the duration in seconds that the button was pressed.
             *
             * @return Duration the user button was pressed in s.
             */
            double getDuration() const;

            /**
             * This method sets the duration in seconds that the button was pressed.
             *
             * @param d Duration the user button was pressed in s.
             */
            void setDuration(const double &d);

			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);

			virtual const string toString() const;

		private:
            BUTTONSTATUS m_buttonStatus;
            double m_duration;
	};

} // msv

#endif /*USERBUTTONDATA_H_*/
