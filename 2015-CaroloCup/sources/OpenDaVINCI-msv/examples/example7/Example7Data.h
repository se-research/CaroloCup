/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef EXAMPLE3DATA_H_
#define EXAMPLE3DATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

namespace examples {

	using namespace std;

	class Example7Data : public core::data::SerializableData {
		public:
			Example7Data();

			virtual ~Example7Data();

			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			Example7Data(const Example7Data &obj);

			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			Example7Data& operator=(const Example7Data &obj);

			/**
			 * This method returns the numerical value.
			 *
			 * @return Numerical value.
			 */
			uint32_t getNumericalValue() const;

			/**
			 * This method sets the numerical value.
			 *
			 * @param nv Numerical value.
			 */
			void setNumericalValue(const uint32_t &nv);

			/**
			 * This method returns the string value.
			 *
			 * @return String value.
			 */
			const string getStringValue() const;

			/**
			 * This method sets the string value.
			 *
			 * @param sv String value.
			 */
			void setStringValue(const string &sv);

			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);

			virtual const string toString() const;

		private:
			uint32_t m_numericalValue;
			string m_stringValue;
	};

} // examples

#endif /*EXAMPLE3DATA_H_*/
