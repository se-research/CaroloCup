/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef STEERINGDATA_H_
#define STEERINGDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

namespace msv {

	using namespace std;

    /**
     * This is an example how you can send data from one component to another.
     */
	class SteeringData : public core::data::SerializableData {
		public:
			SteeringData();

			virtual ~SteeringData();

			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			SteeringData(const SteeringData &obj);

			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			SteeringData& operator=(const SteeringData &obj);

            /**
             * This method returns the example data.
             *
             * @return example data.
             */
            double getExampleData() const;

            /**
             * This method sets the example data.
             *
             * @param e Example data.
             */
            void setExampleData(const double &e);

			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);

			virtual const string toString() const;

		private:
            double m_exampleData;
	};

} // msv

#endif /*STEERINGDATA_H_*/
