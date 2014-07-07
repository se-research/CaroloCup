/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_DATA_RUNTIMESTATISTIC_H_
#define OPENDAVINCI_CORE_DATA_RUNTIMESTATISTIC_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

namespace core {
    namespace data {

        using namespace std;

        /**
         * This class can be used for gathering information about runtime.
         */
        class OPENDAVINCI_API RuntimeStatistic : public SerializableData {
            public:
                RuntimeStatistic();

                virtual ~RuntimeStatistic();

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                RuntimeStatistic(const RuntimeStatistic &obj);

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                RuntimeStatistic& operator=(const RuntimeStatistic &obj);

                /**
                 * This method returns the slice consumption in per cent.
                 *
                 * @return Slice consumption.
                 */
                float getSliceConsumption() const;

                /**
                 * This method sets the slice consumption.
                 *
                 * @param sc Slice consumption.
                 */
                void setSliceConsumption(const float &sc);

                virtual ostream& operator<<(ostream &out) const;
                virtual istream& operator>>(istream &in);

                virtual const string toString() const;

            private:
                float m_sliceConsumption;
        };

    }
} // core::data

#endif /*OPENDAVINCI_CORE_DATA_RUNTIMESTATISTIC_H_*/
