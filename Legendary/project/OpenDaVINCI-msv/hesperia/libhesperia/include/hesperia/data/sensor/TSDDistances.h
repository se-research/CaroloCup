/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SENSOR_TSDDISTANCES_H_
#define HESPERIA_CORE_DATA_SENSOR_TSDDISTANCES_H_

#include <map>

#include "core/platform.h"
#include "core/data/SerializableData.h"

namespace hesperia {
    namespace data {
        namespace sensor {

            using namespace std;

            /**
             * This class contains data about measured distances from a 360 degree single layer lidar.
             */
            class OPENDAVINCI_API TSDDistances : public core::data::SerializableData {
                public:
                    TSDDistances();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    TSDDistances(const TSDDistances &obj);

                    virtual ~TSDDistances();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    TSDDistances& operator=(const TSDDistances &obj);

                    /**
                     * This method returns the current distance map.
                     *
                     * @return Contour.
                     */
                    const map<uint32_t, double> getDistanceMap() const;

                    /**
                     * This method sets a distance map.
                     *
                     * @param contour New distance map.
                     */
                    void setDistanceMap(const map<uint32_t, double> &distanceMap);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    map<uint32_t, double> m_distanceMap;
            };

        }
    }
} // hesperia::data::sensor

#endif /*HESPERIA_CORE_DATA_SENSOR_TSDDISTANCES_H_*/
