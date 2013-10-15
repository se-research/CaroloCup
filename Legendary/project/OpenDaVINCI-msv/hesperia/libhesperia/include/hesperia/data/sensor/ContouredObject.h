/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SENSOR_CONTOUREDOBJECT_H_
#define HESPERIA_CORE_DATA_SENSOR_CONTOUREDOBJECT_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"
#include "hesperia/data/environment/PointShapedObject.h"

namespace hesperia {
    namespace data {
        namespace sensor {

            using namespace std;

            /**
             * This class contains data about a measured object
             * either by the radar or laser system.
             */
            class OPENDAVINCI_API ContouredObject : public hesperia::data::environment::PointShapedObject {
                public:
                    ContouredObject();

                    /**
                     * Constructor.
                     *
                     * @param position Position.
                     * @param rotation Rotation.
                     * @param velocity Velocity.
                     * @param acceleration Acceleration.
                     */
                    ContouredObject(const core::data::environment::Point3 &position, const core::data::environment::Point3 &rotation,
                                    const core::data::environment::Point3 &velocity, const core::data::environment::Point3 &acceleration);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ContouredObject(const ContouredObject &obj);

                    virtual ~ContouredObject();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ContouredObject& operator=(const ContouredObject &obj);

                    /**
                     * This method returns the current contour.
                     *
                     * @return Contour.
                     */
                    const vector<core::data::environment::Point3> getContour() const;

                    /**
                     * This method sets a contour.
                     *
                     * @param contour New contour.
                     */
                    void setContour(const vector<core::data::environment::Point3> &contour);

                    /**
                     * This method resets the current contoured object.
                     */
                    void reset();

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    vector<core::data::environment::Point3> m_contour;
            };

        }
    }
} // hesperia::data::sensor

#endif /*HESPERIA_CORE_DATA_SENSOR_CONTOUREDOBJECT_H_*/
