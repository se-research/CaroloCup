/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_ENVIRONMENT_POINTSHAPEDOBJECT_H_
#define HESPERIA_DATA_ENVIRONMENT_POINTSHAPEDOBJECT_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"
#include "core/data/environment/Position.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;

            using namespace core::data::environment;

            /**
             * This class contains all data about a moving point shaped object.
             */
            class OPENDAVINCI_API PointShapedObject : public Position {
                public:
                    PointShapedObject();

                    /**
                     * Constructor.
                     *
                     * @param position Position.
                     * @param rotation Rotation.
                     * @param velocity Velocity.
                     * @param acceleration Acceleration.
                     */
                    PointShapedObject(const Point3 &position, const Point3 &rotation,
                                      const Point3 &velocity, const Point3 &acceleration);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    PointShapedObject(const PointShapedObject &obj);

                    virtual ~PointShapedObject();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    PointShapedObject& operator=(const PointShapedObject &obj);

                    /**
                     * This method returns the current velocity.
                     *
                     * @return velocity.
                     */
                    const Point3 getVelocity() const;

                    /**
                     * This method sets a velocity.
                     *
                     * @param velocity New velocity.
                     */
                    void setVelocity(const Point3 &velocity);

                    /**
                     * This method returns the current acceleration.
                     *
                     * @return acceleration.
                     */
                    const Point3 getAcceleration() const;

                    /**
                     * This method sets an acceleration.
                     *
                     * @param acceleration New acceleration.
                     */
                    void setAcceleration(const Point3 &acceleration);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    Point3 m_velocity;
                    Point3 m_acceleration;
            };

        }
    }
} // hesperia::data::environment

#endif /*HESPERIA_DATA_ENVIRONMENT_POINTSHAPEDOBJECT_H_*/
