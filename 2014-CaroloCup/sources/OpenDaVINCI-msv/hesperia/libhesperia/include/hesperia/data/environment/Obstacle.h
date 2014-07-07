/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_ENVIRONMENT_OBSTACLE_H_
#define HESPERIA_DATA_ENVIRONMENT_OBSTACLE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/data/environment/PointShapedObject.h"
#include "hesperia/data/environment/Polygon.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;

            using namespace core::data::environment;

            /**
             * This class contains all data about an obstacle.
             */
            class OPENDAVINCI_API Obstacle : public PointShapedObject {
                public:
                    enum STATE {
                        UPDATE,
                        REMOVE
                    };

                    enum CLASSIFICATION {
                        UNKNOWN,
                        CAR,
                        TRUCK,
                        BICYCLE,
                        PEDESTRIAN
                    };

                public:
                    Obstacle();

                    /**
                     * Constructor.
                     *
                     * @param id Obstacle's id.
                     * @param state Obstacle's state.
                     */
                    Obstacle(const uint32_t &id, const enum STATE &state);

                    /**
                     * Constructor.
                     *
                     * @param id Obstacle's id.
                     * @param state Obstacle's state.
                     * @param position Position.
                     * @param rotation Rotation.
                     * @param velocity Velocity.
                     * @param acceleration Acceleration.
                     * @param polygon Polygon describing the shape.
                     */
                    Obstacle(const uint32_t &id, const enum STATE &state, const enum CLASSIFICATION &classification,
                             const Point3 &position, const Point3 &rotation,
                             const Point3 &velocity, const Point3 &acceleration,
                             const Polygon &polygon);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Obstacle(const Obstacle &obj);

                    virtual ~Obstacle();

                    /**
                     * This method returns the obstacle's ID.
                     *
                     * @return Obstacle's ID.
                     */
                    uint32_t getID() const;

                    /**
                     * This method sets the obstacle's ID.
                     *
                     * @param id Obstacle's ID.
                     */
                    void setID(const uint32_t &id);

                    /**
                     * This method returns the state.
                     *
                     * @return state.
                     */
                    enum STATE getState() const;

                    /**
                     * This method sets the state.
                     *
                     * @param s State.
                     */
                    void setState(const enum STATE &s);

                    /**
                     * This method returns the classification.
                     *
                     * @return c Classification.
                     */
                    enum CLASSIFICATION getClassification() const;

                    /**
                     * This method sets the classification.
                     *
                     * @param c Classification.
                     */
                    void setClassification(const enum CLASSIFICATION &c);

                    /**
                     * This method sets the polygon.
                     *
                     * @param p Polygon.
                     */
                    void setPolygon(const Polygon &p);

                    /**
                     * This method returns the polygon.
                     *
                     * @return Polygon.
                     */
                    const Polygon getPolygon() const;

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Obstacle& operator=(const Obstacle &obj);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    uint32_t m_id;
                    enum STATE m_state;
                    enum CLASSIFICATION m_classification;

                    Polygon m_polygon;
            };

        }
    }
} // hesperia::data::environment

#endif /*HESPERIA_DATA_ENVIRONMENT_OBSTACLE_H_*/
