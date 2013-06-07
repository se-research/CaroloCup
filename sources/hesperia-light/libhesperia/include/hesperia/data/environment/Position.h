/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_ENVIRONMENT_POSITION_H_
#define HESPERIA_DATA_ENVIRONMENT_POSITION_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"

#include "hesperia/data/environment/Point3.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;

            /**
             * This class can be used to describe an object in a three dimensional
             * space using position and rotation.
             */
            class HESPERIA_API Position : public core::data::SerializableData {
                public:
                    Position();

                    /**
                     * Constructor.
                     *
                     * @param position Position.
                     * @param rotation Rotation.
                     */
                    Position(const Point3 &position, const Point3 &rotation);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Position(const Position &obj);

                    virtual ~Position();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Position& operator=(const Position &obj);

                    /**
                     * This method returns the translation.
                     *
                     * @return Translation.
                     */
                    const Point3 getPosition() const;

                    /**
                     * This method sets the position.
                     *
                     * @param position Position.
                     */
                    void setPosition(const Point3 &position);

                    /**
                     * This method returns the rotation.
                     *
                     * @return Rotation.
                     */
                    const Point3 getRotation() const;

                    /**
                     * This method sets the rotation.
                     *
                     * @param rotation Rotation.
                     */
                    void setRotation(const Point3 &rotation);

                    bool operator==(const Position& other) const;
                    bool operator!=(const Position& other) const;

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    Point3 m_position;
                    Point3 m_rotation;
            };

        }
    }
} // hesperia::data::environment

#endif /*HESPERIA_DATA_ENVIRONMENT_POSITION_H_*/
