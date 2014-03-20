/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DATA_ENVIRONMENT_POSITION_H_
#define OPENDAVINCI_DATA_ENVIRONMENT_POSITION_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

#include "core/data/environment/Point3.h"

namespace core {
    namespace data {
        namespace environment {

            using namespace std;

            /**
             * This class can be used to describe an object in a three dimensional
             * space using position and rotation.
             */
            class OPENDAVINCI_API Position : public core::data::SerializableData {
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
} // core::data::environment

#endif /*OPENDAVINCI_DATA_ENVIRONMENT_POSITION_H_*/
