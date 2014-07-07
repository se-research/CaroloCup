/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_ENVIRONMENT_OTHERVEHICLESTATE_H_
#define HESPERIA_DATA_ENVIRONMENT_OTHERVEHICLESTATE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/data/environment/PointShapedObject.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;

            using namespace core::data::environment;

            /**
             * This class contains all relevant data for the own car.
             */
            class OPENDAVINCI_API OtherVehicleState : public PointShapedObject {
                public:
                    OtherVehicleState();

                    /**
                     * Constructor.
                     *
                     * @param id Identifier.
                     * @param position Position.
                     * @param rotation Rotation.
                     * @param velocity Velocity.
                     * @param acceleration Acceleration.
                     */
                    OtherVehicleState(const uint32_t &id,
                    		 const Point3 &position, const Point3 &rotation,
                             const Point3 &velocity, const Point3 &acceleration);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    OtherVehicleState(const OtherVehicleState &obj);

                    virtual ~OtherVehicleState();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    OtherVehicleState& operator=(const OtherVehicleState &obj);

                    /**
                     * This method returns this object's ID.
                     *
                     * @return ID.
                     */
                    uint32_t getID() const;

                    /**
                     * This method sets this object's ID.
                     *
                     * @param id.
                     */
                    void setID(const uint32_t &id);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    uint32_t m_id;
            };

        }
    }
} // hesperia::data::environment

#endif /*HESPERIA_DATA_ENVIRONMENT_OTHERVEHICLESTATE_H_*/
