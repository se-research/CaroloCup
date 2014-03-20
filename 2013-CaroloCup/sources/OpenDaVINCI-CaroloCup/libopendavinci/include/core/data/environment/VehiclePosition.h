/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DATA_ENVIRONMENT_VEHICLEPOSITION_H_
#define OPENDAVINCI_DATA_ENVIRONMENT_VEHICLEPOSITION_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/environment/Position.h"

namespace core {
    namespace data {
        namespace environment {

            using namespace std;

            /**
             * This class can be used to describe the position from the vehicle.
             */
            class OPENDAVINCI_API VehiclePosition : public Position {
                public:
                    VehiclePosition();

                    /**
                     * Constructor.
                     *
                     * 12am = Y-Axis, +pi/2 rad (counterclockwisely)
                     * 3pm = X-Axis, 0 rad (counterclockwisely)
                     *
                     * @param position Position.
                     * @param rotation Rotation.
                     */
                    VehiclePosition(const double &x, const double &y, const double &heading);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    VehiclePosition(const VehiclePosition &obj);

                    virtual ~VehiclePosition();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    VehiclePosition& operator=(const VehiclePosition &obj);

                    /**
                     * This method returns the traveled path in m.
                     *
                     * @return Traveled path in m.
                     */
                    double getTravelPath() const;

                    /**
                     * This method sets the traveled path.
                     *
                     * @param tp Traveled path.
                     */
                    void setTravelPath(const double &tp);

                    /**
                     * @return x.
                     */
                    double getX() const;

                    /**
                     * This method sets x.
                     *
                     * @param x.
                     */
                    void setX(const double &x);

                    /**
                     * @return y.
                     */
                    double getY() const;

                    /**
                     * This method sets y.
                     *
                     * @param y.
                     */
                    void setY(const double &y);

                    /**
                     * @return heading.
                     */
                    double getHeading() const;

                    /**
                     * This method sets heading.
                     *
                     * @param heading.
                     */
                    void setHeading(const double &heading);

                    /**
                     * @return traveled path.
                     */
                    double getTraveledPath() const;

                    /**
                     * This method sets the traveled path.
                     *
                     * @param tp.
                     */
                    void setTraveledPath(const double &tp);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    double m_x;
                    double m_y;
                    double m_heading;
                    double m_traveledPath;
            };

        }
    }
} // core::data::environment

#endif /*OPENDAVINCI_DATA_ENVIRONMENT_VEHICLEPOSITION_H_*/
