/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DATA_ENVIRONMENT_VEHICLEDATA_H_
#define OPENDAVINCI_DATA_ENVIRONMENT_VEHICLEDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

#include "core/data/environment/Point3.h"

namespace core {
    namespace data {
        namespace environment {

            using namespace std;

            /**
             * This class contains all relevant data for the own car.
             */
            class OPENDAVINCI_API VehicleData : public core::data::SerializableData {
                public:
                    VehicleData();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    VehicleData(const VehicleData &obj);

                    virtual ~VehicleData();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    VehicleData& operator=(const VehicleData &obj);

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
                     * This method returns the current speed.
                     *
                     * @return speed.
                     */
                    double getSpeed() const;

                    /**
                     * This method sets a speed.
                     *
                     * @param speed New speed.
                     */
                    void setSpeed(const double &speed);

                    /**
                     * This method returns v_log.
                     *
                     * @return v_log.
                     */
                    double getV_log() const;

                    /**
                     * This method sets a v_log.
                     *
                     * @param speed New v_log.
                     */
                    void setV_log(const double &v_log);

                    /**
                     * This method returns v_batt.
                     *
                     * @return v_batt.
                     */
                    double getV_batt() const;

                    /**
                     * This method sets a v_batt.
                     *
                     * @param speed New v_batt.
                     */
                    void setV_batt(const double &v_batt);

                    /**
                     * This method returns the current temp.
                     *
                     * @return temp.
                     */
                    double getTemp() const;

                    /**
                     * This method sets a temp.
                     *
                     * @param temp New temp.
                     */
                    void setTemp(const double &temp);

                    /**
                     * @return true if we run in simulation mode.
                     */
                    bool isSimulation() const;

                    /**
                     * This method sets simulation mode.
                     *
                     * @param s true if we run in simulation mode.
                     */
                    void setSimulation(const bool &s);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    Point3 m_velocity;
                    double m_speed;
                    double m_v_log;
                    double m_v_batt;
                    double m_temp;
                    bool m_isSimulation;
            };

        }
    }
} // core::data::environment

#endif /*OPENDAVINCI_DATA_ENVIRONMENT_VEHICLEDATA_H_*/
