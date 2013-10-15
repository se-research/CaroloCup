/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_DATA_CONTROL_FORCECONTROL_H_
#define OPENDAVINCI_CORE_DATA_CONTROL_FORCECONTROL_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"

namespace core {
    namespace data {
        namespace control {

            /**
             * This class represents forces to be applied to a controller.
             */
            class OPENDAVINCI_API ForceControl : public core::data::SerializableData {
                public:
                    ForceControl();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ForceControl(const ForceControl &obj);

                    virtual ~ForceControl();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ForceControl& operator=(const ForceControl &obj);

                    /**
                     * This method returns the acceleration force.
                     *
                     * @return Acceleration force.
                     */
                    double getAccelerationForce() const;

                    /**
                     * This method sets the acceleration force.
                     *
                     * @param f Acceleration force.
                     */
                    void setAccelerationForce(const double &f);

                    /**
                     * This method returns the brake force.
                     *
                     * @return Brake force.
                     */
                    double getBrakeForce() const;

                    /**
                     * This method sets the brake force.
                     *
                     * @param f Brake force.
                     */
                    void setBrakeForce(const double &f);

                    /**
                     * This method returns the steering force.
                     *
                     * @return Steering force.
                     */
                    double getSteeringForce() const;

                    /**
                     * This method sets the steering force.
                     *
                     * @param f Steering force.
                     */
                    void setSteeringForce(const double &f);

                    /**
                     * This method returns the status of the brake lights.
                     *
                     * @return true if the brake lights are on.
                     */
                    bool getBrakeLights() const;

                    /**
                     * This method turns the brake lights on/off.
                     *
                     * @param on Set to true to turn on the brake lights.
                     */
                    void setBrakeLights(const bool &on);

                    /**
                     * This method returns the status of the left flashing lights.
                     *
                     * @return true if the left flashing lights are on.
                     */
                    bool getLeftFlashingLights() const;

                    /**
                     * This method turns the left flashing lights on/off.
                     *
                     * @param on Set to true to turn on the left flashing lights.
                     */
                    void setLeftFlashingLights(const bool &on);

                    /**
                     * This method returns the status of the right flashing lights.
                     *
                     * @return true if the right flashing lights are on.
                     */
                    bool getRightFlashingLights() const;

                    /**
                     * This method turns the right flashing lights on/off.
                     *
                     * @param on Set to true to turn on the right flashing lights.
                     */
                    void setRightFlashingLights(const bool &on);

                    virtual std::ostream& operator<<(std::ostream &out) const;
                    virtual std::istream& operator>>(std::istream &in);

                    virtual const std::string toString() const;

                private:
                    double m_accelerationForce;
                    double m_brakeForce;
                    double m_steeringForce;
                    bool m_brakeLights;
                    bool m_flashingLightsLeft;
                    bool m_flashingLightsRight;
            };
        }
    }
} // core::data::control

#endif /*OPENDAVINCI_CORE_DATA_CONTROL_FORCECONTROL_H_*/
