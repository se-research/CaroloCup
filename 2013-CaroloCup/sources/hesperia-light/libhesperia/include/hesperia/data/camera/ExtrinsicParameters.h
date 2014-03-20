/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_CAMERA_EXTRINSICPARAMETERS_H_
#define HESPERIA_DATA_CAMERA_EXTRINSICPARAMETERS_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"

#include "hesperia/data/environment/Matrix3x3.h"
#include "hesperia/data/environment/Point3.h"

namespace hesperia {
    namespace data {
        namespace camera {

            using namespace std;

            /**
             * This class represents extrinsic calibration parameters.
             */
            class HESPERIA_API ExtrinsicParameters : public core::data::SerializableData {
                public:
                    ExtrinsicParameters();

                    /**
                     * Constructor.
                     *
                     * @param translation Translation.
                     * @param rotation Rotation.
                     */
                    ExtrinsicParameters(const environment::Point3 &translation, const environment::Matrix3x3 &rotation);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ExtrinsicParameters(const ExtrinsicParameters &obj);

                    virtual ~ExtrinsicParameters();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ExtrinsicParameters& operator=(const ExtrinsicParameters &obj);

                    /**
                     * This method gets the translation.
                     *
                     * @return Translation.
                     */
                    const environment::Point3 getTranslation() const;

                    /**
                     * This method sets the translation.
                     *
                     * @param translation Translation.
                     */
                    void setTranslation(const environment::Point3 &translation);

                    /**
                     * This method gets the rotation.
                     *
                     * @return Rotation.
                     */
                    const environment::Matrix3x3 getRotation() const;

                    /**
                     * This method sets the rotation.
                     *
                     * @param rotation Rotation.
                     */
                    void setRotation(const environment::Matrix3x3 &rotation);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    environment::Point3 m_translation;
                    environment::Matrix3x3 m_rotation;
            };
        }
    }
} // core::data::camera

#endif /*HESPERIA_CORE_DATA_CAMERA_EXTRINSICPARAMETERS_H_*/
