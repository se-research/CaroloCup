/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_CAMERA_IMAGEGRABBERCALIBRATION_H_
#define HESPERIA_DATA_CAMERA_IMAGEGRABBERCALIBRATION_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"

#include "hesperia/data/camera/ExtrinsicParameters.h"
#include "hesperia/data/camera/IntrinsicParameters.h"

namespace hesperia {
    namespace data {
        namespace camera {

            using namespace std;

            /**
             * This class represents a calibration for an image grabber.
             */
            class OPENDAVINCI_API ImageGrabberCalibration : public core::data::SerializableData {
                public:
                    ImageGrabberCalibration();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ImageGrabberCalibration(const ImageGrabberCalibration &obj);

                    virtual ~ImageGrabberCalibration();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ImageGrabberCalibration& operator=(const ImageGrabberCalibration &obj);

                    /**
                     * This method returns the extrinsic calibration parameters.
                     *
                     * @return Extrinsic calibration parameters.
                     */
                    const ExtrinsicParameters getExtrinsicParameters() const;

                    /**
                     * This method sets the extrinsic calibration parameters.
                     *
                     * @param extrinsicParameters Extrinsic calibration parameters.
                     */
                    void setExtrinsicParameters(const ExtrinsicParameters &extrinsicParameters);

                    /**
                     * This method returns the intrinsic calibration parameters.
                     *
                     * @return Intrinsic calibration parameters.
                     */
                    const IntrinsicParameters getIntrinsicParameters() const;

                    /**
                     * This method sets the intrinsic calibration parameters.
                     *
                     * @param intrinsicParameters Intrinsic calibration parameters.
                     */
                    void setIntrinsicParameters(const IntrinsicParameters &intrinsicParameters);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    ExtrinsicParameters m_extrinsicParameters;
                    IntrinsicParameters m_intrinsicParameters;
            };
        }
    }
} // hesperia::data::camera

#endif /*HESPERIA_DATA_CAMERA_IMAGEGRABBERCALIBRATION_H_*/
