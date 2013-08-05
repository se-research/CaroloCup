/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_CAMERA_INTRINSICPARAMETERS_H_
#define HESPERIA_DATA_CAMERA_INTRINSICPARAMETERS_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "core/data/environment/Point3.h"

namespace hesperia {
    namespace data {
        namespace camera {

            using namespace core;

            /* Parameter:

            float distortion[4];
            float alpha; (done)
            float focalLength[2]; (done)
            float principlePoint[2]; (done)
            float focalDistance; (done)
             */

            /**
             * This class represents intrinsic calibration parameters.
             */
            class OPENDAVINCI_API IntrinsicParameters : public core::data::SerializableData {
                public:
                    IntrinsicParameters();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    IntrinsicParameters(const IntrinsicParameters &obj);

                    virtual ~IntrinsicParameters();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    IntrinsicParameters& operator=(const IntrinsicParameters &obj);

                    /**
                     * This method returns the principle point, i.e. the
                     * two-dimensional point where the optic axis intersects
                     * the image.
                     *
                     * @return Principle point.
                     */
                    const core::data::environment::Point3 getPrinciplePoint() const;

                    /**
                     * This method sets the principle point. The z-component
                     * is set to 0 automatically.
                     *
                     * @param principlePoint Two dimensional principle point.
                     */
                    void setPrinciplePoint(const core::data::environment::Point3 &principlePoint);

                    /**
                     * This method returns the focal length in x- and y-
                     * direction.
                     *
                     * @return Focal length.
                     */
                    const core::data::environment::Point3 getFocalLength() const;

                    /**
                     * This method sets the focal length. The z-component
                     * is set to 0 automatically.
                     *
                     * @param focalLength Two-dimensional focal length.
                     */
                    void setFocalLength(const core::data::environment::Point3 &focalLength);

                    /**
                     * This method returns the focal distance.
                     *
                     * @return Focal distance.
                     */
                    double getFocalDistance() const;

                    /**
                     * This method sets the focal distance.
                     *
                     * @param focalDistance Focal distance.
                     */
                    void setFocalDistance(const double &focalDistance);

                    /**
                     * This method returns alpha (aspect ratio?) TODO: Read OpenCV doc.
                     *
                     * @return Alpha.
                     */
                    double getAlpha() const;

                    /**
                     * This method sets alpha.
                     *
                     * @param alpha
                     */
                    void setAlpha(const double &alpha);

                    virtual std::ostream& operator<<(std::ostream &out) const;
                    virtual std::istream& operator>>(std::istream &in);

                    virtual const std::string toString() const;

                private:
                    core::data::environment::Point3 m_principlePoint;
                    core::data::environment::Point3 m_focalLength;
                    double m_focalDistance;
                    double m_alpha;
            };
        }
    }
} // hesperia::data::camera

#endif /*HESPERIA_DATA_CAMERA_INTRINSICPARAMETERS_H_*/
