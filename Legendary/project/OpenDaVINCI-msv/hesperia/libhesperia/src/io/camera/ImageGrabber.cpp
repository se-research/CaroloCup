/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/io/camera/ImageGrabber.h"

namespace hesperia {
    namespace io {
        namespace camera {

            using namespace data::camera;

            ImageGrabber::ImageGrabber(const ImageGrabberID &imageGrabberID, const ImageGrabberCalibration &imageGrabberCalibration) :
                    m_imageGrabberID(imageGrabberID),
                    m_imageGrabberCalibration(imageGrabberCalibration) {}

            ImageGrabber::~ImageGrabber() {}

            const ImageGrabberID ImageGrabber::getImageGrabberID() const {
                return m_imageGrabberID;
            }

            const ImageGrabberCalibration ImageGrabber::getImageGrabberCalibration() const {
                return m_imageGrabberCalibration;
            }

        }
    }
} // hesperia::io::camera
