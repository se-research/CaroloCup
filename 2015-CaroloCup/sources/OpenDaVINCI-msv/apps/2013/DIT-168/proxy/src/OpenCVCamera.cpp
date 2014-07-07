/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/highgui/highgui.hpp"

#include "OpenCVCamera.h"

namespace msv {

    OpenCVCamera::OpenCVCamera(const string &name, const uint32_t &id, const uint32_t &width, const uint32_t &height, const uint32_t &bpp) :
        Camera(name, id, width, height, bpp),
        m_capture(NULL),
        m_image(NULL) {

        m_capture = cvCaptureFromCAM(id);
        if (m_capture) {
            cvSetCaptureProperty(m_capture, CV_CAP_PROP_FRAME_WIDTH, width);
            cvSetCaptureProperty(m_capture, CV_CAP_PROP_FRAME_HEIGHT, height);
        }
        else {
            cerr << "proxy: Could not open camera '" << name << "' with ID: " << id << endl;
        }
    }

    OpenCVCamera::~OpenCVCamera() {
        if (m_capture) {
            cvReleaseCapture(&m_capture);
            m_capture = NULL;
        }
    }

    bool OpenCVCamera::isValid() const {
        return (m_capture != NULL);
    }

    bool OpenCVCamera::captureFrame() {
        bool retVal = false;
        if (m_capture != NULL) {
            if (cvGrabFrame(m_capture)) {
                m_image = cvRetrieveFrame(m_capture);
                retVal = true;
            }
        }
        return retVal;
    }

    bool OpenCVCamera::copyImageTo(char *dest, const uint32_t &size) {
        bool retVal = false;

        if ( (dest != NULL) && (size > 0) && (m_image != NULL) ) {
            ::memcpy(dest, m_image->imageData, size);

            cvShowImage("WindowShowImage", m_image);
            cvWaitKey(10);

            retVal = true;
        }

        return retVal;
    }

} // msv

