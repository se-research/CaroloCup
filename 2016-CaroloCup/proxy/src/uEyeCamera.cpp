/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef HAVE_UEYE

#include <iostream>

#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/highgui/highgui.hpp"

#include "uEyeCamera.h"

namespace msv {

    uEyeCamera::uEyeCamera(const string &name, const uint32_t &id, const uint32_t &width, const uint32_t &height, const uint32_t &bpp) :
        Camera(name, id, width, height, bpp),
        m_capture(0),
        m_imageMemory(NULL),
        m_ueyeImagePtr(NULL),
        m_pid(0),
        m_image(NULL) {

        int retVal = is_InitCamera(&m_capture, NULL);

        is_AllocImageMem(m_capture, width, height, bpp, &m_imageMemory, &m_pid);
        is_SetImageMem(m_capture, m_imageMemory, m_pid);
        is_SetDisplayMode(m_capture, IS_SET_DM_DIB);
        is_SetColorMode(m_capture, IS_CM_MONO8);

        int pnCol, pnColMode;
        is_GetColorDepth(m_capture, &pnCol , &pnColMode);

        is_CaptureVideo(m_capture, IS_WAIT);

        if (retVal != IS_SUCCESS) {
            if (retVal == IS_STARTER_FW_UPLOAD_NEEDED) {
                m_capture = m_capture | IS_ALLOW_STARTER_FW_UPLOAD;
                retVal = is_InitCamera(&m_capture, NULL);
            }
            cerr << "proxy: Could not open camera '" << name << "' with ID: " << id << ", returned value: " << retVal << endl;

            m_capture = 0;
        }
        else {
            m_image = cvCreateImageHeader(cvSize(m_width, m_height), IPL_DEPTH_8U, m_bpp);
        }
    }

    uEyeCamera::~uEyeCamera() {
        // Free the allocated memory.
        is_FreeImageMem(m_capture, m_imageMemory, m_pid);

        // Try to deinitialize camera.
        if (is_ExitCamera(m_capture) != IS_SUCCESS) {
            cerr << "proxy: Could not exit camera '" << m_name << "' with ID: " << m_id << endl;
        }
    }

    bool uEyeCamera::isValid() const {
        return (m_capture > 0);
    }

    bool uEyeCamera::captureFrame() {
        bool retVal = false;
        m_ueyeImagePtr = NULL;
        if (m_capture > 0) {
            if (is_GetImageMem(m_capture, (void**)&(m_ueyeImagePtr)) == IS_SUCCESS) {
                retVal = true;
            }
        }

        return retVal;
    }

    bool uEyeCamera::copyImageTo(char *dest, const uint32_t &size) {
        bool retVal = false;

        if ( (dest != NULL) && (size > 0) && (m_image != NULL) && (m_ueyeImagePtr != NULL) ) {
            ::memcpy(dest, m_ueyeImagePtr, size);
            m_image->imageData = (char*)m_ueyeImagePtr;

            //cvShowImage("WindowShowImage", m_image);
            //cvWaitKey(10);

            retVal = true;
        }

        return retVal;
    }

} // msv

#endif /*HAVE_UEYE*/

