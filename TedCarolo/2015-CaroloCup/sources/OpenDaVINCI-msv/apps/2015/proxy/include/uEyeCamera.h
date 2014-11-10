/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef UEYECAMERA_H_
#define UEYECAMERA_H_

#ifdef HAVE_UEYE

#include "opencv2/highgui/highgui.hpp"

#include <ueye.h>

#include "Camera.h"

namespace msv {

    using namespace std;

    /**
     * This class wraps a uEye camera and captures its data into a shared memory segment.
     */
    class uEyeCamera : public Camera {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            uEyeCamera(const uEyeCamera &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            uEyeCamera& operator=(const uEyeCamera &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param name Name of the shared memory segment.
             * @param id uEyeCamera identifier.
             * @param width
             * @param height
             * @param bpp
             */
            uEyeCamera(const string &name, const uint32_t &id, const uint32_t &width, const uint32_t &height, const uint32_t &bpp);

            virtual ~uEyeCamera();

        private:
            virtual bool copyImageTo(char *dest, const uint32_t &size);

            virtual bool isValid() const;

            virtual bool captureFrame();

        private:
            HIDS m_capture;
            char *m_imageMemory;
            void *m_ueyeImagePtr;
            int m_pid;
            IplImage *m_image;
    };

} // msv

#endif /*HAVE_UEYE*/

#endif /*UEYECAMERA_H_*/
