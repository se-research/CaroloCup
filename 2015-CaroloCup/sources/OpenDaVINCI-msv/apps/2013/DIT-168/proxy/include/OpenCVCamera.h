/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENCVCAMERA_H_
#define OPENCVCAMERA_H_

#include "opencv2/highgui/highgui.hpp"

#include "Camera.h"

namespace msv {

    using namespace std;

    /**
     * This class wraps an OpenCV camera and captures its data into a shared memory segment.
     */
    class OpenCVCamera : public Camera {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            OpenCVCamera(const OpenCVCamera &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            OpenCVCamera& operator=(const OpenCVCamera &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param name Name of the shared memory segment.
             * @param id OpenCVCamera identifier.
             * @param width
             * @param height
             * @param bpp
             */
            OpenCVCamera(const string &name, const uint32_t &id, const uint32_t &width, const uint32_t &height, const uint32_t &bpp);

            virtual ~OpenCVCamera();

        private:
            virtual bool copyImageTo(char *dest, const uint32_t &size);

            virtual bool isValid() const;

            virtual bool captureFrame();

        private:
            CvCapture *m_capture;
            IplImage *m_image;
    };

} // msv

#endif /*OPENCVCAMERA_H_*/
