/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CAMERA_H_
#define CAMERA_H_

#include <stdint.h>

#include "core/SharedPointer.h"
#include "core/data/image/SharedImage.h"
#include "core/wrapper/SharedMemory.h"

#include <string>

namespace msv {

    using namespace std;

    /**
     * This class wraps a camera and captures its data into a shared memory segment.
     */
    class Camera {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Camera(const Camera &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Camera& operator=(const Camera &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param name Name of the shared memory segment.
             * @param id Camera identifier.
             * @param width
             * @param height
             * @param bpp
             */
            Camera(const string &name, const uint32_t &id, const uint32_t &width, const uint32_t &height, const uint32_t &bpp);

            virtual ~Camera();

            /**
             * @return Meta information about the image.
             */
            core::data::image::SharedImage capture();

        protected:
            /**
             * This method is responsible to copy the image from the
             * specific camera driver to the shared memory.
             *
             * @param dest Pointer where to copy the data.
             * @param size Number of bytes to copy.
             * @return true if the data was successfully copied.
             */
            virtual bool copyImageTo(char *dest, const uint32_t &size) = 0;

            virtual bool captureFrame() = 0;

            virtual bool isValid() const = 0;

            const string getName() const;

            uint32_t getID() const;

            uint32_t getWidth() const;

            uint32_t getHeight() const;

            uint32_t getBPP() const;

            uint32_t getSize() const;

        private:
            core::data::image::SharedImage m_sharedImage;
            core::SharedPointer<core::wrapper::SharedMemory> m_sharedMemory;
            
        protected:
            string m_name;
            uint32_t m_id;
            uint32_t m_width;
            uint32_t m_height;
            uint32_t m_bpp;
            uint32_t m_size;
    };

} // msv

#endif /*CAMERA_H_*/
