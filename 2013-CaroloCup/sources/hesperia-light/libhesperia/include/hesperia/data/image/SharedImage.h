/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_IMAGE_SHAREDIMAGE_H_
#define HESPERIA_DATA_IMAGE_SHAREDIMAGE_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SharedData.h"

namespace hesperia {
    namespace data {
        namespace image {

            using namespace std;

            /**
             * This class provides information about a shared image using shared memory
             * segments. This class can be used to exchange information about
             * accessing this image. It can be used as follows:
             *
             * @code
             * // Server:
             * wrapper::Image *m_image = ...
             * SharedPointer<wrapper::SharedMemory> memory = wrapper::SharedMemoryFactory::getInstance().createSharedMemory("NameOfSharedMemory", 1024);
             * if (memory->isValid()) {
             *     SharedImage si;
             *     si.setName(memory->getName());
             *     si.setWidth(m_image->getWidth());
             *     si.setHeight(m_image->getHeight());
             *     si.setBytesPerPixel(m_image->getBytesPerPixel());
             *
             *     Container c(Container::SHARED_IMAGE, si);
             *     getConference().send(c);
             *
             *     // Write something to shared memory.
             *     memory->lock();
             *     memcpy(memory->getAddress(), m_image->getRawData(), 1024);
             *     memory->unlock();
             *     ...
             * }
             *
             *
             * // Client:
             * Container c = myFifo.leave();
             * if (c.getDataType() == Container::SHARED_IMAGE) {
             *     SharedImage si = c.getData<SharedImage>();
             *     SharedPointer<wrapper::SharedMemory> memory = wrapper::SharedMemoryFactory::getInstance().attachToSharedMemory(si.getName());
             *     if (memory->isValid()) {
             *         memory->lock();
             *         ...
             *         memory->unlock();
             *     }
             * }
             * @endcode
             */
            class HESPERIA_API SharedImage : public core::data::SharedData {
                public:
                    SharedImage();

                    virtual ~SharedImage();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    SharedImage(const SharedImage &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    SharedImage& operator=(const SharedImage &obj);

                    /**
                     * This method returns the image's width.
                     *
                     * @return Image's width.
                     */
                    uint32_t getWidth() const;

                    /**
                     * This method sets the image's width.
                     *
                     * @param width Image's width.
                     */
                    void setWidth(const uint32_t &width);

                    /**
                     * This method returns the image's height.
                     *
                     * @return Image's height.
                     */
                    uint32_t getHeight() const;

                    /**
                     * This method sets the image's height.
                     *
                     * @param height Image's height.
                     */
                    void setHeight(const uint32_t &height);

                    /**
                     * This method returns the image's bytes per pixel.
                     *
                     * @return Image's bytes per pixel.
                     */
                    uint32_t getBytesPerPixel() const;

                    /**
                     * This method sets the image's bytes per pixel.
                     *
                     * @param bytesPerPixel Image's bytes per pixel.
                     */
                    void setBytesPerPixel(const uint32_t &bytesPerPixel);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    uint32_t m_width;
                    uint32_t m_height;
                    uint32_t m_bytesPerPixel;
            };

        }
    }
} // hesperia::data::image

#endif /*HESPERIA_DATA_IMAGE_SHAREDIMAGE_H_*/
