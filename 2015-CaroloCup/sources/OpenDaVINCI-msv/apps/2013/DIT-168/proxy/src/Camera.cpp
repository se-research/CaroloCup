/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/highgui/highgui.hpp"

#include "core/wrapper/SharedMemoryFactory.h"

#include "Camera.h"

namespace msv {

    Camera::Camera(const string &name, const uint32_t &id, const uint32_t &width, const uint32_t &height, const uint32_t &bpp) :
        m_sharedImage(),
        m_sharedMemory(),
        m_name(name),
        m_id(id),
        m_width(width),
        m_height(height),
        m_bpp(bpp),
        m_size(0) {

        m_sharedMemory = core::wrapper::SharedMemoryFactory::createSharedMemory(name, width * height * bpp);

        m_sharedImage.setName(name);
        m_sharedImage.setWidth(width);
        m_sharedImage.setHeight(height);
        m_sharedImage.setBytesPerPixel(bpp);

        m_size = width * height * bpp;
    }

    Camera::~Camera() {}

    core::data::image::SharedImage Camera::capture() {
        if (isValid()) {
            if (captureFrame()) {
                if (m_sharedMemory.isValid() && m_sharedMemory->isValid()) {
                    m_sharedMemory->lock();
                        copyImageTo((char*)m_sharedMemory->getSharedMemory(), m_size);
                    m_sharedMemory->unlock();
                }
            }
        }

        return m_sharedImage;
    }

} // msv

