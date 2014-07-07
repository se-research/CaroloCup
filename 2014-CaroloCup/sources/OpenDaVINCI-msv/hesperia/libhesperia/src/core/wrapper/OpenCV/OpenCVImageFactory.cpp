/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/OpenCV/OpenCVImage.h"
#include "core/wrapper/OpenCV/OpenCVImageFactory.h"

namespace core {
    namespace wrapper {
        namespace OpenCV {

            OpenCVImageFactory::OpenCVImageFactory() {}

            OpenCVImageFactory::~OpenCVImageFactory() {}

            Image* OpenCVImageFactory::getImage(istream &in) {
                Image *img = new OpenCVImage(in);
                return img;
            }

            Image* OpenCVImageFactory::getImage(const uint32_t &width, const uint32_t &height, const enum Image::FORMAT &format) {
                if ( ((width * height) == 0) || (format == Image::INVALID) ) {
                    return NULL;
                }
                return new OpenCVImage(width, height, format);
            }

            Image* OpenCVImageFactory::getImage(const uint32_t &width, const uint32_t &height, const enum Image::FORMAT &format, char *ptr) {
                if ( ((width * height) == 0) || (ptr == NULL) || (format == Image::INVALID) ) {
                    return NULL;
                }
                return new OpenCVImage(width, height, format, ptr);
            }
        }
    }
} // core::wrapper::OpenCV
