/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_OPENCV_OPENCVIMAGEFACTORY_H_
#define HESPERIA_CORE_WRAPPER_OPENCV_OPENCVIMAGEFACTORY_H_

#include "core/wrapper/ImageFactory.h"

namespace core {
    namespace wrapper {
        namespace OpenCV {

            /**
             * This class is a concrete derivative for the abstract
             * factory ImageFactory.
             *
             * @See ImageFactory
             */
            class OpenCVImageFactory : public ImageFactory {
                protected:
                    friend class ImageFactory;

                    OpenCVImageFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    OpenCVImageFactory(const OpenCVImageFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    OpenCVImageFactory& operator=(const OpenCVImageFactory &);

                public:
                    virtual ~OpenCVImageFactory();

                    virtual Image* getImage(istream &in);

                    virtual Image* getImage(const uint32_t &width, const uint32_t &height, const enum Image::FORMAT &format);

                    virtual Image* getImage(const uint32_t &width, const uint32_t &height, const enum Image::FORMAT &format, char *ptr);
            };

        }
    }
} // core::wrapper::OpenCV

#endif /*HESPERIA_CORE_WRAPPER_OPENCV_OPENCVIMAGEFACTORY_H_*/
