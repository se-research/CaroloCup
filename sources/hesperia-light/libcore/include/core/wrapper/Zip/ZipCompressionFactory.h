/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_ZIP_ZIPCOMPRESSIONFACTORY_H_
#define HESPERIA_CORE_WRAPPER_ZIP_ZIPCOMPRESSIONFACTORY_H_

#include "core/wrapper/CompressionFactory.h"

namespace core {
    namespace wrapper {
        namespace Zip {

            /**
             * This class is a concrete derivative for the abstract
             * factory CompressionFactory.
             *
             * @See CompressionFactory
             */
            class ZipCompressionFactory : public CompressionFactory {
                protected:
                    friend class CompressionFactory;

                    ZipCompressionFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ZipCompressionFactory(const ZipCompressionFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ZipCompressionFactory& operator=(const ZipCompressionFactory &);

                public:
                    virtual ~ZipCompressionFactory();

                    virtual DecompressedData* getContents(istream &in);
            };

        }
    }
} // core::wrapper::Zip

#endif /*HESPERIA_CORE_WRAPPER_ZIP_ZIPCOMPRESSIONFACTORY_H_*/
