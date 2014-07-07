/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DECORATOR_MODELS_OBJXARCHIVEFACTORY_H_
#define HESPERIA_CORE_DECORATOR_MODELS_OBJXARCHIVEFACTORY_H_

#include <iostream>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/Mutex.h"
#include "core/exceptions/Exceptions.h"

#include "hesperia/decorator/models/OBJXArchive.h"

namespace hesperia {
    namespace decorator {
        namespace models {

            using namespace std;

            /**
             * This class produces an instance for accessing the contents
             * of an OBJXArchive (.objx) from a given input stream.
             */
            class OPENDAVINCI_API OBJXArchiveFactory {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    OBJXArchiveFactory(const OBJXArchiveFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    OBJXArchiveFactory& operator=(const OBJXArchiveFactory &);

                private:
                    OBJXArchiveFactory();

                public:
                    virtual ~OBJXArchiveFactory();

                    /**
                     * This method returns a static instance for this factory.
                     *
                     * @return Instance of this factory.
                     */
                    static OBJXArchiveFactory& getInstance();

                    /**
                     * This method returns the OBJXArchive data structure for a simple obj file without material and textures.
                     *
                     * @param in Input stream containing a plain obj file.
                     * @return OBJXArchive.
                     * @throws InvalidArgumentException if the URL could not be used to create the data structure.
                     */
                    OBJXArchive* getOBJXArchiveFromPlainOBJFile(istream &in) throw (core::exceptions::InvalidArgumentException);

                    /**
                     * This method returns the OBJXArchive data structure.
                     *
                     * @param in Input stream to be used for building the OBJX archive file.
                     * @return OBJXArchive.
                     * @throws InvalidArgumentException if the URL could not be used to create the data structure.
                     */
                    OBJXArchive* getOBJXArchive(istream &in) throw (core::exceptions::InvalidArgumentException);

                private:
                    static core::base::Mutex m_singletonMutex;
                    static OBJXArchiveFactory* m_singleton;
            };

        }
    }
} // hesperia::decorator::models

#endif /*HESPERIA_CORE_DECORATOR_MODELS_OBJXARCHIVEFACTORY_H_*/
