/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXSHAREDMEMORYFACTORY_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXSHAREDMEMORYFACTORY_H_

#include "core/wrapper/SharedMemoryFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            /**
             * This class is a concrete derivative for the abstract
             * factory SharedMemoryFactory.
             *
             * @See SharedMemoryFactory
             */
            class POSIXSharedMemoryFactory : public SharedMemoryFactory {
                protected:
                    friend class SharedMemoryFactory;

                    POSIXSharedMemoryFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXSharedMemoryFactory(const POSIXSharedMemoryFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXSharedMemoryFactory& operator=(const POSIXSharedMemoryFactory &);

                public:
                    virtual ~POSIXSharedMemoryFactory();

                    virtual SharedPointer<SharedMemory> createSharedMemory(const string &name, const uint32_t &size);

                    virtual SharedPointer<SharedMemory> attachToSharedMemory(const string &name);
            };

        }
    }
} // core::wrapper::POSIX

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXSHAREDMEMORYFACTORY_H_*/
