/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_SHAREDMEMORYFACTORY_H_
#define HESPERIA_CORE_WRAPPER_SHAREDMEMORYFACTORY_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include <string>

#include "core/SharedPointer.h"
#include "core/wrapper/Disposable.h"
#include "core/wrapper/SharedMemory.h"
#include "core/wrapper/Mutex.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * Abstract factory for creating shared memory between independent
         * processes using different implementations (i.e. Boost or POSIX).
         */
        class HESPERIA_API SharedMemoryFactory : public Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SharedMemoryFactory(const SharedMemoryFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SharedMemoryFactory& operator=(const SharedMemoryFactory &);

            protected:
                SharedMemoryFactory();

            public:
                virtual ~SharedMemoryFactory();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static SharedMemoryFactory& getInstance();

                /**
                 * This method returns the shared memory.
                 *
                 * @param name Name of the shared memory to create.
                 * @param size Required size for the new shared memory.
                 * @return Shared memory based on the type of instance this factory is.
                 */
                virtual SharedPointer<SharedMemory> createSharedMemory(const string &name, const uint32_t &size) = 0;

                /**
                 * This method returns the shared memory.
                 *
                 * @param name Name of the shared memory to attach.
                 * @return Shared memory based on the type of instance this factory is.
                 */
                virtual SharedPointer<SharedMemory> attachToSharedMemory(const string &name) = 0;

            private:
                static Mutex *m_singletonMutex;
                static SharedMemoryFactory *m_singleton;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_SHAREDMEMORYFACTORY_H_*/
