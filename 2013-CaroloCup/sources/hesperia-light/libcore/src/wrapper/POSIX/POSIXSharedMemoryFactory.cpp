/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/POSIX/POSIXSharedMemory.h"
#include "core/wrapper/POSIX/POSIXSharedMemoryFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            POSIXSharedMemoryFactory::POSIXSharedMemoryFactory() {}

            POSIXSharedMemoryFactory::~POSIXSharedMemoryFactory() {}

            SharedPointer<SharedMemory> POSIXSharedMemoryFactory::createSharedMemory(const string &name, const uint32_t &size) {
                return SharedPointer<SharedMemory>(new POSIXSharedMemory(name, size));
            }

            SharedPointer<SharedMemory> POSIXSharedMemoryFactory::attachToSharedMemory(const string &name) {
                return SharedPointer<SharedMemory>(new POSIXSharedMemory(name));
            }

        }
    }
} // core::wrapper::POSIX
