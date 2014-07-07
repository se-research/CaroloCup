/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_SHAREDMEMORYTESTSUITE_H_
#define CORE_SHAREDMEMORYTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include "core/SharedPointer.h"
#include "core/wrapper/SharedMemory.h"
#include "core/wrapper/SharedMemoryFactory.h"

using namespace std;

class SharedMemoryTest : public CxxTest::TestSuite {
    public:
        void testSharedMemory() {
            core::SharedPointer<core::wrapper::SharedMemory> memClient;

            core::SharedPointer<core::wrapper::SharedMemory> memServer = core::wrapper::SharedMemoryFactory::createSharedMemory("SharedMemoryTest", 10);
            TS_ASSERT(memServer->isValid());
            TS_ASSERT(memServer->getSize() == 10);
            memServer->lock();
            for (uint32_t i = 0; i < memServer->getSize(); i++) {
                *(((char*)(memServer->getSharedMemory())) + i) = ('A' + i);
            }
            memServer->unlock();

            TS_ASSERT(!memClient.isValid());

            memClient = core::wrapper::SharedMemoryFactory::attachToSharedMemory("SharedMemoryTest");
            TS_ASSERT(memClient->isValid());
            TS_ASSERT(memClient->getSize() == 10);
            memClient->lock();
            for (uint32_t i = 0; i < memClient->getSize(); i++) {
                char c = *(((char*)(memClient->getSharedMemory())) + i);
                TS_ASSERT(c == (char)('A' + i));
            }
            memClient->unlock();
        }

};

#endif /*CORE_SHAREDMEMORYTESTSUITE_H_*/
