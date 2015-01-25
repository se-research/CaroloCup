/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_TOOLS_RECORDER_SHAREDDATAWRITER_H_
#define OPENDAVINCI_TOOLS_RECORDER_SHAREDDATAWRITER_H_

#include <iostream>
#include <map>

#include "core/base/Mutex.h"
#include "core/base/Service.h"
#include "core/base/FIFOQueue.h"

namespace tools {

    namespace recorder {

        using namespace std;

        /**
         * This class writes the FIFO of MemorySegments to an outstream.
         */
        class SharedDataWriter : public core::base::Service {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                SharedDataWriter(const SharedDataWriter &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                SharedDataWriter& operator=(const SharedDataWriter &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param out Output stream to write to.
                 */
                SharedDataWriter(ostream &out, map<uint32_t, char*> &mapOfMemories, core::base::FIFOQueue &bufferIn, core::base::FIFOQueue &bufferOut);

                virtual ~SharedDataWriter();

            private:
                virtual void beforeStop();

                virtual void run();

                void recordEntries();

            private:
                ostream &m_out;

                map<uint32_t, char*> &m_mapOfMemories;

                core::base::FIFOQueue &m_bufferIn;
                core::base::FIFOQueue &m_bufferOut;
        };

    } // recorder
} // tools

#endif /*OPENDAVINCI_TOOLS_RECORDER_SHAREDDATAWRITER_H_*/
