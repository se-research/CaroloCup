/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_TOOLS_RECORDER_RECORDER_H_
#define OPENDAVINCI_TOOLS_RECORDER_RECORDER_H_

#include "core/base/FIFOQueue.h"
#include "core/data/Container.h"

#include "tools/recorder/SharedDataListener.h"

namespace tools {

    namespace recorder {

        using namespace std;

        /**
         * This class is the interface to use the recorder module from within other modules.s
         */
        class Recorder {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                Recorder(const Recorder &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                Recorder& operator=(const Recorder &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param url URL of the resource to be used for writing containers to.
                 * @param memorySegmentSize Size of a memory segment for storing shared memory data (like shared images).
                 * @param numberOfSegments Number of segments to be used.
                 */
                Recorder(const string &url, const uint32_t &memorySegmentSize, const uint32_t &numberOfSegments);

                virtual ~Recorder();

                /**
                 * This method records data from the given FIFOQueue.
                 *
                 * @param fifo FIFOQueue to be used for recording data.
                 */
                void recordQueueEntries();

                /**
                 * This method returns the FIFO to be used for all
                 * containers except for shared memory segments.
                 *
                 * @return Reference to queue to be used for queuing Containers.
                 */
                core::base::FIFOQueue& getFIFO();

                /**
                 * This method returns the data store to be used
                 * for storing shared memory.
                 *
                 * @return Reference to data handler to be used for storing shared memory.
                 */
                SharedDataListener& getDataStoreForSharedData();

                /**
                 * This method stores the given container. Depending on the container
                 * data type, either the FIFO queue is used or the one to handle
                 * shared memory data.
                 *
                 * @param c Container to be recorded.
                 */
                void store(core::data::Container c);

            private:
                core::base::FIFOQueue m_fifo;
                SharedDataListener *m_sharedDataListener;
                ostream *m_out;
                ostream *m_outSharedMemoryFile;
        };

    } // recorder
} // tools

#endif /*OPENDAVINCI_TOOLS_RECORDER_RECORDER_H_*/
