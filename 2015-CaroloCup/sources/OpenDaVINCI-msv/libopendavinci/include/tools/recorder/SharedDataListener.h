/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_TOOLS_RECORDER_SHAREDDATALISTENER_H_
#define OPENDAVINCI_TOOLS_RECORDER_SHAREDDATALISTENER_H_

#include <map>
#include <string>

#include "core/SharedPointer.h"
#include "core/base/ConferenceClientModule.h"
#include "core/base/FIFOQueue.h"
#include "core/data/Container.h"
#include "core/data/SerializableData.h"
#include "core/data/SharedData.h"
#include "core/data/image/SharedImage.h"
#include "core/wrapper/SharedMemory.h"

#include "tools/recorder/SharedDataWriter.h"

namespace tools {

    namespace recorder {

        using namespace std;

        /**
         * This class encapsulates a listener for SharedData containers.
         */
        class SharedDataListener : public core::base::AbstractDataStore {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                SharedDataListener(const SharedDataListener &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                SharedDataListener& operator=(const SharedDataListener &/*obj*/);

            public:
                /**
                 * Constructor.
                 */
                SharedDataListener(ostream &out, const uint32_t &memorySegmentSize, const uint32_t &numberOfMemorySegments);

                virtual ~SharedDataListener();

                virtual void add(const core::data::Container &container);

                virtual void clear();

                virtual uint32_t getSize() const;

                virtual bool isEmpty() const;

            private:
                /**
                 * This method copies the data pointed to by SharedData
                 * or SharedImage to the next available MemorySegment.
                 *
                 * @param name Name of SharedPointer to be used.
                 * @param header Container that contains the meta-data for this shared memory segment which shall be used as header in the file.
                 * @return true if the copy succeeded.
                 */
                bool copySharedMemoryToMemorySegment(const string &name, const core::data::Container &header);

            private:
                SharedDataWriter *m_sharedDataWriter;
                map<string, core::data::SharedData> m_mapOfAvailableSharedData;
                map<string, core::data::image::SharedImage> m_mapOfAvailableSharedImages;

                map<uint32_t, char*> m_mapOfMemories;

                core::base::FIFOQueue m_bufferIn;
                core::base::FIFOQueue m_bufferOut;

                map<string, core::SharedPointer<core::wrapper::SharedMemory> > m_sharedPointers;

                ostream &m_out;
        };

    } // recorder
} // tools

#endif /*OPENDAVINCI_TOOLS_RECORDER_SHAREDDATALISTENER_H_*/
