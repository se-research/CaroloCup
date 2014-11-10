/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Thread.h"

#include "tools/MemorySegment.h"
#include "tools/recorder/SharedDataWriter.h"

namespace tools {

    namespace recorder {

        using namespace core::base;
        using namespace core::data;
        using namespace tools;

        SharedDataWriter::SharedDataWriter(ostream &out, map<uint32_t, char*> &mapOfMemories, FIFOQueue &bufferIn, FIFOQueue &bufferOut) :
            m_out(out),
            m_mapOfMemories(mapOfMemories),
            m_bufferIn(bufferIn),
            m_bufferOut(bufferOut)
        {}

        SharedDataWriter::~SharedDataWriter() {
            cout << "SharedDataWriter: Cleaning queue... ";
            recordEntries();
            cout << "done." << endl;
        }

        void SharedDataWriter::beforeStop() {}

        void SharedDataWriter::recordEntries() {
            while (!m_bufferOut.isEmpty()) {
                if (m_out.good()) {
                    // Get next entry to process from output queue.
                    Container c = m_bufferOut.leave();
                    MemorySegment ms = c.getData<MemorySegment>();

                    // Get meta data to be written as header.
                    Container header = ms.m_header;

                    // Get pointer to memory with the data.
                    char *ptrToMemory = m_mapOfMemories[ms.m_id];

                    m_out << header;
                    m_out.write(ptrToMemory, ms.m_consumedSize);

                    // Reset meta information.
                    ms.m_consumedSize = 0;

                    // Save meta information.
                    c = Container(Container::UNDEFINEDDATA, ms);

                    // After processing, put memory segment back into input queue.
                    m_bufferIn.enter(c);
                }

                m_out.flush();

                // Allow rescheduling between processing the different containers.
                Thread::usleep(500);
            }
        }

        void SharedDataWriter::run() {
            serviceReady();

            while (isRunning()) {
                recordEntries();

                // Allow rescheduling for 5ms between the next cycle.
                Thread::usleep(5000);
            }
        }

    } // recorder
} // tools

