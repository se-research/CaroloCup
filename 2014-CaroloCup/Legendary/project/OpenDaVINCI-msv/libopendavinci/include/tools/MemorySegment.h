/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_TOOLS_MEMORYSEGMENT_H_
#define OPENDAVINCI_TOOLS_MEMORYSEGMENT_H_

#include "core/data/Container.h"

namespace tools {

    using namespace std;

    class MemorySegment : public core::data::SerializableData {
        public:
            core::data::Container m_header;
            uint32_t m_size;
            uint32_t m_consumedSize;
            uint32_t m_id;

        public:
            /**
             * Constructor.
             */
            MemorySegment();

            MemorySegment(const MemorySegment &/*obj*/);

            MemorySegment& operator=(const MemorySegment &/*obj*/);

            virtual const string toString() const;

            virtual ostream& operator<<(ostream &out) const;
            virtual istream& operator>>(istream &in);
    };

} // tools

#endif /*OPENDAVINCI_TOOLS_MEMORYSEGMENT_H_*/
