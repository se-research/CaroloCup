/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <limits>

#include "core/macros.h"

#include "core/base/Lock.h"
#include "core/base/BufferedLIFOQueue.h"

namespace core {
    namespace base {

        using namespace data;
        using namespace exceptions;

        BufferedLIFOQueue::BufferedLIFOQueue() :
                LIFOQueue(),
                m_bufferSize(numeric_limits<uint32_t>::max()) {}

        BufferedLIFOQueue::BufferedLIFOQueue(const uint32_t &bufferSize) :
                LIFOQueue(),
                m_bufferSize(bufferSize) {}

        BufferedLIFOQueue::~BufferedLIFOQueue() {}

        void BufferedLIFOQueue::push(const data::Container &container) {
            if (getSize() < m_bufferSize) {
                LIFOQueue::push(container);
            }
        }

        int32_t BufferedLIFOQueue::getIndexOfLastElement() const {
            return (getSize() - 1);
        }

        const Container BufferedLIFOQueue::getElementAt(const uint32_t &index) const throw (ArrayIndexOutOfBoundsException) {
            if (static_cast<int>(index) > getIndexOfLastElement()) {
                HESPERIA_CORE_THROW_EXCEPTION(ArrayIndexOutOfBoundsException, "Given index is invalid.");
            }

            return get(index);
        }

    }
} // core::base
