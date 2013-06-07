/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <limits>

#include "core/macros.h"

#include "core/base/Lock.h"
#include "core/base/BufferedFIFOQueue.h"

namespace core {
    namespace base {

        using namespace data;
        using namespace exceptions;

        BufferedFIFOQueue::BufferedFIFOQueue() :
                FIFOQueue(),
                m_bufferSize(numeric_limits<uint32_t>::max()) {}

        BufferedFIFOQueue::BufferedFIFOQueue(const uint32_t &bufferSize) :
                FIFOQueue(),
                m_bufferSize(bufferSize) {}

        BufferedFIFOQueue::~BufferedFIFOQueue() {}

        void BufferedFIFOQueue::enter(const data::Container &container) {
            if (getSize() < m_bufferSize) {
                FIFOQueue::enter(container);
            }
        }

        int32_t BufferedFIFOQueue::getIndexOfLastElement() const {
            return (getSize() - 1);
        }

        const Container BufferedFIFOQueue::getElementAt(const uint32_t &index) const throw (ArrayIndexOutOfBoundsException) {
            if (static_cast<int>(index) > getIndexOfLastElement()) {
                HESPERIA_CORE_THROW_EXCEPTION(ArrayIndexOutOfBoundsException, "Given index is invalid.");
            }

            return get(index);
        }

        bool BufferedFIFOQueue::isFull() const {
        	return (getSize() >= m_bufferSize - 1);
        }

    }
} // core::base
