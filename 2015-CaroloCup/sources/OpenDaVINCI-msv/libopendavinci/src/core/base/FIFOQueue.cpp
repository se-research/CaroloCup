/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/base/FIFOQueue.h"

namespace core {
    namespace base {

        using namespace data;

        FIFOQueue::FIFOQueue() :
                m_mutexQueue(),
                m_queue() {}

        FIFOQueue::~FIFOQueue() {
            wakeAll();
        }

        void FIFOQueue::clear() {
            {
                Lock l(m_mutexQueue);
                m_queue.clear();
            }
            wakeAll();
        }

        void FIFOQueue::enter(const Container &container) {
            {
                Lock l(m_mutexQueue);
                m_queue.push_back(container);
            }
            wakeAll();
        }

        const Container FIFOQueue::leave() {
            waitForData();

            Container container;
            if (!isEmpty()) {
                Lock l(m_mutexQueue);
                container = m_queue.front();
                m_queue.pop_front();
            }

            return container;
        }

        const Container FIFOQueue::get(const uint32_t &index) const {
            Container container;

            if (!isEmpty()) {
                if (index < getSize()) {
                    Lock l(m_mutexQueue);
                    container = m_queue[index];
                }
            }

            return container;
        }

        void FIFOQueue::add(const Container &container) {
            enter(container);
        }

        uint32_t FIFOQueue::getSize() const {
            Lock l(m_mutexQueue);
            return static_cast<uint32_t>(m_queue.size());
        }

        bool FIFOQueue::isEmpty() const {
            Lock l(m_mutexQueue);
            return m_queue.empty();
        }
    }
} // core::base
