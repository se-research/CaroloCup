/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/base/LIFOQueue.h"

namespace core {
    namespace base {

        using namespace data;

        LIFOQueue::LIFOQueue() :
                m_mutexQueue(),
                m_queue() {}

        LIFOQueue::~LIFOQueue() {
            wakeAll();
        }

        void LIFOQueue::clear() {
            {
                Lock l(m_mutexQueue);
                m_queue.clear();
            }

            wakeAll();
        }

        void LIFOQueue::push(const Container &container) {
            {
                Lock l(m_mutexQueue);
                m_queue.push_front(container);
            }

            wakeAll();
        }

        const Container LIFOQueue::pop() {
            waitForData();

            Container container;
            if (!isEmpty()) {
                Lock l(m_mutexQueue);
                container = m_queue.front();
                m_queue.pop_front();
            }

            return container;
        }

        const Container LIFOQueue::get(const uint32_t &index) const {
            Container container;

            if (!isEmpty()) {
                if (index < getSize()) {
                    Lock l(m_mutexQueue);
                    container = m_queue[index];
                }
            }

            return container;
        }

        void LIFOQueue::add(const Container &container) {
            push(container);
        }

        uint32_t LIFOQueue::getSize() const {
            Lock l(m_mutexQueue);
            return static_cast<uint32_t>(m_queue.size());
        }

        bool LIFOQueue::isEmpty() const {
            Lock l(m_mutexQueue);
            return m_queue.empty();
        }

    }
} // core::base
