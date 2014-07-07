/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_BUFFEREDFIFOQUEUE_H_
#define OPENDAVINCI_CORE_BASE_BUFFEREDFIFOQUEUE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/FIFOQueue.h"
#include "core/exceptions/Exceptions.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This interface encapsulates all methods necessary for a buffered FIFO.
         * A buffered FIFO allows indexed access to the elements without removing
         * them.
         */
        class OPENDAVINCI_API BufferedFIFOQueue : public FIFOQueue {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                BufferedFIFOQueue(const BufferedFIFOQueue &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                BufferedFIFOQueue& operator=(const BufferedFIFOQueue &);

            public:
                BufferedFIFOQueue();

                /**
                 * Constructor.
                 *
                 * @param bufferSize Maximum size for this buffer fifo.
                 */
                BufferedFIFOQueue(const uint32_t &bufferSize);

                virtual ~BufferedFIFOQueue();

                virtual void enter(const data::Container &container);

                /**
                 * This method returns the index of the last element.
                 *
                 * @return Index of the last element or -1 if the FIFO is empty.
                 */
                int32_t getIndexOfLastElement() const;

                /**
                 * This method returns the elements at the current index.
                 *
                 * @param index Index of the element to be retrieved.
                 * @return Element at the given index.
                 * @throws ArrayIndexOutOfBoundsException if the index is invalid.
                 */
                const data::Container getElementAt(const uint32_t &index) const throw (exceptions::ArrayIndexOutOfBoundsException);

                /**
                 * This method returns whether the queue is full or not.
                 *
                 * @return true if the queue-size reached it's maximum.
                 */
                virtual bool isFull() const;

            private:
                uint32_t m_bufferSize;
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_BUFFEREDFIFOQUEUE_H_*/
