/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_LIFOQUEUE_H_
#define HESPERIA_CORE_BASE_LIFOQUEUE_H_

#include <deque>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/AbstractDataStore.h"
#include "core/base/Mutex.h"
#include "core/data/Container.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This interface encapsulates all methods necessary for a LIFO (stack).
         */
        class HESPERIA_API LIFOQueue : public AbstractDataStore  {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                LIFOQueue(const LIFOQueue &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                LIFOQueue& operator=(const LIFOQueue &);

            public:
                LIFOQueue();

                virtual ~LIFOQueue();

                virtual void clear();

                virtual void push(const data::Container &container);

                const data::Container pop();

                virtual void add(const data::Container &container);

                virtual uint32_t getSize() const;

                virtual bool isEmpty() const;

            protected:
                /**
                 * This method returns the element at the given index or an
                 * empty container.
                 *
                 * @param index Index of the element to be retrieved.
                 * @return Element at the given index.
                 */
                const data::Container get(const uint32_t &index) const;

            private:
                mutable Mutex m_mutexQueue;
                deque<data::Container> m_queue;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_LIFOQUEUE_H_*/
