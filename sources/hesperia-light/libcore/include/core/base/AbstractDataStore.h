/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_ABSTRACTDATASTORE_H_
#define HESPERIA_CORE_BASE_ABSTRACTDATASTORE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/Condition.h"
#include "core/data/Container.h"

namespace core {
    namespace base {

        /**
         * This interface encapsulates methods for every data store.
         */
        class HESPERIA_API AbstractDataStore {
            public:
                AbstractDataStore();

                virtual ~AbstractDataStore();

                /**
                 * This method can be called to fall asleep.
                 */
                virtual void waitForData();

                /**
                 * This method wakes all waiting threads.
                 */
                virtual void wakeAll();

                /**
                 * This method adds a new container to this data store
                 * depending on the entering strategy (i.e. front or end).
                 *
                 * @param container Container to be added.
                 */
                virtual void add(const data::Container &container) = 0;

                /**
                 * This method clears a datastore.
                 */
                virtual void clear() = 0;

                /**
                 * This method returns the number of elements currently
                 * available in the queue.
                 */
                virtual uint32_t getSize() const = 0;

                /**
                 * This method returns true if getSize() == 0.
                 *
                 * @return true if getSize() == 0.
                 */
                virtual bool isEmpty() const = 0;

            protected:
              /**
               * This method can be called to fall asleep.
              */
              virtual void wait();

            private:
                Condition m_condition;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_ABSTRACTDATASTORE_H_*/
