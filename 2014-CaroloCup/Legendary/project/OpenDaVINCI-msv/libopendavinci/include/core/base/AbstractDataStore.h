/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_ABSTRACTDATASTORE_H_
#define OPENDAVINCI_CORE_BASE_ABSTRACTDATASTORE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Condition.h"
#include "core/data/Container.h"

namespace core {
    namespace base {

        /**
         * This interface encapsulates methods for every data store.
         */
        class OPENDAVINCI_API AbstractDataStore {
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

#endif /*OPENDAVINCI_CORE_BASE_ABSTRACTDATASTORE_H_*/
