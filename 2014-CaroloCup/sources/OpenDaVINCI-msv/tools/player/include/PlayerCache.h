/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PLAYERCACHE_H_
#define PLAYERCACHE_H_

#include <iostream>
#include <deque>

#include "core/base/Mutex.h"
#include "core/base/Service.h"
#include "core/data/Container.h"

namespace player {

    using namespace std;

    /**
     * This class caches containers from previously recorded file..
     */
    class PlayerCache : public core::base::Service {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            PlayerCache(const PlayerCache &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            PlayerCache& operator=(const PlayerCache &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param size Number of elements to be cached from file.
             * @param in Input stream to read data from.
             * @oaram autoRewind True if restart filling the queue.
             */
            PlayerCache(const uint32_t size, const bool &autoRewind, istream &in);

            virtual ~PlayerCache();

            /**
             * This method returns the next available container or
             * and empty container if the queue is empty.
             *
             * @return Container.
             */
            core::data::Container getNextContainer();

            /**
             * This method returns the number of entries in the queue.
             *
             * @return Number of entries in the queue.
             */
            uint32_t getNumberOfEntries() const;

            /**
             * This method clears the queue, rewinds the input stream and fills
             * the queue with the start of the input stream.
             */
            void clearQueueRewindInputStream();

        private:
            uint32_t m_cacheSize;
            const bool m_autoRewind;
            istream &m_in;

            mutable core::base::Mutex m_queueMutex;
            deque<core::data::Container> m_queue;

            virtual void beforeStop();

            virtual void run();
    };

} // player

#endif /*PLAYERCACHE_H_*/
