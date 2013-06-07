/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef FIFOMULTIPLEXER_H_
#define FIFOMULTIPLEXER_H_

#include <vector>

#include "core/base/FIFOQueue.h"
#include "core/base/Mutex.h"
#include "core/base/Service.h"
#include "core/data/Container.h"
#include "hesperia/base/DataStoreManager.h"

#include "ContainerObserver.h"

namespace monitor {

    using namespace std;

    /**
     * This class implements a simple FIFO for multiplexing incoming containers.
     */
    class FIFOMultiplexer : public core::base::Service, public ContainerObserver {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            FIFOMultiplexer(const FIFOMultiplexer &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            FIFOMultiplexer& operator=(const FIFOMultiplexer &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param dsm DataStoreManager to be used for registering own FIFO.
             */
            FIFOMultiplexer(hesperia::base::DataStoreManager &dsm);

            virtual ~FIFOMultiplexer();

            virtual void addContainerListener(core::io::ContainerListener *containerListener);

            virtual void removeContainerListener(core::io::ContainerListener *containerListener);

        protected:
            virtual void distributeContainer(core::data::Container &c);
            virtual core::data::Container leaveContainer();
            virtual uint32_t getFIFOSize();
            virtual void waitForData();

        private:
            hesperia::base::DataStoreManager &m_dataStoreManager;
            mutable core::base::Mutex m_fifoMutex;
            vector<core::io::ContainerListener*> m_listOfContainerListeners;
            core::base::FIFOQueue m_fifo;

            virtual void beforeStop();

            virtual void run();


    };

} // monitor

#endif /*FIFOMULTIPLEXER_H_*/
