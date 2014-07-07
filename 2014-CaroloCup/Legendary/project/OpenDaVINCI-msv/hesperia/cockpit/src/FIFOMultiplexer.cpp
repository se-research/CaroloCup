/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/base/Lock.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"

#include "FIFOMultiplexer.h"

namespace cockpit {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::io;

    FIFOMultiplexer::FIFOMultiplexer(DataStoreManager &dsm) :
        m_dataStoreManager(dsm),
        m_fifoMutex(),
        m_listOfContainerListeners(),
        m_fifo() {}

    FIFOMultiplexer::~FIFOMultiplexer() {
        Lock l(m_fifoMutex);
        m_listOfContainerListeners.clear();
    }

    void FIFOMultiplexer::addContainerListener(core::io::ContainerListener *containerListener) {
        if (containerListener != NULL) {
            Lock l(m_fifoMutex);
            m_listOfContainerListeners.push_back(containerListener);
        }
    }

    void FIFOMultiplexer::removeContainerListener(core::io::ContainerListener *containerListener) {
        if (containerListener != NULL) {
            Lock l(m_fifoMutex);
            vector<ContainerListener*>::iterator it = m_listOfContainerListeners.begin();
            while (it != m_listOfContainerListeners.end()) {
                if (*it == containerListener) {
                    break;
                }
                it++;
            }

            // Actually remove the container listener.
            if (it != m_listOfContainerListeners.end()) {
                m_listOfContainerListeners.erase(it);
            }
        }
    }

    void FIFOMultiplexer::beforeStop() {
        // Awake our thread.
        m_fifo.wakeAll();
    }

    void FIFOMultiplexer::waitForData(){
        m_fifo.waitForData();
    }

    void FIFOMultiplexer::distributeContainer(Container &c){
    	{
    	  Lock l(m_fifoMutex);
    	  vector<ContainerListener*>::iterator it = m_listOfContainerListeners.begin();
    	  while (it != m_listOfContainerListeners.end()) {
    		  ContainerListener *cl = (*it++);
    		  if (cl != NULL) {
    			 cl->nextContainer(c);
    		  }
    	  }
    	}
    }

    Container FIFOMultiplexer::leaveContainer(){
    	Container c;
    	{
    	   Lock l(m_fifoMutex);
    	   c = m_fifo.leave();
    	}
    	return c;
    }

    uint32_t FIFOMultiplexer::getFIFOSize(){
    	uint32_t size = 0;
    	{
    	   Lock l(m_fifoMutex);
    	   size = m_fifo.getSize();
    	}
    	return size;
    }

    void FIFOMultiplexer::run() {
        // Register FIFO for receiving new data.
        m_dataStoreManager.addDataStoreFor(m_fifo);

        serviceReady();
        while (isRunning()) {
        	waitForData();

            if (isRunning()) {
                // Distribute new containers.
                for (uint32_t i = 0; i < getFIFOSize(); i++) {
                	Container c = leaveContainer();
                    distributeContainer(c);
                }
            }
        }
    }

} // cockpit

