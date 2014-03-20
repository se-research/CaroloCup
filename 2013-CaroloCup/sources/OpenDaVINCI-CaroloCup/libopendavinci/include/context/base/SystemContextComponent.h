/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_SYSTEMCONTEXTCOMPONENT_H_
#define CONTEXT_BASE_SYSTEMCONTEXTCOMPONENT_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/SharedPointer.h"
#include "core/base/FIFOQueue.h"
#include "core/base/KeyValueDataStore.h"
#include "core/io/ContainerListener.h"
#include "context/base/Runner.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class can be used to compute data for system's parts
         * which can be either feedback or reporting components.
         */
        class OPENDAVINCI_API SystemContextComponent : public Runner, public core::io::ContainerListener {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SystemContextComponent(const SystemContextComponent&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SystemContextComponent& operator=(const SystemContextComponent&);

            protected:
                /**
                 * Protected constructor to enforce subclasses.
                 */
                SystemContextComponent();

            public:
                virtual ~SystemContextComponent();

                /**
                 * This method is called to setup this component.
                 */
                virtual void setup() = 0;

                /**
                 * This method is called to tear down this component.
                 */
                virtual void tearDown() = 0;

                virtual void nextContainer(core::data::Container &c);

                /**
                 * This method returns the FIFO containing received containers.
                 *
                 * @return FIFO containing received containers.
                 */
                core::base::FIFOQueue& getFIFO();

                /**
                 * This method returns the key/value-data store.
                 *
                 * @return Key/value-datastore.
                 */
                core::base::KeyValueDataStore& getKeyValueDataStore();

            private:
                core::base::FIFOQueue m_fifo;
                core::SharedPointer<core::base::KeyValueDataStore> m_keyValueDataStore;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_SYSTEMCONTEXTCOMPONENT_H_*/
