/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/KeyValueDatabaseFactory.h"

#include "context/base/SystemContextComponent.h"

namespace context {
    namespace base {

        using namespace std;
        using namespace core::base;

        SystemContextComponent::SystemContextComponent() :
            m_fifo(),
            m_keyValueDataStore() {
            // Create an in-memory database.
            m_keyValueDataStore = core::SharedPointer<KeyValueDataStore>(new KeyValueDataStore(core::wrapper::KeyValueDatabaseFactory::createKeyValueDatabase("")));
        }

        SystemContextComponent::~SystemContextComponent() {}

        void SystemContextComponent::nextContainer(core::data::Container &c) {
            m_fifo.add(c);

            // Store data using a plain map.
            m_keyValueDataStore->put(c.getDataType(), c);
        }

        core::base::FIFOQueue& SystemContextComponent::getFIFO() {
            return m_fifo;
        }

        core::base::KeyValueDataStore& SystemContextComponent::getKeyValueDataStore() {
        	return *m_keyValueDataStore;
        }

    }
} // context::base
