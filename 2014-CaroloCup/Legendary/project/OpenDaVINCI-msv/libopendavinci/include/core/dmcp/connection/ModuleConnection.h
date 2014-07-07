/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_CONNECTION_MODULECONNECTION_H_
#define OPENDAVINCI_DMCP_CONNECTION_MODULECONNECTION_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/SharedPointer.h"

#include "core/base/Condition.h"
#include "core/io/Connection.h"
#include "core/io/ConnectionErrorListener.h"
#include "core/io/ContainerListener.h"

#include "core/data/dmcp/ModuleDescriptor.h"
#include "core/dmcp/ModuleConfigurationProvider.h"
#include "core/dmcp/ModuleStateListener.h"

namespace core {
    namespace dmcp {
        namespace connection {

            class OPENDAVINCI_API ModuleConnection : protected core::io::ConnectionErrorListener,
                                                  protected core::io::ContainerListener
            {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ModuleConnection(const ModuleConnection &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ModuleConnection& operator=(const ModuleConnection &);

                public:
                    ModuleConnection(core::io::Connection* connection,
                                     ModuleConfigurationProvider& configProvider);
                    virtual ~ModuleConnection();

                    void setModuleStateListener(ModuleStateListener* listener);

                    void waitForModuleDescription();
                    const core::data::dmcp::ModuleDescriptor getModuleDescriptor() const;

                protected:
                    virtual void nextContainer(core::data::Container &c);
                    virtual void handleConnectionError();

                    core::SharedPointer<core::io::Connection> m_connection;
                    ModuleConfigurationProvider& m_configurationProvider;

                    core::base::Condition m_discriptorCondition;
                    core::data::dmcp::ModuleDescriptor m_descriptor;
                    bool m_hasDescriptor;

                    ModuleStateListener* m_stateListener;
                    core::base::Mutex m_stateListenerMutex;
            };
        }
    }
} // core::dmcp

#endif /*OPENDAVINCI_DMCP_CONNECTION_MODULECONNECTION_H_*/
