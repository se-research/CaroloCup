/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_CONNECTION_MODULECONNECTION_H_
#define HESPERIA_DMCP_CONNECTION_MODULECONNECTION_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/SharedPointer.h"

#include "core/base/Condition.h"
#include "core/io/Connection.h"
#include "core/io/ConnectionErrorListener.h"
#include "core/io/ContainerListener.h"

#include "hesperia/data/dmcp/ModuleDescriptor.h"
#include "hesperia/dmcp/ModuleConfigurationProvider.h"
#include "hesperia/dmcp/ModuleStateListener.h"

namespace hesperia {
    namespace dmcp {
        namespace connection {

            class HESPERIA_API ModuleConnection : protected core::io::ConnectionErrorListener,
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
                    const hesperia::data::dmcp::ModuleDescriptor getModuleDescriptor() const;

                protected:
                    virtual void nextContainer(core::data::Container &c);
                    virtual void handleConnectionError();

                    core::SharedPointer<core::io::Connection> m_connection;
                    ModuleConfigurationProvider& m_configurationProvider;

                    core::base::Condition m_discriptorCondition;
                    hesperia::data::dmcp::ModuleDescriptor m_descriptor;
                    bool m_hasDescriptor;

                    ModuleStateListener* m_stateListener;
                    core::base::Mutex m_stateListenerMutex;
            };
        }
    }
} // hesperia::dmcp

#endif /*HESPERIA_DMCP_CONNECTION_MODULECONNECTION_H_*/
