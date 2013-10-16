/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DATA_DMCP_DISCOVERMESSAGE_H_
#define OPENDAVINCI_DATA_DMCP_DISCOVERMESSAGE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"
#include "core/dmcp/ServerInformation.h"

namespace core {
    namespace data {
        namespace dmcp {

            using namespace std;

            class OPENDAVINCI_API DiscoverMessage : public core::data::SerializableData {
                public:
                    enum TYPE {
                        UNDEFINED,
                        DISCOVER,
                        RESPONSE
                    };

                    DiscoverMessage();
                    DiscoverMessage(TYPE type);
                    DiscoverMessage(TYPE type, const core::dmcp::ServerInformation& serverInformation);

                    virtual ~DiscoverMessage();

                    TYPE getType() const;
                    const core::dmcp::ServerInformation getServerInformation() const;

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    TYPE m_type;
                    core::dmcp::ServerInformation m_serverInformation;
            };
        }
    }
} // core::data::dmcp

#endif /*OPENDAVINCI_DATA_DMCP_DISCOVERMESSAGE_H_*/
