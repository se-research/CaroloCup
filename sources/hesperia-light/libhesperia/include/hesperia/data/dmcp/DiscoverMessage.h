/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_DMCP_DISCOVERMESSAGE_H_
#define HESPERIA_DATA_DMCP_DISCOVERMESSAGE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "hesperia/dmcp/ServerInformation.h"

namespace hesperia {
    namespace data {
        namespace dmcp {

            using namespace std;

            class HESPERIA_API DiscoverMessage : public core::data::SerializableData {
                public:
                    enum TYPE {
                        UNDEFINED,
                        DISCOVER,
                        RESPONSE
                    };

                    DiscoverMessage();
                    DiscoverMessage(TYPE type);
                    DiscoverMessage(TYPE type, const hesperia::dmcp::ServerInformation& serverInformation);

                    virtual ~DiscoverMessage();

                    TYPE getType() const;
                    const hesperia::dmcp::ServerInformation getServerInformation() const;

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    TYPE m_type;
                    hesperia::dmcp::ServerInformation m_serverInformation;
            };
        }
    }
} // hesperia::data::dmcp

#endif /*HESPERIA_DATA_DMCP_DISCOVERMESSAGE_H_*/
