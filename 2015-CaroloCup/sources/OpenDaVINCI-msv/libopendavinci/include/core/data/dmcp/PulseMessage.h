/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DATA_DMCP_PULSEMESSAGE_H_
#define OPENDAVINCI_DATA_DMCP_PULSEMESSAGE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"
#include "core/data/TimeStamp.h"
#include "core/base/ModuleState.h"

namespace core {
    namespace data {
        namespace dmcp {

            using namespace std;

            class OPENDAVINCI_API PulseMessage : public core::data::SerializableData {
                public:
                    PulseMessage();
                    PulseMessage(const core::data::TimeStamp &ts, const uint32_t &nominalTimeSlice, const uint32_t &cumulatedTimeSlice);
                    PulseMessage(const PulseMessage &obj);
                    PulseMessage& operator=(const PulseMessage &obj);

                    virtual ~PulseMessage();

                    void setNominalTimeSlice(const uint32_t &nts);
                    uint32_t getNominalTimeSlice() const;

                    void setCumulatedTimeSlice(const uint32_t &cts);
                    uint32_t getCumulatedTimeSlice() const;

                    void setRealTimeFromSupercomponent(const core::data::TimeStamp &ts);
                    const core::data::TimeStamp getRealtimeFromSupercomponent() const;

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    core::data::TimeStamp m_realTimeFromSupercomponent;
                    uint32_t m_nominalTimeSlice;
                    uint32_t m_cumulatedTimeSlice;
            };
        }
    }
} // core::data::dmcp

#endif /*OPENDAVINCI_DATA_DMCP_PULSEMESSAGE_H_*/
