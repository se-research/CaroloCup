/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "core/data/dmcp/PulseMessage.h"

namespace core {
    namespace data {
        namespace dmcp {

            using namespace core::base;
            using namespace core::data;

            PulseMessage::PulseMessage() :
                m_realTimeFromSupercomponent(),
                m_nominalTimeSlice(0),
                m_cumulatedTimeSlice(0) {}

            PulseMessage::PulseMessage(const core::data::TimeStamp &ts, const uint32_t &nominalTimeSlice, const uint32_t &cumulatedTimeSlice) :
                m_realTimeFromSupercomponent(ts),
                m_nominalTimeSlice(nominalTimeSlice),
                m_cumulatedTimeSlice(cumulatedTimeSlice) {}

            PulseMessage::PulseMessage(const PulseMessage &obj) :
                m_realTimeFromSupercomponent(obj.m_realTimeFromSupercomponent),
                m_nominalTimeSlice(obj.m_nominalTimeSlice),
                m_cumulatedTimeSlice(obj.m_cumulatedTimeSlice) {}

            PulseMessage& PulseMessage::operator=(const PulseMessage &obj) {
                m_realTimeFromSupercomponent = obj.m_realTimeFromSupercomponent;
                m_nominalTimeSlice = obj.m_nominalTimeSlice;
                m_cumulatedTimeSlice = obj.m_cumulatedTimeSlice;

                return *this;
            }

            PulseMessage::~PulseMessage() {}

            void PulseMessage::setNominalTimeSlice(const uint32_t &nts) {
                m_nominalTimeSlice = nts;
            }

            uint32_t PulseMessage::getNominalTimeSlice() const {
                return m_nominalTimeSlice;
            }

            void PulseMessage::setCumulatedTimeSlice(const uint32_t &cts) {
                m_cumulatedTimeSlice = cts;
            }

            uint32_t PulseMessage::getCumulatedTimeSlice() const {
                return m_cumulatedTimeSlice;
            }

            void PulseMessage::setRealTimeFromSupercomponent(const TimeStamp &ts) {
                m_realTimeFromSupercomponent = ts;
            }

            const TimeStamp PulseMessage::getRealtimeFromSupercomponent() const {
                return m_realTimeFromSupercomponent;
            }

            const string PulseMessage::toString() const {
                stringstream sstr;
                sstr << "Real time from supercomponent: " << m_realTimeFromSupercomponent.toString() << ", " << "nominal time slice: " << m_nominalTimeSlice << ", " << "cumulated time slice: " << m_cumulatedTimeSlice;
                return sstr.str();
            }

            ostream& PulseMessage::operator<<(ostream &out) const {
                SerializationFactory sf;
                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('r', 't', 's', 'c') >::RESULT, m_realTimeFromSupercomponent);
                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('n', 'o', 'm', 'i', 'n', 'a', 'l') >::RESULT, m_nominalTimeSlice);
                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('c', 'u', 'm', 'u', 'l', 'a', 't', 'd') >::RESULT, m_cumulatedTimeSlice);

                return out;
            }

            istream& PulseMessage::operator>>(istream &in) {
                SerializationFactory sf;
                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('r', 't', 's', 'c') >::RESULT, m_realTimeFromSupercomponent);
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('n', 'o', 'm', 'i', 'n', 'a', 'l') >::RESULT, m_nominalTimeSlice);
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('c', 'u', 'm', 'u', 'l', 'a', 't', 'd') >::RESULT, m_cumulatedTimeSlice);

                return in;
            }

        }
    }
}
