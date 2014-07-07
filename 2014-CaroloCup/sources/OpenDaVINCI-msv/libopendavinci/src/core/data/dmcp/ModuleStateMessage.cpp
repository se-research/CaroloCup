/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "core/data/dmcp/ModuleStateMessage.h"

namespace core {
    namespace data {
        namespace dmcp {

            using namespace core::base;

            ModuleStateMessage::ModuleStateMessage()
                    : m_ms(ModuleState::UNDEFINED_STATE) {}

            ModuleStateMessage::ModuleStateMessage(const ModuleState::MODULE_STATE &ms)
                    : m_ms(ms) {}

            ModuleStateMessage::~ModuleStateMessage() {}

            ostream&  ModuleStateMessage::operator<<(ostream &out) const {
                SerializationFactory sf;
                Serializer &s = sf.getSerializer(out);

                uint32_t type = m_ms;
                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT, type);

                return out;
            }

            istream& ModuleStateMessage::operator>>(istream &in) {
                SerializationFactory sf;
                Deserializer &d = sf.getDeserializer(in);

                uint32_t type = 0;
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT, type);
                m_ms = static_cast<ModuleState::MODULE_STATE>(type);

                return in;
            }

            const string ModuleStateMessage::toString() const {
                switch (m_ms) {
                case ModuleState::NOT_RUNNING:
                    return "MODULE_STATE::NOT_RUNNING";
                case ModuleState::RUNNING:
                    return "MODULE_STATE::RUNNING";
                case ModuleState::UNDEFINED_STATE:
                    return "MODULE_STATE::UNDEFINED_STATE";
                }
                return "";
            }

            ModuleState::MODULE_STATE ModuleStateMessage::getModuleState() const {
                return m_ms;
            }
        }
    }
}
