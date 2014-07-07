/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "core/data/dmcp/ModuleExitCodeMessage.h"

namespace core {
    namespace data {
        namespace dmcp {

            using namespace core::base;

            ModuleExitCodeMessage::ModuleExitCodeMessage()
                    : m_me(ModuleState::UNDEFINED_EXITCODE) {}

            ModuleExitCodeMessage::ModuleExitCodeMessage(const ModuleState::MODULE_EXITCODE &me)
                    : m_me(me) {}

            ModuleExitCodeMessage::~ModuleExitCodeMessage() {}

            ostream&  ModuleExitCodeMessage::operator<<(ostream &out) const {
                SerializationFactory sf;
                Serializer &s = sf.getSerializer(out);

                uint32_t type = m_me;
                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT, type);

                return out;
            }

            istream& ModuleExitCodeMessage::operator>>(istream &in) {
                SerializationFactory sf;
                Deserializer &d = sf.getDeserializer(in);

                uint32_t type = 0;
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT, type);
                m_me = static_cast<ModuleState::MODULE_EXITCODE>(type);

                return in;
            }

            const string ModuleExitCodeMessage::toString() const {
                switch (m_me) {
                case ModuleState::OKAY:
                    return "MODULE_EXITCODE::OKAY";
                case ModuleState::EXCEPTION_CAUGHT:
                    return "MODULE_EXITCODE::EXCEPTION_CAUGHT";
                case ModuleState::SERIOUS_ERROR:
                    return "MODULE_EXITCODE::SERIOUS_ERROR";
                case ModuleState::CONNECTION_LOST:
                    return "MODULE_EXITCODE::CONNECTION_LOST";
                case ModuleState::UNDEFINED_EXITCODE:
                    return "MODULE_EXITCODE::UNDEFINED_EXITCODE";
                case ModuleState::NO_SUPERCOMPONENT:
                    return "MODULE_EXITCODE::NO_SUPERCOMPONENT";
                }
                return "";
            }

            ModuleState::MODULE_EXITCODE ModuleExitCodeMessage::getModuleExitCode() const {
                return m_me;
            }
        }
    }
}
