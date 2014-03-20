/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <sstream>

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/player/PlayerCommand.h"

namespace hesperia {
    namespace data {
        namespace player {

            using namespace core::base;

            PlayerCommand::PlayerCommand() :
                    m_command(PAUSE) {}

            PlayerCommand::PlayerCommand(const PlayerCommand &obj) :
                m_command(obj.getCommand()) {}

            PlayerCommand::~PlayerCommand() {}

            PlayerCommand& PlayerCommand::operator=(const PlayerCommand &obj) {
                setCommand(obj.getCommand());

                return (*this);
            }

            PlayerCommand::COMMAND PlayerCommand::getCommand() const {
                return m_command;
            }

            void PlayerCommand::setCommand(const PlayerCommand::COMMAND &c) {
                m_command = c;
            }

            const string PlayerCommand::toString() const {
                stringstream sstr;
                switch (m_command) {
                case PLAY:
                    sstr << "Play.";
                    break;
                case PAUSE:
                    sstr << "Pause.";
                    break;
                case REWIND:
                    sstr << "Rewind.";
                    break;
                case STEP_FORWARD:
                    sstr << "Step forward.";
                    break;
                }
                return sstr.str();
            }

            ostream& PlayerCommand::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL7('c', 'o', 'm', 'm', 'a', 'n', 'd') >::RESULT,
                        static_cast<uint32_t>(m_command));

                return out;
            }

            istream& PlayerCommand::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                uint32_t command = 0;
                d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL7('c', 'o', 'm', 'm', 'a', 'n', 'd') >::RESULT,
                       command);
                m_command = static_cast<COMMAND>(command);

                return in;
            }

        }
    }
} // hesperia::data::player
