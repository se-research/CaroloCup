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
#include "hesperia/data/recorder/RecorderCommand.h"

namespace hesperia {
    namespace data {
        namespace recorder {

            using namespace core::base;

            RecorderCommand::RecorderCommand() :
                    m_command(STOP) {}

            RecorderCommand::RecorderCommand(const RecorderCommand &obj) :
                m_command(obj.getCommand()) {}

            RecorderCommand::~RecorderCommand() {}

            RecorderCommand& RecorderCommand::operator=(const RecorderCommand &obj) {
                setCommand(obj.getCommand());

                return (*this);
            }

            RecorderCommand::COMMAND RecorderCommand::getCommand() const {
                return m_command;
            }

            void RecorderCommand::setCommand(const RecorderCommand::COMMAND &c) {
                m_command = c;
            }

            const string RecorderCommand::toString() const {
                stringstream sstr;
                sstr  << ((m_command == STOP) ? "Not r" : "R") << "ecording.";
                return sstr.str();
            }

            ostream& RecorderCommand::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL7('c', 'o', 'm', 'm', 'a', 'n', 'd') >::RESULT,
                        static_cast<uint32_t>(m_command));

                return out;
            }

            istream& RecorderCommand::operator>>(istream &in) {
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
} // hesperia::data::recorder
