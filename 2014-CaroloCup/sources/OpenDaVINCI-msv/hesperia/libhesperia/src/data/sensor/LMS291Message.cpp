/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "hesperia/data/sensor/LMS291Message.h"

namespace hesperia {
    namespace data {
        namespace sensor {

            using namespace std;
            using namespace core::base;

            LMS291Message::LMS291Message() :
                    SerializableData(),
                    m_id(),
                    m_message() {}

            LMS291Message::LMS291Message(const LMS291Message &obj) :
                    m_id(obj.getID()),
                    m_message(obj.getMessage()) {}

            LMS291Message::~LMS291Message() {}

            LMS291Message& LMS291Message::operator=(const LMS291Message &obj) {
                setID(obj.getID());
                setMessage(obj.getMessage());

                return (*this);
            }

            const string LMS291Message::getID() const {
                return m_id;
            }

            void LMS291Message::setID(const string &id) {
                m_id = id;
            }

            const string LMS291Message::getMessage() const {
                return m_message;
            }

            void LMS291Message::setMessage(const string &msg) {
                m_message = msg;
            }

            const string LMS291Message::toString() const {
                return m_message;
            }

            ostream& LMS291Message::operator<<(ostream &out) const {
                // Serialize this class.
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('i', 'd') >::RESULT,
                        m_id);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('m', 'e', 's', 's', 'a', 'g', 'e') >::RESULT,
                        m_message);

                return out;
            }

            istream& LMS291Message::operator>>(istream &in) {
                // Deserialize this class.
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('i', 'd') >::RESULT,
                        m_id);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('m', 'e', 's', 's', 'a', 'g', 'e') >::RESULT,
                       m_message);

                return in;
            }

        }
    }
} // hesperia::data::sensor
