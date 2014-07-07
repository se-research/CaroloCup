/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/situation/Behavior.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;
            using namespace core::base;

            Behavior::Behavior() :
                    m_type(Behavior::UNDEFINED) {}

            Behavior::Behavior(const Behavior &obj) :
                    SerializableData(),
                    m_type(obj.getType()) {}

            Behavior::~Behavior() {}

            Behavior& Behavior::operator=(const Behavior &obj) {
                setType(obj.getType());
                return (*this);
            }

            enum Behavior::BEHAVIORTYPE Behavior::getType() const {
                return m_type;
            }

            void Behavior::setType(const enum Behavior::BEHAVIORTYPE &type) {
                m_type = type;
            }

            const string Behavior::toString() const {
                stringstream s;
                s << "Type: " << static_cast<uint32_t>(m_type);
                return s.str();
            }

            ostream& Behavior::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
                        static_cast<uint32_t>(m_type));

                return out;
            }

            istream& Behavior::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                uint32_t type = 0;
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
                       type);

                m_type = static_cast<enum Behavior::BEHAVIORTYPE>(type);

                return in;
            }

        }
    }
} // hesperia::data::situation
