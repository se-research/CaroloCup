/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/situation/Immediately.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;
            using namespace core::base;

            Immediately::Immediately() :
                    StartType() {
                setType(StartType::IMMEDIATELY);
            }

            Immediately::Immediately(const Immediately &obj) :
                    StartType(obj) {}

            Immediately::~Immediately() {}

            Immediately& Immediately::operator=(const Immediately &obj) {
                StartType::operator=(obj);
                return (*this);
            }

            void Immediately::accept(SituationVisitor &visitor) {
                visitor.visit(*this);
            }

            const string Immediately::toString() const {
                stringstream s;
                s << getType();
                return s.str();
            }

            ostream& Immediately::operator<<(ostream &out) const {
                // Serializer super class.
                StartType::operator<<(out);

//
//                SerializationFactory sf;
//
//                Serializer &s = sf.getSerializer(out);
//
//                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
//                        static_cast<uint32_t>(m_type));

                return out;
            }

            istream& Immediately::operator>>(istream &in) {
                // Deserializer super class.
                StartType::operator>>(in);

//                SerializationFactory sf;
//
//                Deserializer &d = sf.getDeserializer(in);
//
//                uint32_t type = 0;
//                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
//                       type);
//
//                m_type = static_cast<enum Immediately::BEHAVIORTYPE>(type);

                return in;
            }

        }
    }
} // hesperia::data::situation
