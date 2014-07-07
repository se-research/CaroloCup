/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/scenario/IDVertex3.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;
            using namespace core::base;

            IDVertex3::IDVertex3() :
                    Vertex3(),
                    m_id(0) {}

            IDVertex3::IDVertex3(const IDVertex3 &obj) :
                    Vertex3(obj),
                    m_id(obj.getID()) {}

            IDVertex3::~IDVertex3() {}

            IDVertex3& IDVertex3::operator=(const IDVertex3 &obj) {
                Vertex3::operator=(obj);
                setID(obj.getID());

                return (*this);
            }

            void IDVertex3::accept(ScenarioVisitor &visitor) {
                Vertex3::accept(visitor);
                visitor.visit(*this);
            }

            uint32_t IDVertex3::getID() const {
                return m_id;
            }

            void IDVertex3::setID(const uint32_t &id) {
                m_id = id;
            }

            const string IDVertex3::toString() const {
                stringstream s;
                s << "ID: " << getID() << ": " << Vertex3::toString();
                return s.str();
            }

            ostream& IDVertex3::operator<<(ostream &out) const {
                // Serializer super class.
                Vertex3::operator<<(out);

                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('i', 'd') >::RESULT,
                        getID());

                return out;
            }

            istream& IDVertex3::operator>>(istream &in) {
                // Deserializer super class.
                Vertex3::operator>>(in);

                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('i', 'd') >::RESULT,
                       m_id);

                return in;
            }

        }
    }
} // hesperia::data::scenario
