/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/scenario/Shape.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;
            using namespace core::base;

            Shape::Shape() :
                    m_name(""),
                    m_type(Shape::POLYGON) {}

            Shape::Shape(const Shape &obj) :
                    SerializableData(),
                    m_name(obj.m_name),
                    m_type(obj.m_type) {}

            Shape::~Shape() {}

            Shape& Shape::operator=(const Shape &obj) {
                setName(obj.getName());
                setType(obj.getType());
                return (*this);
            }

            const string Shape::getName() const {
                return m_name;
            }

            void Shape::setName(const string &name) {
                m_name = name;
            }

            enum Shape::SHAPETYPE Shape::getType() const {
                return m_type;
            }

            void Shape::setType(const enum Shape::SHAPETYPE &type) {
                m_type = type;
            }

            const string Shape::toString() const {
                stringstream s;
                s << "Name: '" << m_name << "', Type: " << static_cast<uint32_t>(m_type);
                return s.str();
            }

            ostream& Shape::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('n', 'a', 'm', 'e') >::RESULT,
                        m_name);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
                        static_cast<uint32_t>(m_type));

                return out;
            }

            istream& Shape::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('n', 'a', 'm', 'e') >::RESULT,
                       m_name);

                uint32_t type = 0;
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
                       type);

                m_type = static_cast<enum Shape::SHAPETYPE>(type);

                return in;
            }

        }
    }
} // hesperia::data::scenario
