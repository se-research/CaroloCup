/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include <sstream>

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "hesperia/data/environment/NamedLine.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;
            using namespace core::base;
            using namespace core::data::environment;

            NamedLine::NamedLine() :
                Line(),
                m_name("") {}

            NamedLine::NamedLine(const string &n, const Point3 &A, const Point3 &B) :
                Line(A, B),
                m_name(n) {}

            NamedLine::NamedLine(const NamedLine &obj) :
                Line(obj.getA(), obj.getB()),
                m_name(obj.getName()) {}

            NamedLine::~NamedLine() {}

            NamedLine& NamedLine::operator=(const NamedLine &obj) {
                setA(obj.getA());
                setB(obj.getB());
                setName(obj.getName());

                return (*this);
            }

            const string NamedLine::getName() const {
                return m_name;
            }

            void NamedLine::setName(const string &n) {
                m_name = n;
            }

            const string NamedLine::toString() const {
                stringstream sstr;
                sstr << m_name << ": " << Line::toString();
                return sstr.str();
            }

            ostream& NamedLine::operator<<(ostream &out) const {
                // Serializer super class.
                Line::operator<<(out);

                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('n','a','m','e') >::RESULT,
                        m_name);

                return out;
            }

            istream& NamedLine::operator>>(istream &in) {
                // Deserializer super class.
                Line::operator>>(in);

                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('n','a','m','e') >::RESULT,
                       m_name);

                return in;
            }

        }
    }
} // hesperia::data::environment
