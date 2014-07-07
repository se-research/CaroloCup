/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/scenario/CoordinateSystem.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;
            using namespace core::base;

            CoordinateSystem::CoordinateSystem() :
                    m_type(""),
                    m_rotation(0) {}

            CoordinateSystem::CoordinateSystem(const CoordinateSystem &obj) :
                    SerializableData(),
                    m_type(obj.m_type),
                    m_rotation(obj.m_rotation) {}

            CoordinateSystem::~CoordinateSystem() {}

            CoordinateSystem& CoordinateSystem::operator=(const CoordinateSystem &obj) {
                setType(obj.getType());
                setRotation(obj.getRotation());
                return (*this);
            }

            const string CoordinateSystem::getType() const {
                return m_type;
            }

            void CoordinateSystem::setType(const string &type) {
                m_type = type;
            }

            double CoordinateSystem::getRotation() const {
                return m_rotation;
            }

            void CoordinateSystem::setRotation(const double &r) {
                m_rotation = r;
            }

            const string CoordinateSystem::toString() const {
                stringstream s;
                s << "Type: '" << m_type << "', Rotation: " << m_rotation;
                return s.str();
            }

            ostream& CoordinateSystem::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
                        m_type);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('r', 'o', 't', 'a', 't', 'i', 'o', 'n') >::RESULT,
                        m_rotation);

                return out;
            }

            istream& CoordinateSystem::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
                       m_type);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('r', 'o', 't', 'a', 't', 'i', 'o', 'n') >::RESULT,
                       m_rotation);

                return in;
            }

        }
    }
} // hesperia::data::scenario
