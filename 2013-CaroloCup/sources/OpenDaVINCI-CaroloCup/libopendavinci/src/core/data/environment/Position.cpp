/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "core/data/environment/Position.h"

namespace core {
    namespace data {
        namespace environment {

            using namespace std;
            using namespace core::base;

            Position::Position() : m_position(), m_rotation() {}

            Position::Position(const Point3 &position, const Point3 &rotation) :
                    m_position(position),
                    m_rotation(rotation) {}

            Position::Position(const Position &obj) :
		SerializableData(),
                m_position(obj.getPosition()),
                m_rotation(obj.getRotation()) {}

            Position::~Position() {}

            Position& Position::operator=(const Position &obj) {
                setPosition(obj.getPosition());
                setRotation(obj.getRotation());
                return (*this);
            }

            const Point3 Position::getPosition() const {
                return m_position;
            }

            void Position::setPosition(const Point3 &position) {
                m_position = position;
            }

            const Point3 Position::getRotation() const {
                return m_rotation;
            }

            void Position::setRotation(const Point3 &rotation) {
                m_rotation = rotation;
            }

            bool Position::operator==(const Position& other) const
            {
                return (m_position == other.m_position) && (m_rotation == other.m_rotation);
            }

            bool Position::operator!=(const Position& other) const
            {
                return !operator==(other);
            }

            const string Position::toString() const {
                stringstream sstr;
                sstr  << m_position.toString() << "/" << m_rotation.toString();
                return sstr.str();
            }

            ostream& Position::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('p', 'o', 's', 'i', 't', 'i', 'o', 'n') >::RESULT,
                        m_position);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('r', 'o', 't', 'a', 't', 'i', 'o', 'n') >::RESULT,
                        m_rotation);

                return out;
            }

            istream& Position::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('p', 'o', 's', 'i', 't', 'i', 'o', 'n') >::RESULT,
                       m_position);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('r', 'o', 't', 'a', 't', 'i', 'o', 'n') >::RESULT,
                       m_rotation);

                return in;
            }

        }
    }
} // core::data::environment
