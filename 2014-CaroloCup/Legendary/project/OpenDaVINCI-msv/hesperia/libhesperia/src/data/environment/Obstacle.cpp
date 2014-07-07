/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "hesperia/data/environment/Obstacle.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;
            using namespace core::base;
            using namespace core::data::environment;

            Obstacle::Obstacle() :
                    PointShapedObject(Point3(0, 0, 0), Point3(0, 0, 0), Point3(0, 0, 0), Point3(0, 0, 0)),
                    m_id(0),
                    m_state(Obstacle::REMOVE),
                    m_classification(Obstacle::UNKNOWN),
                    m_polygon() {}

            Obstacle::Obstacle(const uint32_t &id, const enum Obstacle::STATE &state) :
                   PointShapedObject(Point3(0, 0, 0), Point3(0, 0, 0), Point3(0, 0, 0), Point3(0, 0, 0)),
                   m_id(id),
                   m_state(state),
                   m_classification(Obstacle::UNKNOWN),
                   m_polygon() {}

            Obstacle::Obstacle(const uint32_t &id, const enum Obstacle::STATE &state, const enum Obstacle::CLASSIFICATION &classification,
                               const Point3 &position, const Point3 &rotation,
                               const Point3 &velocity, const Point3 &acceleration,
                               const Polygon &polygon) :
                   PointShapedObject(position, rotation, velocity, acceleration),
                   m_id(id),
                   m_state(state),
                   m_classification(classification),
                   m_polygon(polygon) {}

            Obstacle::Obstacle(const Obstacle &obj) :
                    PointShapedObject(obj),
                    m_id(obj.m_id),
                    m_state(obj.m_state),
                    m_classification(obj.m_classification),
                    m_polygon(obj.m_polygon) {}

            Obstacle::~Obstacle() {}

            Obstacle& Obstacle::operator=(const Obstacle &obj) {
                PointShapedObject::operator=(obj);

                m_id = obj.m_id;
                m_state = obj.m_state;
                m_classification = obj.m_classification;
                m_polygon = obj.m_polygon;

                return (*this);
            }

            uint32_t Obstacle::getID() const {
                return m_id;
            }

            void Obstacle::setID(const uint32_t &id) {
                m_id = id;
            }

            enum Obstacle::STATE Obstacle::getState() const {
                return m_state;
            }

            void Obstacle::setState(const enum Obstacle::STATE &s) {
                m_state = s;
            }

            enum Obstacle::CLASSIFICATION Obstacle::getClassification() const {
                return m_classification;
            }

            void Obstacle::setClassification(const enum Obstacle::CLASSIFICATION &c) {
                m_classification = c;
            }

            void Obstacle::setPolygon(const Polygon &p) {
                m_polygon = p;
            }

            const Polygon Obstacle::getPolygon() const {
                return m_polygon;
            }

            const string Obstacle::toString() const {
                stringstream s;
                s << PointShapedObject::toString() << "/" << m_polygon.toString() << ", state: " << m_state << ", classification: " << m_classification;
                return s.str();
            }

            ostream& Obstacle::operator<<(ostream &out) const {
                // Serializer super class.
                PointShapedObject::operator<<(out);

                // Serialize this class.
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('i', 'd') >::RESULT,
                        static_cast<uint32_t>(m_id));

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('s', 't', 'a', 't', 'e') >::RESULT,
                        static_cast<uint32_t>(m_state));

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('c', 'l', 'a', 's', 's') >::RESULT,
                        static_cast<uint32_t>(m_classification));

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('p', 'o', 'l', 'y', 'g', 'o', 'n') >::RESULT,
                        m_polygon);

                return out;
            }

            istream& Obstacle::operator>>(istream &in) {
                // Deserializer super class.
                PointShapedObject::operator>>(in);

                // Deserialize this class.
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('i', 'd') >::RESULT,
                       m_id);

                uint32_t v = 0;
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('s', 't', 'a', 't', 'e') >::RESULT,
                       v);
                m_state = static_cast<Obstacle::STATE>(v);

                v = 0;
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('c', 'l', 'a', 's', 's') >::RESULT,
                       v);
                m_classification = static_cast<Obstacle::CLASSIFICATION>(v);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('p', 'o', 'l', 'y', 'g', 'o', 'n') >::RESULT,
                       m_polygon);

                return in;
            }

        }
    }
} // hesperia::data::environment
