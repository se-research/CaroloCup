/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"
#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/scenario/Connector.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;
            using namespace core::base;
            using namespace scenario;

            Connector::Connector() :
                    m_source(),
                    m_target() {}

            Connector::Connector(const Connector &obj) :
                    SerializableData(),
                    m_source(obj.getSource()),
                    m_target(obj.getTarget()) {}

            Connector::~Connector() {}

            Connector& Connector::operator=(const Connector &obj) {
                setSource(obj.getSource());
                setTarget(obj.getTarget());
                return (*this);
            }

            void Connector::accept(ScenarioVisitor &visitor) {
                visitor.visit(*this);

                m_source.accept(visitor);
                m_target.accept(visitor);
            }

            const PointID& Connector::getSource() const {
                return m_source;
            }

            void Connector::setSource(const PointID &sourceID) {
                m_source = sourceID;
            }

            const PointID& Connector::getTarget() const {
                return m_target;
            }

            void Connector::setTarget(const PointID &targetID) {
                m_target = targetID;
            }

            const string Connector::toString() const {
                stringstream s;
                s << "(" << getSource().toString() << ")" << " -> " << "(" << getTarget().toString() << ")";
                return s.str();
            }

            ostream& Connector::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('s', 'o', 'u', 'r', 'c', 'e') >::RESULT,
                        getSource());

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('t', 'a', 'r', 'g', 'e', 't') >::RESULT,
                        getTarget());

                return out;
            }

            istream& Connector::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('s', 'o', 'u', 'r', 'c', 'e') >::RESULT,
                       m_source);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('t', 'a', 'r', 'g', 'e', 't') >::RESULT,
                       m_target);

                return in;
            }

        }
    }
} // hesperia::data::scenario
