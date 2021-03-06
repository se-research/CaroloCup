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

#include "hesperia/data/scenario/Clothoid.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;
            using namespace core::base;
            using namespace scenario;

            Clothoid::Clothoid() :
                    StraightLine(),
                    m_dk(0),
                    m_k(0),
                    m_rotationZ(0) {
                LaneModel::setType(LaneModel::CLOTHOID);
            }

            Clothoid::Clothoid(const Clothoid &obj) :
                    StraightLine(obj),
                    m_dk(obj.getDK()),
                    m_k(obj.getK()),
                    m_rotationZ(obj.getRotationZ()) {}

            Clothoid::~Clothoid() {}

            Clothoid& Clothoid::operator=(const Clothoid &obj) {
                LaneModel::operator=(obj);
                setDK(obj.getDK());
                setK(obj.getK());
                setRotationZ(obj.getRotationZ());

                return (*this);
            }

            void Clothoid::accept(ScenarioVisitor &visitor) {
                StraightLine::accept(visitor);
                visitor.visit(*this);
            }

            double Clothoid::getDK() const {
                return m_dk;
            }

            void Clothoid::setDK(const double &dk) {
                m_dk = dk;
            }

            double Clothoid::getK() const {
                return m_k;
            }

            void Clothoid::setK(const double &k) {
                m_k = k;
            }

            double Clothoid::getRotationZ() const {
                return m_rotationZ;
            }

            void Clothoid::setRotationZ(const double &rotationZ) {
                m_rotationZ = rotationZ;
            }

            const string Clothoid::toString() const {
                return "";
            }

            ostream& Clothoid::operator<<(ostream &out) const {
                // Serializer super class.
                StraightLine::operator<<(out);

                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('d', 'k') >::RESULT,
                        getDK());

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL1('k') >::RESULT,
                        getK());

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('r', 'o', 't', 'z') >::RESULT,
                        getRotationZ());

                return out;
            }

            istream& Clothoid::operator>>(istream &in) {
                // Deserializer super class.
                StraightLine::operator>>(in);

                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('d', 'k') >::RESULT,
                       m_dk);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL1('k') >::RESULT,
                       m_k);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('r', 'o', 't', 'z') >::RESULT,
                       m_rotationZ);

                return in;
            }

        }
    }
} // hesperia::data::scenario
