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
#include "hesperia/data/scenario/StraightLine.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;
            using namespace core::base;
            using namespace scenario;

            StraightLine::StraightLine() :
                    m_start(),
                    m_end() {
                LaneModel::setType(LaneModel::STRAIGHTLINE);
            }

            StraightLine::StraightLine(const StraightLine &obj) :
                    LaneModel(obj),
                    m_start(obj.getStart()),
                    m_end(obj.getEnd()) {}

            StraightLine::~StraightLine() {}

            StraightLine& StraightLine::operator=(const StraightLine &obj) {
                LaneModel::operator=(obj);
                setStart(obj.getStart());
                setEnd(obj.getEnd());

                return (*this);
            }

            void StraightLine::accept(ScenarioVisitor &visitor) {
                LaneModel::accept(visitor);
                visitor.visit(*this);

                m_start.accept(visitor);
                m_end.accept(visitor);
            }

            const IDVertex3& StraightLine::getStart() const {
                return m_start;
            }

            void StraightLine::setStart(const IDVertex3 &s) {
                m_start = s;
            }

            const IDVertex3& StraightLine::getEnd() const {
                return m_end;
            }

            void StraightLine::setEnd(const IDVertex3 &e) {
                m_end = e;
            }

            const string StraightLine::toString() const {
                return "";
            }

            ostream& StraightLine::operator<<(ostream &out) const {
                // Serializer super class.
                LaneModel::operator<<(out);

                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL5('s', 't', 'a', 'r', 't') >::RESULT,
                        getStart());

                s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL3('e', 'n', 'd') >::RESULT,
                        getEnd());

                return out;
            }

            istream& StraightLine::operator>>(istream &in) {
                // Deserializer super class.
                LaneModel::operator>>(in);

                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL5('s', 't', 'a', 'r', 't') >::RESULT,
                       m_start);

                d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL3('e', 'n', 'd') >::RESULT,
                       m_end);

                return in;
            }

        }
    }
} // hesperia::data::scenario
