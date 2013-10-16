/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/situation/WarpToStart.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;
            using namespace core::base;

            WarpToStart::WarpToStart() :
                    StopType() {
                setType(StopType::WARPTOSTART);
            }

            WarpToStart::WarpToStart(const WarpToStart &obj) :
                    StopType(obj) {}

            WarpToStart::~WarpToStart() {}

            WarpToStart& WarpToStart::operator=(const WarpToStart &obj) {
                StopType::operator=(obj);
                return (*this);
            }

            void WarpToStart::accept(SituationVisitor &visitor) {
                visitor.visit(*this);
            }

            const string WarpToStart::toString() const {
                stringstream s;
                s << getType();
                return s.str();
            }

            ostream& WarpToStart::operator<<(ostream &out) const {
                // Serializer super class.
                StopType::operator<<(out);

//
//                SerializationFactory sf;
//
//                Serializer &s = sf.getSerializer(out);
//
//                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
//                        static_cast<uint32_t>(m_type));

                return out;
            }

            istream& WarpToStart::operator>>(istream &in) {
                // Deserializer super class.
                StopType::operator>>(in);

//                SerializationFactory sf;
//
//                Deserializer &d = sf.getDeserializer(in);
//
//                uint32_t type = 0;
//                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
//                       type);
//
//                m_type = static_cast<enum Stop::BEHAVIORTYPE>(type);

                return in;
            }

        }
    }
} // hesperia::data::situation
