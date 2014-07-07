/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/situation/StopType.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;
            using namespace core::base;

            StopType::StopType() :
                    m_type(StopType::UNDEFINED) {}

            StopType::StopType(const StopType &obj) :
                    SerializableData(),
                    m_type(obj.getType()) {}

            StopType::~StopType() {}

            StopType& StopType::operator=(const StopType &obj) {
                setType(obj.getType());
                return (*this);
            }

            enum StopType::STOPTYPE StopType::getType() const {
                return m_type;
            }

            void StopType::setType(const enum StopType::STOPTYPE &type) {
                m_type = type;
            }

            const string StopType::toString() const {
                stringstream s;
                s << "Type: " << static_cast<uint32_t>(m_type);
                return s.str();
            }

            ostream& StopType::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
                        static_cast<uint32_t>(m_type));

                return out;
            }

            istream& StopType::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                uint32_t type = 0;
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'y', 'p', 'e') >::RESULT,
                       type);

                m_type = static_cast<enum StopType::STOPTYPE>(type);

                return in;
            }

        }
    }
} // hesperia::data::situation
