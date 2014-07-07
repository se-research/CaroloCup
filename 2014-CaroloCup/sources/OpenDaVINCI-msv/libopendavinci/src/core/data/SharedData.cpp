/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "core/data/SharedData.h"

namespace core {
    namespace data {

        using namespace std;
        using namespace base;

        SharedData::SharedData() : m_name() {}

        SharedData::SharedData(const SharedData &obj) :
                SerializableData(),
                m_name(obj.m_name) {}

        SharedData::~SharedData() {}

        SharedData& SharedData::operator=(const SharedData &obj) {
            m_name = obj.m_name;

            return (*this);
        }

        const string SharedData::getName() const {
            return m_name;
        }

        void SharedData::setName(const string &name) {
            m_name = name;
        }

        const string SharedData::toString() const {
            stringstream s;
            s << m_name;
            return s.str();
        }

        ostream& SharedData::operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);

            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('n', 'a', 'm', 'e') >::RESULT,
                    m_name);

            return out;
        }

        istream& SharedData::operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);

            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('n', 'a', 'm', 'e') >::RESULT,
                   m_name);

            return in;
        }

    }
} // core::data
