/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "core/data/StringData.h"

namespace core {
    namespace data {

        using namespace std;
        using namespace base;

        StringData::StringData() :
                m_data("") {}

        StringData::StringData(const StringData &obj) :
                SerializableData(),
                m_data(obj.m_data) {}

        StringData::~StringData() {}

        StringData& StringData::operator=(const StringData &obj) {
            m_data = obj.m_data;
            return (*this);
        }

        const string StringData::getData() const {
            return m_data;
        }

        void StringData::setData(const string &data) {
            m_data = data;
        }

        const string StringData::toString() const {
            stringstream s;
            s << "'" << m_data << "'";
            return s.str();
        }

        ostream& StringData::operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);

            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'a', 't', 'a') >::RESULT,
                    m_data);

            return out;
        }

        istream& StringData::operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);

            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'a', 't', 'a') >::RESULT,
                   m_data);

            return in;
        }

    }
} // core::data
