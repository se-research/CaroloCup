/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "core/data/Configuration.h"

namespace core {
    namespace data {

        using namespace std;
        using namespace core::base;

        Configuration::Configuration() : m_keyValueConfiguration() {}

        Configuration::Configuration(const KeyValueConfiguration &keyValueConfiguration) :
                m_keyValueConfiguration(keyValueConfiguration) {}

        Configuration::Configuration(const Configuration &obj) :
                SerializableData(),
                m_keyValueConfiguration(obj.m_keyValueConfiguration) {}

        Configuration::~Configuration() {}

        Configuration& Configuration::operator=(const Configuration &obj) {
            m_keyValueConfiguration = obj.m_keyValueConfiguration;
            return (*this);
        }

        const KeyValueConfiguration Configuration::getKeyValueConfiguration() const {
            return m_keyValueConfiguration;
        }

        const string Configuration::toString() const {
            stringstream s;
            s << m_keyValueConfiguration;
            return s.str();
        }

        ostream& Configuration::operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);

            const string configuration = toString();
            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('c', 'o', 'n', 'f', 'i', 'g') >::RESULT,
                    configuration);

            return out;
        }

        istream& Configuration::operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);

            string configuration;
            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('c', 'o', 'n', 'f', 'i', 'g') >::RESULT,
                   configuration);

            stringstream stringstreamConfiguration(configuration);
            stringstreamConfiguration >> m_keyValueConfiguration;

            return in;
        }

    }
} // core::data
