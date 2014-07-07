/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/QueryableNetstringsSerializer.h"
#include "core/base/QueryableNetstringsDeserializer.h"
#include "core/base/SerializationFactory.h"

namespace core {
    namespace base {

        using namespace std;

        SerializationFactory::SerializationFactory() :
                m_listOfSerializers(),
                m_listOfDeserializers() {}

        SerializationFactory::~SerializationFactory() {
            m_listOfSerializers.clear();
            m_listOfDeserializers.clear();
        }

        Serializer& SerializationFactory::getSerializer(ostream &out) const {
            Serializer *s = new QueryableNetstringsSerializer(out);
            m_listOfSerializers.push_back(SharedPointer<Serializer>(s));
            return *s;
        }

        Deserializer& SerializationFactory::getDeserializer(istream &in) const {
            Deserializer *d = new QueryableNetstringsDeserializer(in);
            m_listOfDeserializers.push_back(SharedPointer<Deserializer>(d));
            return *d;
        }

    }
} // core::base
