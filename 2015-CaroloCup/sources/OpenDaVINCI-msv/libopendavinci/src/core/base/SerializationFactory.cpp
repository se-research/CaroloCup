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
            Serializer *s = NULL;
            if (m_listOfSerializers.empty()) {
                s = new QueryableNetstringsSerializer(out);
                m_listOfSerializers.push_back(SharedPointer<Serializer>(s));
            }
            else {
                s = &(*(*(m_listOfSerializers.begin()))); // The innermost * dereferences the iterator to SharedPointer<Serializer>, the second * returns the Serializer from within the SharedPointer, and the & turns it into a regular pointer.
            }
            return *s;
        }

        Deserializer& SerializationFactory::getDeserializer(istream &in) const {
            Deserializer *d = NULL;
            if (m_listOfDeserializers.empty()) {
                d = new QueryableNetstringsDeserializer(in);
                m_listOfDeserializers.push_back(SharedPointer<Deserializer>(d)); // The innermost * dereferences the iterator to SharedPointer<Deserializer>, the second * returns the Deserializer from within the SharedPointer, and the & turns it into a regular pointer.
            }
            else {
                d = &(*(*(m_listOfDeserializers.begin())));
            }
            return *d;
        }

    }
} // core::base
