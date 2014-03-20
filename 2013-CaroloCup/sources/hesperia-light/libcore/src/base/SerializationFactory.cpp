/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"
#include "core/base/NetstringsSerializer.h"
#include "core/base/NetstringsDeserializer.h"
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
            // Plain netstrings are still not working!
//            Serializer *s = new NetstringsSerializer(out);
            m_listOfSerializers.push_back(SharedPointer<Serializer>(s));
            return *s;
        }

        Deserializer& SerializationFactory::getDeserializer(istream &in) const {
            Deserializer *d = new QueryableNetstringsDeserializer(in);
            // Plain netstrings are still not working!
//            Deserializer *d = new NetstringsDeserializer(in);
            m_listOfDeserializers.push_back(SharedPointer<Deserializer>(d));
            return *d;
        }

    }
} // core::base
