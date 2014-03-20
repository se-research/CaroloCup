/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_SERIALIZATIONFACTORY_H_
#define HESPERIA_CORE_BASE_SERIALIZATIONFACTORY_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/SharedPointer.h"
#include "core/base/Serializer.h"
#include "core/base/Deserializer.h"
#include "core/exceptions/Exceptions.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This class is the factory for providing serializers and
         * deserializers.
         *
         * @See Serializable
         */
        class HESPERIA_API SerializationFactory {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SerializationFactory(const SerializationFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SerializationFactory& operator=(const SerializationFactory &);

            public:
                SerializationFactory();

                virtual ~SerializationFactory();

                /**
                 * This method returns a serializer.
                 *
                 * @param out Output stream for serialization.
                 * @return Serializer.
                 */
                Serializer& getSerializer(ostream &out) const;

                /**
                 * This method returns a deserializer.
                 *
                 * @param in Input stream for deserialization.
                 * @return Deserializer.
                 */
                Deserializer& getDeserializer(istream &in) const;

            private:
                mutable vector<SharedPointer<Serializer> > m_listOfSerializers;
                mutable vector<SharedPointer<Deserializer> > m_listOfDeserializers;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_SERIALIZATIONFACTORY_H_*/
