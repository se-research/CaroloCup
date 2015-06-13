/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_SERIALIZATIONFACTORY_H_
#define OPENDAVINCI_CORE_BASE_SERIALIZATIONFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/SharedPointer.h"
#include "core/base/Serializer.h"
#include "core/base/Deserializer.h"
#include "core/exceptions/Exceptions.h"

//Thesis implementation
#include "core/base/LCMSerializer.h"
#include "core/base/LCMDeserializer.h"
#include "core/base/PROTOSerializer.h"
#include "core/base/PROTODeserializer.h"
#include "core/base/ROSSerializer.h"
#include "core/base/ROSDeserializer.h"


namespace core {
    namespace base {

        using namespace std;

        /**
         * This class is the factory for providing serializers and
         * deserializers.
         *
         * @See Serializable
         */
        class OPENDAVINCI_API SerializationFactory {
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


                //Thesis implementation
                LCMSerializer& getLCMSerializer(ostream &out) const;
                LCMDeserializer& getLCMDeserializer(istream &in) const;
                PROTOSerializer& getPROTOSerializer(ostream &out) const;
                PROTODeserializer& getPROTODeserializer(istream &in) const;
                ROSSerializer& getROSSerializer(ostream &out) const;
                ROSDeserializer& getROSDeserializer(istream &in) const;

            private:
                mutable vector<SharedPointer<Serializer> > m_listOfSerializers;
                mutable vector<SharedPointer<Deserializer> > m_listOfDeserializers;
                //Thesis implementation
                mutable vector<SharedPointer<LCMSerializer> > m_listOfLCMSerializers;
                mutable vector<SharedPointer<LCMDeserializer> > m_listOfLCMDeserializers;
                mutable vector<SharedPointer<PROTOSerializer> > m_listOfPROTOSerializers;
                mutable vector<SharedPointer<PROTODeserializer> > m_listOfPROTODeserializers;
                mutable vector<SharedPointer<ROSSerializer> > m_listOfROSSerializers;
                mutable vector<SharedPointer<ROSDeserializer> > m_listOfROSDeserializers;


        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_SERIALIZATIONFACTORY_H_*/


