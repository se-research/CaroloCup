/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/QueryableNetstringsSerializer.h"
 #include "core/base/QueryableNetstringsDeserializer.h"
#include "core/base/PROTOSerializer.h"
#include "core/base/PROTODeserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/LCMSerializer.h"
#include "core/base/LCMDeserializer.h"

namespace core {
    namespace base {

        using namespace std;

        SerializationFactory::SerializationFactory() :
                m_listOfSerializers(),
                m_listOfDeserializers(),
                m_listOfLCMSerializers(),
                m_listOfLCMDeserializers(){}

        SerializationFactory::~SerializationFactory() {
            m_listOfSerializers.clear();
            m_listOfDeserializers.clear();
            m_listOfLCMSerializers.clear();
            m_listOfLCMDeserializers.clear();

        }

        Serializer& SerializationFactory::getSerializer(ostream &out) const {
            Serializer *s = NULL;
            //Serializer *temp = NULL;
            if (m_listOfSerializers.empty()) {
//           	s = new QueryableNetstringsSerializer(out);
              //  s = new PROTOSerializer(out);
           	s = new LCMSerializer(out);
                m_listOfSerializers.push_back(SharedPointer<Serializer>(s));
            }
            else {
                s = &(*(*(m_listOfSerializers.begin()))); // The innermost * dereferences the iterator to SharedPointer<Serializer>, the second * returns the Serializer from within the SharedPointer, and the & turns it into a regular pointer.
            }
            return *s;
        }

        //Thesis implementation
        LCMSerializer& SerializationFactory::getLCMSerializer(ostream &out) const {
        	LCMSerializer *lcms = NULL;
        	if (m_listOfLCMSerializers.empty()){
        		lcms = new LCMSerializer(out);
        		m_listOfLCMSerializers.push_back(SharedPointer<LCMSerializer>(lcms));
        	}
        	else {
        	    lcms = &(*(*(m_listOfLCMSerializers.begin()))); // The innermost * dereferences the iterator to SharedPointer<Serializer>, the second * returns the Serializer from within the SharedPointer, and the & turns it into a regular pointer.
        	}
        	return *lcms;
        }


        LCMDeserializer& SerializationFactory::getLCMDeserializer(istream &in) const {
			LCMDeserializer *lcmd = NULL;
			if (m_listOfLCMDeserializers.empty()) {
				lcmd = new LCMDeserializer(in);
				m_listOfLCMDeserializers.push_back(SharedPointer<LCMDeserializer>(lcmd)); // The innermost * dereferences the iterator to SharedPointer<Deserializer>, the second * returns the Deserializer from within the SharedPointer, and the & turns it into a regular pointer.
			}
			else {
				lcmd = &(*(*(m_listOfLCMDeserializers.begin())));
			}
			return *lcmd;
		}

		
		
		       PROTOSerializer& SerializationFactory::getPROTOSerializer(ostream &out) const {
            PROTOSerializer *lcms = NULL;
            if (m_listOfPROTOSerializers.empty()){
                lcms = new PROTOSerializer(out);
                m_listOfPROTOSerializers.push_back(SharedPointer<PROTOSerializer>(lcms));
            }
            else {
                lcms = &(*(*(m_listOfPROTOSerializers.begin()))); // The innermost * dereferences the iterator to SharedPointer<Serializer>, the second * returns the Serializer from within the SharedPointer, and the & turns it into a regular pointer.
            }
            return *lcms;
        }


        PROTODeserializer& SerializationFactory::getPROTODeserializer(istream &in) const {
            PROTODeserializer *lcmd = NULL;
            if (m_listOfPROTODeserializers.empty()) {
                lcmd = new PROTODeserializer(in);
                m_listOfPROTODeserializers.push_back(SharedPointer<PROTODeserializer>(lcmd)); // The innermost * dereferences the iterator to SharedPointer<Deserializer>, the second * returns the Deserializer from within the SharedPointer, and the & turns it into a regular pointer.
            }
            else {
                lcmd = &(*(*(m_listOfPROTODeserializers.begin())));
            }
            return *lcmd;
        }
        //End of thesis implementation

        Deserializer& SerializationFactory::getDeserializer(istream &in) const {
            Deserializer *d = NULL;
            if (m_listOfDeserializers.empty()) {
//               d = new QueryableNetstringsDeserializer(in);
              //d = new  PROTODeserializer(in);
            	d = new LCMDeserializer(in);
	       	    m_listOfDeserializers.push_back(SharedPointer<Deserializer>(d)); // The innermost * dereferences the iterator to SharedPointer<Deserializer>, the second * returns the Deserializer from within the SharedPointer, and the & turns it into a regular pointer.
            }
            else {
                d = &(*(*(m_listOfDeserializers.begin())));
            }
            return *d;
        }

    }
} // core::base
