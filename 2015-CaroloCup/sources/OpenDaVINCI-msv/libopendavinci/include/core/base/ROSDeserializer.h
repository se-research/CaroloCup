/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_ROSDESERIALIZER_H_
#define OPENDAVINCI_CORE_BASE_ROSDESERIALIZER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Deserializer.h"
#include "core/data/Container.h"


namespace core {
    namespace base {

        using namespace std;

        class SerializationFactory;


        class OPENDAVINCI_API ROSDeserializer : public Deserializer {
            private:
                // Only the SerializationFactory or its subclasses are allowed to create instances of this Deserializer.
                friend class SerializationFactory;

                /**
                 * Constructor.
                 *
                 * @param in Input stream for the data.
                 */
                ROSDeserializer(istream &in);

            private:
                /**
                 * Forbidden default constructor.
                 */
                ROSDeserializer();

                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ROSDeserializer(const ROSDeserializer &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ROSDeserializer& operator=(const ROSDeserializer &);


            public:
                virtual ~ROSDeserializer();

                virtual void read(const uint32_t id, Serializable &s);

                virtual void read(const uint32_t id, bool &b);

                virtual void read(const uint32_t id, char &c);

                virtual void read(const uint32_t id, unsigned char &uc);

                virtual void read(const uint32_t id, int32_t &i);

                virtual void read(const uint32_t id, uint32_t &ui);

                virtual void read(const uint32_t id, float &f);

                virtual void read(const uint32_t id, double &d);

                virtual void read(const uint32_t id, string &s);

                virtual void read(const uint32_t id, void *data, uint32_t size);
                
                void read(istream &in, core::data::Container &container);

            private:
                                
                stringstream m_buffer;
                uint32_t m_size;
                uint32_t position;
                istream &m_in;
                
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_ROSDESERIALIZER_H_*/
