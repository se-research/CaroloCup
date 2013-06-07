/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_SERIALIZABLE_H_
#define HESPERIA_CORE_BASE_SERIALIZABLE_H_

#include <iostream>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This class is the interface for any serializable.
         *
         * It can be used as follows:
         *
         * @code
         * #include "base/Hash.h"
         *
         * class MyData : Serializable {
         *     private:
         *        int32_t m_data;
         *
         *     public:
         *        ostream& operator<<(ostream &out) const {
         *            SerializationFactory sf;
         *            Serializer &s = sf.getSerializer(out);
         *            s.write(CRC32<HESPERIA_CORE_STRINGLITERAL2('I', 'D')>::RESULT, m_data);
         *            return out;
         *        }
         *
         *        istream& operator>>(istream &in) {
         *            SerializationFactory sf;
         *            Deserializer &d = sf.getDeserializer(in);
         *            d.read(CRC32<HESPERIA_CORE_STRINGLITERAL2('I', 'D')>::RESULT, m_data);
         *            return in;
         *        }
         * }
         * @endcode
         */
        class HESPERIA_API Serializable {
            public:
                virtual ~Serializable();

                /**
                 * This method needs to be used to serialize data.
                 *
                 * @param out ostream to serialize data to.
                 * @return The ostream.
                 */
                virtual ostream& operator<<(ostream &out) const = 0;

                /**
                 * This method needs to be used to deserialize data.
                 *
                 * @param in istream to deserialize data from.
                 * @return The istream.
                 */
                virtual istream& operator>>(istream &in) = 0;
        };

    }
} // core::base

namespace std {
    // The following methods are declarations for convenient usage.
    ostream HESPERIA_API &operator<<(ostream &out, const core::base::Serializable &s);
    istream HESPERIA_API &operator>>(istream &in, core::base::Serializable &s);
}

#endif /*HESPERIA_CORE_BASE_SERIALIZEABLE_H_*/
