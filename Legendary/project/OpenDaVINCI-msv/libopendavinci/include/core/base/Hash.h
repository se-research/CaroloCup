/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_HASH_H_
#define OPENDAVINCI_CORE_BASE_HASH_H_

#define OPENDAVINCI_CORE_STRINGLITERAL1(a)                      CharList<a, NullType>
#define OPENDAVINCI_CORE_STRINGLITERAL2(a, b)                   CharList<a, CharList<b, NullType> >
#define OPENDAVINCI_CORE_STRINGLITERAL3(a, b, c)                CharList<a, CharList<b, CharList<c, NullType> > >
#define OPENDAVINCI_CORE_STRINGLITERAL4(a, b, c, d)             CharList<a, CharList<b, CharList<c, CharList<d, NullType> > > >
#define OPENDAVINCI_CORE_STRINGLITERAL5(a, b, c, d, e)          CharList<a, CharList<b, CharList<c, CharList<d, CharList<e, NullType> > > > >
#define OPENDAVINCI_CORE_STRINGLITERAL6(a, b, c, d, e, f)       CharList<a, CharList<b, CharList<c, CharList<d, CharList<e, CharList<f, NullType> > > > > >
#define OPENDAVINCI_CORE_STRINGLITERAL7(a, b, c, d, e, f, g)    CharList<a, CharList<b, CharList<c, CharList<d, CharList<e, CharList<f, CharList<g, NullType> > > > > > >
#define OPENDAVINCI_CORE_STRINGLITERAL8(a, b, c, d, e, f, g, h) CharList<a, CharList<b, CharList<c, CharList<d, CharList<e, CharList<f, CharList<g, CharList<h, NullType> > > > > > > >

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace base {

        /** This is the CRC32 polynomial. */
        const uint32_t CRC32POLYNOMIAL = 0x04C11DB7;

        /**
         * This template is the '\0' character for the character list.
         */
        class NullType {
            public:
                enum { value = -1 };
                enum { hasNext = false };
                typedef NullType tail;
        };

        /**
         * This template is the character list.
         */
        template <char x, typename xs>
        class CharList {
            public:
                enum { value = x };
                typedef xs tail;
        };

        /**
         * This template is the specialization of character list
         * for the case one character, NullType.
         */
        template <char x>
        class CharList<x, NullType> {
            public:
                enum { value = x };
                typedef NullType tail;
        };

        /**
         * This template computes the CRC32 for a given character.
         */
        template<char c, uint32_t result >
        class CRC32_COMPUTING {
            public:
                enum { RESULT = result ^ (c ^ CRC32POLYNOMIAL) };
        };

        /**
         * This template computes the CRC32 recursively by iterating
         * through the list of characters.
         */
        template <char c, int32_t res, typename T>
        class CRC32_RECURSIVE {
            public:
                enum { RES = CRC32_COMPUTING<c, res>::RESULT };
                enum { RESULT = RES + CRC32_RECURSIVE<T::value, RES, typename T::tail>::RESULT };
        };

        /**
         * This template computes the CRC32 recursively by iterating
         * through the list of characters for the specialization
         * of c, NullType.
         */
        template <char c, int32_t res>
        class CRC32_RECURSIVE<c, res, NullType> {
            public:
                enum { RESULT = CRC32_COMPUTING<c, res>::RESULT };
        };

        /**
         * This template is for convenience purposes (i.e. the interface
         * for the compile-time-computing of CRC32 checksums).
         */
        template <typename T>
        class CRC32 {
            public:
                enum { RESULT = CRC32_RECURSIVE<T::value, 0, typename T::tail>::RESULT };
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_HASH_H_*/
