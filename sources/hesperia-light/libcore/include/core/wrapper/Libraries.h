/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_LIBRARIES_H_
#define HESPERIA_CORE_WRAPPER_LIBRARIES_H_

namespace core {
    namespace wrapper {

        enum SYSTEM_ENDIANESS {
            IS_LITTLE_ENDIAN,
            IS_BIG_ENDIAN
        };

#if defined(__i386__) || defined(i386) || defined(_M_IX86) || defined(_X86_) || defined(__arm__) || defined(__x86_64__) || defined(__ia64__) || defined(_M_X64_)
        const SYSTEM_ENDIANESS USESYSTEMENDINANESS = IS_LITTLE_ENDIAN;
#elif defined(__ppc__) || defined(__powerpc) || defined(__powerpc__) || defined(__POWERPC__) || defined(_M_PPC)
        const SYSTEM_ENDIANESS USESYSTEMENDINANESS = IS_BIG_ENDIAN;
#else
    #error "Please define system endianess in core/wrapper/Libraries.h."
#endif

        /**
         * Enumeration of supported, but mutual excluded system libraries.
         */
        enum SYSTEM_LIBRARIES {
            POSIX_LIBRARIES
        };

        /**
         * Which system library has to be used?
         *
         * To add a new library, simply change const to static,
         * try to compile libnative, fix all compiling errors
         * and re-change it back to const!
        * For Win32 systems, Boost libraries must be used!
         */
#ifdef WIN32
        const SYSTEM_LIBRARIES USESYSTEMLIBRARY = BOOST_LIBRARIES;
#else
        const SYSTEM_LIBRARIES USESYSTEMLIBRARY = POSIX_LIBRARIES;
#endif

        /**
         * Enumeration of supported, but mutual excluded key/value-database libraries.
         */
        enum KEYVALUEDATABASE_LIBRARIES {
            BERKELEYDB_LIBRARIES,
            SIMPLEDB_LIBRARIES
        };

        /**
         * Which key/value-database library has to be used?
         *
         * To add a new library, simply change const to static,
         * try to compile libnative, fix all compiling errors
         * and re-change it back to const!
         */
        const KEYVALUEDATABASE_LIBRARIES USEKEYVALUEDATABASELIBRARY = BERKELEYDB_LIBRARIES;

        /**
         * Enumeration of supported, but mutual excluded compression libraries.
         */
        enum COMPRESSION_LIBRARIES {
            ZIP_LIBRARIES
        };

        /**
         * Which compression library has to be used?
         *
         * To add a new library, simply change const to static,
         * try to compile libnative, fix all compiling errors
         * and re-change it back to const!
         */
        const COMPRESSION_LIBRARIES USECOMPRESSIONLIBRARY = ZIP_LIBRARIES;

        /**
         * Enumeration of supported, but mutual excluded parser libraries.
         */
        enum PARSER_LIBRARIES {
            BOOST_SPIRIT_LIBRARIES
        };

        /**
         * Which parser library has to be used?
         *
         * To add a new library, simply change const to static,
         * try to compile libnative, fix all compiling errors
         * and re-change it back to const!
         */
        const PARSER_LIBRARIES USEPARSERLIBRARY = BOOST_SPIRIT_LIBRARIES;

        /**
         * Enumeration of supported, but mutual excluded imaging libraries.
         */
        enum IMAGING_LIBRARIES {
            OPENCV_LIBRARIES
        };

        /**
         * Which imaging library has to be used?
         *
         * To add a new library, simply change const to static,
         * try to compile libnative, fix all compiling errors
         * and re-change it back to const!
         */
        const IMAGING_LIBRARIES USEIMAGINGLIBRARY = OPENCV_LIBRARIES;


        /**
         * Enumeration of supported, but mutual excluded matrix libraries.
         */
        enum MATRIX_LIBRARIES {
            MATRIX_OPENCV_LIBRARIES
        };

        /**
         * Which matrix library has to be used?
         *
         * To add a new library, simply change const to static,
         * try to compile libnative, fix all compiling errors
         * and re-change it back to const!
         */
        const MATRIX_LIBRARIES USEMATRIXLIBRARY = MATRIX_OPENCV_LIBRARIES;
    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_LIBRARIES_H_*/
