/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_LIBRARIES_H_
#define HESPERIA_CORE_WRAPPER_LIBRARIES_H_

namespace core {
    namespace wrapper {

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
