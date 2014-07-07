/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_PARSER_PARSERTOKENEXTENDEDDATA_H_
#define HESPERIA_CORE_WRAPPER_PARSER_PARSERTOKENEXTENDEDDATA_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {
        namespace parser {

            /**
             * This is the super class for extended token data.
             */
            class OPENDAVINCI_API ParserTokenExtendedData {
                public:
                    virtual ~ParserTokenExtendedData();
            };

        }
    }
} // core::wrapper::parser

#endif /*HESPERIA_CORE_WRAPPER_PARSER_PARSERTOKENEXTENDEDDATA_H_*/
