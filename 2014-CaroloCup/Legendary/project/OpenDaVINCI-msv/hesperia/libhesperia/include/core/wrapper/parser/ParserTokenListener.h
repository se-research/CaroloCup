/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_PARSER_PARSERTOKENLISTENER_H_
#define HESPERIA_CORE_WRAPPER_PARSER_PARSERTOKENLISTENER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {
        namespace parser {

            // Forward declaration.
            class ParserToken;

            /**
             * This interface encapsulates all data for a token listener.
             */
            class OPENDAVINCI_API ParserTokenListener {
                public:
                    virtual ~ParserTokenListener();

                    /**
                     * This method is called to process the next token from the grammar.
                     *
                     * @param token Token to be processed.
                     */
                    virtual void nextToken(const ParserToken &token) = 0;
            };

        }
    }
} // core::wrapper::parser

#endif /*HESPERIA_CORE_WRAPPER_PARSER_PARSERTOKENLISTENER_H_*/
