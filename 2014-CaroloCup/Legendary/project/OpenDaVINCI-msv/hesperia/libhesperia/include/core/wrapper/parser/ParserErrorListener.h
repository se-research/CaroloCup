/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_PARSER_PARSERERRORLISTENER_H_
#define HESPERIA_CORE_WRAPPER_PARSER_PARSERERRORLISTENER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {
        namespace parser {

            // Forward declaration.
            class ParserError;

            /**
             * This interface encapsulates all data for an error listener.
             */
            class OPENDAVINCI_API ParserErrorListener {
                public:
                    virtual ~ParserErrorListener();

                    /**
                     * This method is called to process the next token from the grammar.
                     *
                     * @param error The occurred error.
                     */
                    virtual void errorToken(ParserError &error) = 0;
            };

        }
    }
} // core::wrapper::parser

#endif /*HESPERIA_CORE_WRAPPER_PARSER_PARSERERRORLISTENER_H_*/
