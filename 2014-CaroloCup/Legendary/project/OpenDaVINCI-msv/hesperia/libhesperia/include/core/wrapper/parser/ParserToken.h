/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_PARSER_PARSERTOKEN_H_
#define HESPERIA_CORE_WRAPPER_PARSER_PARSERTOKEN_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/wrapper/parser/ParserTokenExtendedData.h"

namespace core {
    namespace wrapper {
        namespace parser {

            using namespace std;

            class OPENDAVINCI_API ParserToken {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ParserToken(const ParserToken &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ParserToken& operator=(const ParserToken &);

                public:
                    /**
                     * Constructor.
                     *
                     * @param value The value of the token.
                     * @param data Extended data for a token.
                     */
                    ParserToken(const string &value, const ParserTokenExtendedData &data);

                    virtual ~ParserToken();

                    /**
                     * This method returns the value of the token.
                     *
                     * @return Value.
                     */
                    const string getValue() const;

                    /**
                     * This method returns the extended data.
                     *
                     * @return Extended data.
                     */
                    const ParserTokenExtendedData& getExtendedData() const;

                private:
                    const string m_value;
                    const ParserTokenExtendedData &m_data;
            };

        }
    }
} // core::wrapper::parser

#endif /*HESPERIA_CORE_WRAPPER_PARSER_PARSERTOKEN_H_*/
