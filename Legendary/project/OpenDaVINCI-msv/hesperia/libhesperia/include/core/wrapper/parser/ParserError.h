/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_PARSER_PARSERERROR_H_
#define HESPERIA_CORE_WRAPPER_PARSER_PARSERERROR_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/wrapper/parser/ParserErrorExtendedData.h"

namespace core {
    namespace wrapper {
        namespace parser {

            using namespace std;

            /**
             * This class encapsulates a parser error.
             */
            class OPENDAVINCI_API ParserError {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ParserError(const ParserError &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ParserError& operator=(const ParserError &);

                public:
                    /**
                     * Constructor.
                     *
                     * @param context The context where the error occurred.
                     * @param peed Extended data for this parser error.
                     */
                    ParserError(const string &context, const ParserErrorExtendedData &peed);

                    virtual ~ParserError();

                    /**
                     * This method returns the context where the error occurred.
                     *
                     * @return context.
                     */
                    const string getContext() const;

                    /**
                     * This method returns the extended data for the parser error.
                     *
                     * @return Extended data for the occurred error.
                     */
                    const ParserErrorExtendedData& getExtendedData() const;

                private:
                    const string m_context;
                    const ParserErrorExtendedData &m_data;
            };

        }
    }
} // core::wrapper::parser

#endif /*HESPERIA_CORE_WRAPPER_PARSER_PARSERERROR_H_*/
