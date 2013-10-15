/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_PARSER_PARSERERROREXTENDEDDATA_H_
#define HESPERIA_CORE_WRAPPER_PARSER_PARSERERROREXTENDEDDATA_H_

#include <stdint.h>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {
        namespace parser {

            /**
             * This is the super class for extended parser error data.
             */
            class OPENDAVINCI_API ParserErrorExtendedData {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ParserErrorExtendedData(const ParserErrorExtendedData &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ParserErrorExtendedData& operator=(const ParserErrorExtendedData &);

                public:
                    ParserErrorExtendedData();

                    virtual ~ParserErrorExtendedData();

                    /**
                     * This method sets the line on which the error occurred.
                     *
                     * @param line Line on which the error occurred.
                     */
                    void setLine(const uint32_t &line);

                    /**
                     * This method returns the line on which the error occurred.
                     *
                     * @return Line on which the error occurred.
                     */
                    uint32_t getLine() const;

                private:
                    uint32_t m_line;
            };

        }
    }
} // core::wrapper::parser

#endif /*HESPERIA_CORE_WRAPPER_PARSER_PARSERERROREXTENDEDDATA_H_*/
