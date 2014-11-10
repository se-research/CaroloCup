/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/parser/ParserError.h"

namespace core {
    namespace wrapper {
        namespace parser {

            ParserError::ParserError(const string &context, const ParserErrorExtendedData &peed) :
                    m_context(context),
                    m_data(peed) {}

            ParserError::~ParserError() {}

            const string ParserError::getContext() const {
                return m_context;
            }

            const ParserErrorExtendedData& ParserError::getExtendedData() const {
                return m_data;
            }

        }
    }
} // core::wrapper::parser
