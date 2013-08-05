/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/parser/ParserToken.h"

namespace core {
    namespace wrapper {
        namespace parser {

            ParserToken::ParserToken(const string &value, const ParserTokenExtendedData &data) :
                    m_value(value),
                    m_data(data) {}

            ParserToken::~ParserToken() {}

            const string ParserToken::getValue() const {
                return m_value;
            }

            const ParserTokenExtendedData& ParserToken::getExtendedData() const {
                return m_data;
            }

        }
    }
} // core::wrapper::parser
