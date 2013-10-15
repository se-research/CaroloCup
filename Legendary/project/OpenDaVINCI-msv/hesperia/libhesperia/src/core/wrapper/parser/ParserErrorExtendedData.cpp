/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/parser/ParserErrorExtendedData.h"

namespace core {
    namespace wrapper {
        namespace parser {

            ParserErrorExtendedData::ParserErrorExtendedData() :
                    m_line(0) {}

            ParserErrorExtendedData::~ParserErrorExtendedData() {}

            void ParserErrorExtendedData::setLine(const uint32_t &line) {
                m_line = line;
            }

            uint32_t ParserErrorExtendedData::getLine() const {
                return m_line;
            }

        }
    }
} // core::wrapper::parser
