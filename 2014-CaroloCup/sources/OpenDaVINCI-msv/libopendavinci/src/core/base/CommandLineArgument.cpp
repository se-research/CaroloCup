/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/CommandLineArgument.h"

namespace core {
    namespace base {

        CommandLineArgument::CommandLineArgument() :
                m_argument(""),
                m_value(""),
                m_set(false) {}

        CommandLineArgument::CommandLineArgument(const string& argument, const string& value, const bool &set) :
                m_argument(argument),
                m_value(value),
                m_set(set) {}

        CommandLineArgument::~CommandLineArgument() {}

        bool CommandLineArgument::isSet() const {
            return m_set;
        }

        const std::string CommandLineArgument::getArgument() const {
            return m_argument;
        }
    }
} // core::base
