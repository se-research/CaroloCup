/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_COMMANDLINEARGUMENT_H_
#define HESPERIA_CORE_BASE_COMMANDLINEARGUMENT_H_

#include <string>
#include <sstream>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This class represents a command line argument parsed by
         * CommandLineParser. It provides methods to access the
         * argument's name, the argument's value (as arbitrary data type)
         * and to find out whether this argument has been specified in the
         * command line.
         */
        class HESPERIA_API CommandLineArgument {
            public:
                CommandLineArgument();

                /**
                 * Constructor.
                 *
                 * @param argument Commandline argument to match.
                 * @param value Value for the given argument.
                 */
                CommandLineArgument(const string& argument, const string& value, const bool &set);

                virtual ~CommandLineArgument();

                /**
                 * Returns true if this argument was specified
                 * in the command line.
                 */
                bool isSet() const;

                /**
                 * Returns the argument's name.
                 *
                 * @return The argument's name.
                 */
                const string getArgument() const;

                /**
                 * Returns the value of the command line argument
                 * as data type T
                 */
                template<class T>
                inline T getValue() {
                    T retVal;
                    stringstream ss;
                    ss << m_value;
                    ss >> retVal;
                    return retVal;
                };

            protected:
                string m_argument;
                string m_value;
                bool m_set;
        };
    }
} // core::base

#endif // HESPERIA_CORE_BASE_COMMANDLINEARGUMENT_H_
