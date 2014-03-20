/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_COMMANDLINEPARSER_H_
#define HESPERIA_CORE_BASE_COMMANDLINEPARSER_H_

#include <string>
#include <map>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/CommandLineArgument.h"
#include "core/exceptions/Exceptions.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * The command line parser parses the command line used
         * to start a module. Possible command line arguments can
         * be set before parsing and accessed later.
         */
        class HESPERIA_API CommandLineParser {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                CommandLineParser(const CommandLineParser&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                CommandLineParser& operator=(const CommandLineParser &);

            public:
                CommandLineParser();

                virtual ~CommandLineParser();

                /**
                 * Adds a new argument of the form "--argument=..." to
                 * the command line parser.
                 *
                 * @param argument Argument to be added to the parser.
                 */
                void addCommandLineArgument(const string& argument);

                /**
                 * Parses the command line. The first argument is skipped
                 * because it contains the path to the executable.
                 *
                 * @param argc Number of arguments.
                 * @param argv Arguments.
                 */
                void parse(const int32_t &argc, char **argv);

                /**
                 * Returns a CommandLineArgument containing information about
                 * the specified argument.
                 *
                 * @param argument Command line argument to be queried.
                 * @return Value for the queried argument.
                 */
                const CommandLineArgument getCommandLineArgument(const string& argument) throw (exceptions::CommandLineParserException);

                /**
                 * Returns a list of CommandLineArgument containing information about
                 * the specified argument.
                 *
                 * @param argument Command line argument to be queried.
                 * @return List of values for the queried argument.
                 */
                const vector<CommandLineArgument> getCommandLineArguments(const string& argument);

            protected:
                vector<string> m_listOfArgumentsToParse;
                map<string, vector<CommandLineArgument> > m_values;
        };
    } // base
} // core::base

#endif // HESPERIA_CORE_BASE_COMMANDLINEPARSER_H_
