/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/CommandLineParser.h"
#include "core/exceptions/Exceptions.h"

namespace core {
    namespace base {
        using namespace std;
        using namespace core::exceptions;

        CommandLineParser::CommandLineParser() :
            m_listOfArgumentsToParse(),
            m_values() {}

        CommandLineParser::~CommandLineParser() {}

        void CommandLineParser::addCommandLineArgument(const string& argument) {
            m_listOfArgumentsToParse.push_back(argument);
        }

        const CommandLineArgument CommandLineParser::getCommandLineArgument(const string &argument) throw (CommandLineParserException) {
            vector<string>::iterator it = find(m_listOfArgumentsToParse.begin(), m_listOfArgumentsToParse.end(), argument);
            if (it == m_listOfArgumentsToParse.end()) {
                errno = 0;
                OPENDAVINCI_CORE_THROW_EXCEPTION(CommandLineParserException, "Argument " + argument + " unknown to command line parser.");
            }

            CommandLineArgument cla(argument, "", false);
            if (m_values[argument].size() > 0) {
                cla = *(m_values[argument].begin());
            }
            return cla;
        }

        const vector<CommandLineArgument> CommandLineParser::getCommandLineArguments(const string& argument) {
            return m_values[argument];
        }

        void CommandLineParser::parse(const int32_t &argc, char **argv) {
            assert(argc > 1);
            vector<string>::iterator it;
            const string argStart("--");

            for (int32_t i = 1; i < argc; i++) {
                string cmdLineValue(argv[i]);
                bool handled = false;
                for (it = m_listOfArgumentsToParse.begin(); it != m_listOfArgumentsToParse.end(); it++) {
                    const string argument = (*it);
                    string cmdLineArgument = argStart + argument + "=";

                    if ( (cmdLineValue.length() > cmdLineArgument.length()) &&
                            (cmdLineValue.substr(0, cmdLineArgument.length()) == cmdLineArgument) ) {
                        m_values[argument].push_back(CommandLineArgument(argument, cmdLineValue.substr(cmdLineArgument.length()), true));
                        handled = true;
                        break;
                    }
                }

                if (!handled) {
                    OPENDAVINCI_CORE_THROW_EXCEPTION(CommandLineParserException, "Unknown command line argument '" + cmdLineValue + "'.");
                }
            }
        }
    }
} // core::base
