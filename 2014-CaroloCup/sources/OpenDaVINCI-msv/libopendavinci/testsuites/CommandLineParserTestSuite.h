/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_COMMANDLINEPARSERTESTSUITE_H_
#define CORE_COMMANDLINEPARSERTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <string>
#include <iostream>

#include "core/base/CommandLineParser.h"

using namespace std;
using namespace core::base;
using namespace core::exceptions;

class CommandLineParserTestSuite : public CxxTest::TestSuite {
    public:
        void testParse() {
            CommandLineParser parser;
            parser.addCommandLineArgument("arg1");
            parser.addCommandLineArgument("arg2");

            string argv0("Command");
            string argv1("--arg1=TestArg");
            string argv2("--arg2=1");
            int32_t argc = 3;
            char **argv;
            argv = new char*[3];
            argv[0] = const_cast<char*>(argv0.c_str());
            argv[1] = const_cast<char*>(argv1.c_str());
            argv[2] = const_cast<char*>(argv2.c_str());

            parser.parse(argc, argv);

            CommandLineArgument cla1 = parser.getCommandLineArgument("arg1");
            TS_ASSERT( cla1.isSet() );
            TS_ASSERT( cla1.getArgument() == "arg1");
            TS_ASSERT( cla1.getValue<string>() == "TestArg");

            CommandLineArgument cla2 = parser.getCommandLineArgument("arg2");
            TS_ASSERT( cla2.isSet() );
            TS_ASSERT( cla2.getArgument() == "arg2");
            TS_ASSERT( cla2.getValue<int>() == 1);
        }

        void testParseSeveralArgumentFromSameKey() {
            CommandLineParser parser;
            parser.addCommandLineArgument("arg1");
            parser.addCommandLineArgument("arg2");

            string argv0("Command");
            string argv1("--arg1=TestArg");
            string argv2("--arg1=TestArg2");
            string argv3("--arg2=1");
            int32_t argc = 4;
            char **argv;
            argv = new char*[4];
            argv[0] = const_cast<char*>(argv0.c_str());
            argv[1] = const_cast<char*>(argv1.c_str());
            argv[2] = const_cast<char*>(argv2.c_str());
            argv[3] = const_cast<char*>(argv3.c_str());

            parser.parse(argc, argv);

            CommandLineArgument cla1 = parser.getCommandLineArgument("arg1");
            TS_ASSERT( cla1.isSet() );
            TS_ASSERT( cla1.getArgument() == "arg1");
            TS_ASSERT( cla1.getValue<string>() == "TestArg");

            CommandLineArgument cla2 = parser.getCommandLineArgument("arg2");
            TS_ASSERT( cla2.isSet() );
            TS_ASSERT( cla2.getArgument() == "arg2");
            TS_ASSERT( cla2.getValue<int>() == 1);

            vector<CommandLineArgument> values = parser.getCommandLineArguments("arg1");
            TS_ASSERT(values.size() == 2);
            TS_ASSERT(values.at(0).isSet());
            TS_ASSERT(values.at(0).getArgument() == "arg1");
            TS_ASSERT(values.at(0).getValue<string>() == "TestArg");

            TS_ASSERT(values.at(1).isSet());
            TS_ASSERT(values.at(1).getArgument() == "arg1");
            TS_ASSERT(values.at(1).getValue<string>() == "TestArg2");
        }

        void testGetInvalidArgument() {
            CommandLineParser parser;

            bool caught = false;
            try {
                parser.getCommandLineArgument("noArg");
            } catch (CommandLineParserException& clpe) {
                caught = true;
                TS_ASSERT(clpe.getMessage() == "Argument noArg unknown to command line parser.");
            }

            TS_ASSERT(caught);
        }

        void testParseIncompleteArgument() {
            CommandLineParser parser;
            parser.addCommandLineArgument("arg1");
            parser.addCommandLineArgument("arg2");
            parser.addCommandLineArgument("arg3NotSet");

            string argv0("Command");
            string argv1("--arg1=TestArg");
            string argv2("--arg2=1");
            int32_t argc = 3;
            char **argv;
            argv = new char*[3];
            argv[0] = const_cast<char*>(argv0.c_str());
            argv[1] = const_cast<char*>(argv1.c_str());
            argv[2] = const_cast<char*>(argv2.c_str());

            parser.parse(argc, argv);

            CommandLineArgument cla1 = parser.getCommandLineArgument("arg1");
            TS_ASSERT( cla1.isSet() );
            TS_ASSERT( cla1.getArgument() == "arg1");
            TS_ASSERT( cla1.getValue<string>() == "TestArg");

            CommandLineArgument cla2 = parser.getCommandLineArgument("arg2");
            TS_ASSERT( cla2.isSet() );
            TS_ASSERT( cla2.getArgument() == "arg2");
            TS_ASSERT( cla2.getValue<int>() == 1);

            CommandLineArgument cla3 = parser.getCommandLineArgument("arg3NotSet");
            TS_ASSERT( !cla3.isSet() );
            TS_ASSERT( cla3.getArgument() == "arg3NotSet");
            TS_ASSERT( cla3.getValue<string>() == "");
        }

        void testUnhandledArgument() {
            CommandLineParser parser;
            parser.addCommandLineArgument("arg1");

            string argv0("Command");
            string argv1("--arg1=TestArg");
            string argv2("--unhandled");
            int32_t argc = 3;
            char **argv;
            argv = new char*[3];
            argv[0] = const_cast<char*>(argv0.c_str());
            argv[1] = const_cast<char*>(argv1.c_str());
            argv[2] = const_cast<char*>(argv2.c_str());

            bool caught = false;
            try {
                parser.parse(argc, argv);
            } catch (CommandLineParserException& clpe) {
                caught = true;
                TS_ASSERT(clpe.getMessage() == "Unknown command line argument '--unhandled'.");
            }

            TS_ASSERT(caught);
        }
};

#endif /*CORE_COMMANDLINEPARSERTESTSUITE_H_*/
