/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_COMMANDLINEINTERFACE_H_
#define CONTEXT_BASE_COMMANDLINEINTERFACE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "context/base/RuntimeControlInterface.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class provides access to the RuntimeControl using the CLI.
         */
        class OPENDAVINCI_API CommandLineInterface : public RuntimeControlInterface {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                CommandLineInterface(const CommandLineInterface&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                CommandLineInterface& operator=(const CommandLineInterface&);

            public:
                CommandLineInterface();

                virtual ~CommandLineInterface();

                /**
                 * This method parses the commandline to get necessary information.
                 *
                 * @param argc Number of command line arguments.
                 * @param argv Commandline arguments.
                 * @return true if the arguments provided by the commandline could be successfully parsed.
                 */
                bool parse(const int32_t &argc, char **argv);

                virtual const core::base::KeyValueConfiguration getConfiguration() const;

                virtual const string getMulticastGroup() const;

                virtual uint32_t getCID() const;

                virtual bool isVerbose() const;

                virtual bool isSupercomponent() const;

            private:
                core::base::KeyValueConfiguration m_configuration;
                string m_multicastGroup;
                uint32_t m_CID;
                bool m_isVerbose;
                bool m_isSupercomponent;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_COMMANDLINEINTERFACE_H_*/
