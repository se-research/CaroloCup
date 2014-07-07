/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <algorithm>
#include <fstream>

#include "core/base/CommandLineParser.h"
#include "context/base/CommandLineInterface.h"

namespace context {
    namespace base {

        using namespace std;
        using namespace core::base;

        CommandLineInterface::CommandLineInterface() :
            m_configuration(),
            m_multicastGroup(),
            m_CID(),
			m_isVerbose(false),
			m_isSupercomponent(false) {}

        CommandLineInterface::~CommandLineInterface() {}

        bool CommandLineInterface::parse(const int32_t &argc, char **argv) {
            bool retVal = true;

            CommandLineParser cmdParser;
            cmdParser.addCommandLineArgument("cid");
            cmdParser.addCommandLineArgument("configuration");
            cmdParser.addCommandLineArgument("verbose");
            cmdParser.addCommandLineArgument("supercomponent");

            cmdParser.parse(argc, argv);

            CommandLineArgument cmdArgumentCID = cmdParser.getCommandLineArgument("cid");
            CommandLineArgument cmdArgumentCONFIGURATION = cmdParser.getCommandLineArgument("configuration");
            CommandLineArgument cmdArgumentVERBOSE = cmdParser.getCommandLineArgument("verbose");
            CommandLineArgument cmdArgumentSUPERCOMPONENT = cmdParser.getCommandLineArgument("supercomponent");

            if (cmdArgumentCID.isSet()) {
                m_multicastGroup = "225.0.0." + cmdArgumentCID.getValue<string>();
                m_CID = cmdArgumentCID.getValue<uint32_t>();
            }
            else {
                retVal &= false;
            }

            if (cmdArgumentCONFIGURATION.isSet()) {
                string fileName = cmdArgumentCONFIGURATION.getValue<string>();
                ifstream configStream(fileName.c_str(), ios::in);
                if (configStream.good()) {
                    configStream >> m_configuration;
                }
                else {
                    retVal &= false;
                }
            }
            else {
                retVal &= false;
            }

            if (cmdArgumentVERBOSE.isSet()) {
            	string value = cmdArgumentVERBOSE.getValue<string>();
                transform(value.begin(), value.end(), value.begin(), ptr_fun(::tolower));

            	m_isVerbose = (value == "true");
            }

            if (cmdArgumentSUPERCOMPONENT.isSet()) {
            	string value = cmdArgumentSUPERCOMPONENT.getValue<string>();
                transform(value.begin(), value.end(), value.begin(), ptr_fun(::tolower));

            	m_isSupercomponent = (value == "true");
            }

            return retVal;
        }

        const KeyValueConfiguration CommandLineInterface::getConfiguration() const {
            return m_configuration;
        }

        const string CommandLineInterface::getMulticastGroup() const {
            return m_multicastGroup;
        }

        uint32_t CommandLineInterface::getCID() const {
            return m_CID;
        }

        bool CommandLineInterface::isVerbose() const {
        	return m_isVerbose;
        }

        bool CommandLineInterface::isSupercomponent() const {
        	return m_isSupercomponent;
        }

    }
} // context::base
