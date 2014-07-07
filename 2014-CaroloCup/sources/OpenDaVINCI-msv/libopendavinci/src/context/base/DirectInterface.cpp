/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <sstream>

#include "context/base/DirectInterface.h"

namespace context {
    namespace base {

        using namespace std;
        using namespace core::base;

        DirectInterface::DirectInterface(const string &multicastGroup, const uint32_t &CID, const string &configuration) :
            m_configuration(),
            m_multicastGroup(multicastGroup),
            m_CID(CID) {
            // Parse configuration data.
            stringstream sstr;
            sstr.str(configuration);
            sstr >> m_configuration;
        }

        DirectInterface::~DirectInterface() {}

        const KeyValueConfiguration DirectInterface::getConfiguration() const {
            return m_configuration;
        }

        const string DirectInterface::getMulticastGroup() const {
            return m_multicastGroup;
        }

        uint32_t DirectInterface::getCID() const {
            return m_CID;
        }

        bool DirectInterface::isVerbose() const {
        	return true;
        }

        bool DirectInterface::isSupercomponent() const {
        	return true;
        }
    }
} // context::base
