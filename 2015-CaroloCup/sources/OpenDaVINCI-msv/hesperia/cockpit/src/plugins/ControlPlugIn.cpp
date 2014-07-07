/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/base/Lock.h"

#include "plugins/ControlPlugIn.h"

namespace cockpit {

    namespace plugins {

        using namespace std;
        using namespace core::base;
        using namespace core::io;

        ControlPlugIn::ControlPlugIn(const string &name, const KeyValueConfiguration &kvc, ContainerConference &conf, QWidget* prnt) :
                PlugIn(name, kvc, prnt),
                m_conference(conf) {}

        ControlPlugIn::~ControlPlugIn() {}

        ContainerConference& ControlPlugIn::getConference() const {
            return m_conference;
        }

    } // plugins

} // cockpit

