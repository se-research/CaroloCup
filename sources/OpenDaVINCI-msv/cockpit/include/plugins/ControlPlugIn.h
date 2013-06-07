/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_CONTROLPLUGIN_H_
#define COCKPIT_PLUGINS_CONTROLPLUGIN_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/io/ContainerConference.h"
#include "plugins/PlugIn.h"

namespace cockpit {

    namespace plugins {

        using namespace std;

        /**
         * This class is the main class for all plugins that are able to send data.
         */
        class ControlPlugIn : public PlugIn {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ControlPlugIn(const ControlPlugIn &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ControlPlugIn& operator=(const ControlPlugIn &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param name Name of this plugin.
                 * @param kvc KeyValueConfiguration for this plugin.
                 * @param conf client conference to send data to.
                 * @param prnt Pointer to the containing super window.
                 */
                ControlPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, core::io::ContainerConference &conf, QWidget* prnt);

                virtual ~ControlPlugIn();

                /**
                 * This method returns the container conference for this widget.
                 *
                 * @return ContainerConference.
                 */
                core::io::ContainerConference& getConference() const;

            private:
                core::io::ContainerConference &m_conference;
        };

    } // plugins

} // cockpit

#endif /*COCKPIT_PLUGINS_CONTROLPLUGIN_H_*/

