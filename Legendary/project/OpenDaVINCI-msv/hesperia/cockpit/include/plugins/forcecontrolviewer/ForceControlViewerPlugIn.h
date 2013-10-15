/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_FORCECONTROLVIEWERPLUGIN_H_
#define COCKPIT_PLUGINS_FORCECONTROLVIEWERPLUGIN_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/PlugIn.h"
#include "plugins/forcecontrolviewer/ForceControlViewerWidget.h"

namespace cockpit {

    namespace plugins {

        namespace forcecontrolviewer {

          class ForceControlViewerPlugIn : public PlugIn {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ForceControlViewerPlugIn(const ForceControlViewerPlugIn &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ForceControlViewerPlugIn& operator=(const ForceControlViewerPlugIn &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param name Name of this plugin.
                 * @param kvc KeyValueConfiguration for this based widget.
                 * @param prnt Pointer to the containing super window.
                 */
                ForceControlViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

                virtual ~ForceControlViewerPlugIn();

                virtual QWidget* getQWidget() const;

                virtual void setupPlugin();

                virtual void stopPlugin();

            private:
                ForceControlViewerWidget *m_fcViewerWidget;
            };

        }
    }
}

#endif /*COCKPIT_PLUGINS_FORCECONTROLVIEWERPLUGIN_H_*/

