/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_IRUSCHARTS_IRUSCHARTSPLUGIN_H_
#define COCKPIT_PLUGINS_IRUSCHARTS_IRUSCHARTSPLUGIN_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/PlugIn.h"
#include "plugins/iruscharts/IrUsChartsWidget.h"

namespace cockpit {

    namespace plugins {

        namespace iruscharts {

          class IrUsChartsPlugIn : public PlugIn {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                IrUsChartsPlugIn(const IrUsChartsPlugIn &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                IrUsChartsPlugIn& operator=(const IrUsChartsPlugIn &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param name Name of this plugin.
                 * @param kvc KeyValueConfiguration for this based widget.
                 * @param prnt Pointer to the containing super window.
                 */
                IrUsChartsPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

                virtual ~IrUsChartsPlugIn();

                virtual QWidget* getQWidget() const;

                virtual void setupPlugin();

                virtual void stopPlugin();

            private:
                IrUsChartsWidget *m_irusChartsWidget;
            };

        }
    }
}

#endif /*COCKPIT_PLUGINS_IRUSCHARTS_IRUSCHARTSPLUGIN_H_*/

