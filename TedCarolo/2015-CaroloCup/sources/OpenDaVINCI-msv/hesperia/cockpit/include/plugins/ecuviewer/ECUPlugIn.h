/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_ECUPLUGIN_H_
#define COCKPIT_PLUGINS_ECUPLUGIN_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/ControlPlugIn.h"
#include "plugins/ecuviewer/ECUWidget.h"

namespace cockpit {

    namespace plugins {

        namespace ecuviewer {

            class ECUPlugIn : public ControlPlugIn {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ECUPlugIn(const ECUPlugIn &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ECUPlugIn& operator=(const ECUPlugIn &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param name Name of this plugin.
                     * @param kvc KeyValueConfiguration for this GL-based widget.
                     * @param prnt Pointer to the container super window.
                     */
                    ECUPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, core::io::ContainerConference &conf, QWidget *prnt);

                    virtual ~ECUPlugIn();

                    virtual QWidget* getQWidget() const;

                    virtual void setupPlugin();

                    virtual void stopPlugin();

                private:
                    ECUWidget *m_viewerWidget;
            };

        }
    }
}

#endif /* COCKPIT_PLUGINS_ECUPLUGIN_H_ */
