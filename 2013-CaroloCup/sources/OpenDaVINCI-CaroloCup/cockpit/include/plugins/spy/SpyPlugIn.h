/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_SPYPLUGIN_H_
#define COCKPIT_PLUGINS_SPYPLUGIN_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/PlugIn.h"
#include "plugins/spy/SpyWidget.h"

namespace cockpit {

    namespace plugins {

        namespace spy {

            class SpyPlugIn : public PlugIn {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    SpyPlugIn(const SpyPlugIn &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    SpyPlugIn& operator=(const SpyPlugIn &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param name Name of this plugin.
                     * @param kvc KeyValueConfiguration for this GL-based widget.
                     * @param prnt Pointer to the container super window.
                     */
                    SpyPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

                    virtual ~SpyPlugIn();

                    virtual QWidget* getQWidget() const;

                    virtual void setupPlugin();

                    virtual void stopPlugin();

                private:
                    SpyWidget *m_viewerWidget;
            };

        }
    }
}

#endif /* COCKPIT_PLUGINS_SPYPLUGIN_H_ */
