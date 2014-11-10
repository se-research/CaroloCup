/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_IRUSMAP_IRUSMAPPLUGIN_H_
#define COCKPIT_PLUGINS_IRUSMAP_IRUSMAPPLUGIN_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/base/KeyValueConfiguration.h"

#include "plugins/PlugIn.h"
#include "plugins/irusmap/IrUsMapWidgetControl.h"

namespace cockpit {

    namespace plugins {

        namespace irusmap {

            using namespace std;

            class IrUsMapPlugIn : public PlugIn {

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    IrUsMapPlugIn(const IrUsMapPlugIn &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    IrUsMapPlugIn& operator=(const IrUsMapPlugIn &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param name Name of this plugin.
                     * @param kvc KeyValueConfiguration for this widget.
                     * @param prnt Pointer to the container super window.
                     */
                    IrUsMapPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

                    virtual ~IrUsMapPlugIn();

                    virtual QWidget* getQWidget() const;

                    virtual void setupPlugin();

                    virtual void stopPlugin();

                private:
                    const core::base::KeyValueConfiguration &m_kvc;
                    IrUsMapWidgetControl *m_irusmapWidgetControl;
            };

        }
    }
}

#endif /*COCKPIT_PLUGINS_IRUSMAP_IRUSMAPPLUGIN_H_*/

