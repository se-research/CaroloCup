/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_IRUSMAP_IRUSMAPWIDGETCONTROL_H_
#define COCKPIT_PLUGINS_IRUSMAP_IRUSMAPWIDGETCONTROL_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <map>
#include <string>
#include <vector>

#include "QtIncludes.h"

#include "core/base/Mutex.h"
#include "core/io/ContainerListener.h"

#include "plugins/PlugIn.h"
#include "plugins/irusmap/IrUsMapWidget.h"

namespace cockpit {

    namespace plugins {

        namespace irusmap {

            using namespace std;

            /**
             * This class is the container for the irus map widget control.
             */
            class IrUsMapWidgetControl : public QWidget, public core::io::ContainerListener {

                    Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    IrUsMapWidgetControl(const IrUsMapWidgetControl &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    IrUsMapWidgetControl& operator=(const IrUsMapWidgetControl &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param kvc KeyValueConfiguration for this based widget.
                     * @param prnt Pointer to the parental widget.
                     */
                    IrUsMapWidgetControl(const PlugIn &plugIn, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

                    virtual ~IrUsMapWidgetControl();

                    virtual void nextContainer(core::data::Container &c);

                public slots:
                    void setScale(int val);

                private:
                    core::base::Mutex m_mapWidgetMutex;
                    IrUsMapWidget *m_mapWidget;
            };

        }
    }
}

#endif /*COCKPIT_PLUGINS_IRUSMAP_IRUSMAPWIDGETCONTROL_H_*/

