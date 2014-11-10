/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_IRUSMAP_IRUSMAPWIDGET_H_
#define COCKPIT_PLUGINS_IRUSMAP_IRUSMAPWIDGET_H_

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
#include "plugins/irusmap/PointSensor.h"

#include "SensorBoardData.h"

namespace cockpit {

    namespace plugins {

        namespace irusmap {

            using namespace std;

            /**
             * This class is the container for the irus map widget.
             */
            class IrUsMapWidget : public QWidget, public core::io::ContainerListener {

                    Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    IrUsMapWidget(const IrUsMapWidget &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    IrUsMapWidget& operator=(const IrUsMapWidget &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param kvc KeyValueConfiguration for this based widget.
                     * @param prnt Pointer to the parental widget.
                     */
                    IrUsMapWidget(const PlugIn &plugIn, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

                    virtual ~IrUsMapWidget();

                    virtual void nextContainer(core::data::Container &c);

                    void setScale(const int &val);

                    void stopTimer();

                private:
                    virtual void paintEvent(QPaintEvent *evnt);

                private:
                    QTimer *m_timer;

                    core::base::Mutex m_scaleFactorMutex;
                    double m_scaleFactor;
                    double m_rotation;

                    map<string, PointSensor*> m_mapOfPointSensors;

                    core::base::Mutex m_sensorBoardDataMutex;
                    msv::SensorBoardData m_sensorBoardData;
            };

        }
    }
}

#endif /*COCKPIT_PLUGINS_IRUSMAP_IRUSMAPWIDGET_H_*/

