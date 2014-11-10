/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_ECUWIDGET_H_
#define COCKPIT_PLUGINS_ECUWIDGET_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <map>
#include <cstring>

#include "core/io/ContainerConference.h"
#include "core/data/Container.h"
#include "core/io/ContainerListener.h"

#include "plugins/PlugIn.h"
#include "QtIncludes.h"

namespace cockpit {

    namespace plugins {

        namespace ecuviewer {

            using namespace std;
            using namespace core::data;

            /**
             * This class is the container for the ECUViewer widget.
             */
            class ECUWidget : public QWidget, public core::io::ContainerListener {

                    Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ECUWidget(const ECUWidget &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ECUWidget&
                    operator=(const ECUWidget &/*obj*/);

                public slots:
                    void setDesiredData(int v);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param prnt Pointer to the parental widget.
                     */
                    ECUWidget(const PlugIn &plugIn, core::io::ContainerConference &conf, QWidget *prnt);

                    virtual ~ECUWidget();

                    virtual void nextContainer(Container &c);

                private:
                    core::io::ContainerConference &m_conference;

                    QTextEdit *m_rawDataFromECU;

                    QLabel *m_accelX;
                    QLabel *m_accelY;
                    QLabel *m_accelZ;

                    bool m_firstCycle;
            };

        }
    }
}

#endif /* COCKPIT_PLUGINS_ECUWIDGET_H_ */
