/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_CUTTERWIDGET_H_
#define COCKPIT_PLUGINS_CUTTERWIDGET_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <iostream>
#include <map>
#include <string>

#include "core/data/Container.h"
#include "core/io/ContainerListener.h"

#include "plugins/PlugIn.h"
#include "QtIncludes.h"

namespace cockpit {

    namespace plugins {

        namespace cutter {

            using namespace std;
            using namespace core::data;

            /**
             * This class is the container for the cutter widget.
             */
            class CutterWidget : public QWidget {

                    Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    CutterWidget(const CutterWidget &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    CutterWidget& operator=(const CutterWidget &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param prnt Pointer to the parental widget.
                     */
                    CutterWidget(const PlugIn &plugIn, QWidget *prnt);

                    virtual ~CutterWidget();

                public slots:
                    void loadFile();
                    void saveFile();

                private:
                    QListWidget *m_containerWidget;
                    QLabel *m_desc;
                    map<uint32_t, string> m_mapOfContainers;
                    map<uint32_t, string> m_mapOfSelectedContainers;
                    istream *m_in;
                    uint32_t m_numberOfCounters;

            };

        }
    }
}

#endif /* COCKPIT_PLUGINS_CUTTERWIDGET_H_ */
