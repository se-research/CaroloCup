/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_SPYWIDGET_H_
#define COCKPIT_PLUGINS_SPYWIDGET_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <map>
#include <cstring>

#include "core/data/Container.h"
#include "core/io/ContainerListener.h"

#include "plugins/PlugIn.h"
#include "QtIncludes.h"

namespace cockpit {

    namespace plugins {

        namespace spy {

            using namespace std;
            using namespace core::data;

            /**
             * This class is the container for the spy widget.
             */
            class SpyWidget : public QWidget, public core::io::ContainerListener {

                    Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    SpyWidget(const SpyWidget &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    SpyWidget&
                    operator=(const SpyWidget &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param prnt Pointer to the parental widget.
                     */
                    SpyWidget(const PlugIn &plugIn, QWidget *prnt);

                    virtual ~SpyWidget();

                    virtual void nextContainer(Container &c);

                private:
                    QTreeWidget* m_dataView;
                    map<Container::DATATYPE, QTreeWidgetItem* > m_dataToType;

                    string DataToString(Container &container);
            };

        }
    }
}

#endif /* COCKPIT_PLUGINS_SPYWIDGET_H_ */
