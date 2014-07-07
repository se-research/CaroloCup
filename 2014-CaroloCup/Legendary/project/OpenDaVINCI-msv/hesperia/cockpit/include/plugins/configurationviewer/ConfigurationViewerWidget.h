/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONFIGURATIONVIEWERWIDGET_H_
#define CONFIGURATIONVIEWERWIDGET_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/base/KeyValueConfiguration.h"

#include "QtIncludes.h"

#include "plugins/PlugIn.h"

namespace cockpit {

    namespace plugins {

      namespace configurationviewer {

          using namespace std;

            /**
             * This class is the container for the configuration viewer widget.
             */
            class ConfigurationViewerWidget : public QWidget {

                Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ConfigurationViewerWidget(const ConfigurationViewerWidget &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ConfigurationViewerWidget& operator=(const ConfigurationViewerWidget &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param kvc KeyValueConfiguration for this based widget.
                     * @param prnt Pointer to the parental widget.
                     */
                    ConfigurationViewerWidget(const PlugIn &plugIn, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

                    virtual ~ConfigurationViewerWidget();

                private:
                    QTextEdit *m_configurationDataText;
            };
      }
    }
}

#endif /*CONFIGURATIONVIEWERWIDGET_H_*/

