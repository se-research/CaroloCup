/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CONFIGURATIONVIEWERWIDGET_H_
#define CONFIGURATIONVIEWERWIDGET_H_

#include "core/base/KeyValueConfiguration.h"

#include "QtIncludes.h"

#include "plugins/PlugIn.h"

namespace plugins {
  namespace configurationviewer {

      using namespace std;
      using namespace core::data;

        /**
         * This class is the container for the OBJX viewer widget.
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

#endif /* CONFIGURATIONVIEWERWIDGET_H_ */
