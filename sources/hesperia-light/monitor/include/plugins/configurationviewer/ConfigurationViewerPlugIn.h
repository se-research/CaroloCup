/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CONFIGURATIONVIEWER_H_
#define CONFIGURATIONVIEWER_H_

#include "plugins/PlugIn.h"
#include "plugins/configurationviewer/ConfigurationViewerWidget.h"

namespace plugins {
    namespace configurationviewer {

      class ConfigurationViewerPlugIn : public PlugIn {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            ConfigurationViewerPlugIn(const ConfigurationViewerPlugIn &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            ConfigurationViewerPlugIn& operator=(const ConfigurationViewerPlugIn &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param name Name of this plugin.
             * @param kvc KeyValueConfiguration for this based widget.
             * @param prnt Pointer to the container super window.
             */
            ConfigurationViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

            virtual ~ConfigurationViewerPlugIn();

            virtual QWidget* getQWidget() const;

            virtual void setupPlugin();

            virtual void stopPlugin();

        private:
            ConfigurationViewerWidget *m_viewerWidget;
        };

    }
}

#endif /* CONFIGURATIONVIEWER_H_ */
