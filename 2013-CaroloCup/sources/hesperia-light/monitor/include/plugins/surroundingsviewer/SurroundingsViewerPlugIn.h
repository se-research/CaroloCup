/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERPLUGIN_H_
#define PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERPLUGIN_H_

#include "plugins/PlugIn.h"

#include "plugins/surroundingsviewer/SurroundingsViewerWidget.h"

namespace plugins {
    namespace surroundingsviewer {

        /**
         * This class is surroundings viewer plugin.
         */
        class SurroundingsViewerPlugIn : public PlugIn {

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SurroundingsViewerPlugIn(const SurroundingsViewerPlugIn &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SurroundingsViewerPlugIn& operator=(const SurroundingsViewerPlugIn &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param name Name of this plugin.
                 * @param kvc KeyValueConfiguration for this GL-based widget.
                 * @param prnt Pointer to the container super window.
                 */
                SurroundingsViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget* prnt);

                virtual ~SurroundingsViewerPlugIn();

                virtual QWidget* getQWidget() const;

                virtual void setupPlugin();

                virtual void stopPlugin();

            private:
                SurroundingsViewerWidget *m_widget;
        };

    }
} // plugins::surroundingsviewer

#endif /*PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERPLUGIN_H_*/
