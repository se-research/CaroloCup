/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_ENVIRONMENTVIEWER_ENVIRONMENTVIEWERPLUGIN_H_
#define PLUGINS_ENVIRONMENTVIEWER_ENVIRONMENTVIEWERPLUGIN_H_

#include "plugins/PlugIn.h"

#include "plugins/environmentviewer/EnvironmentViewerWidget.h"

namespace cockpit {
    namespace plugins {
        namespace environmentviewer {

            /**
             * This class is environment viewer plugin.
             */
            class EnvironmentViewerPlugIn : public PlugIn {

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    EnvironmentViewerPlugIn(const EnvironmentViewerPlugIn &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    EnvironmentViewerPlugIn& operator=(const EnvironmentViewerPlugIn &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param name Name of this plugin.
                     * @param kvc KeyValueConfiguration for this GL-based widget.
                     * @param prnt Pointer to the container super window.
                     */
                    EnvironmentViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget* prnt);

                    virtual ~EnvironmentViewerPlugIn();

                    virtual QWidget* getQWidget() const;

                    virtual void setupPlugin();

                    virtual void stopPlugin();

                private:
                    EnvironmentViewerWidget *m_widget;
            };
        }
    }
} // plugins::environmentviewer

#endif /*PLUGINS_ENVIRONMENTVIEWER_ENVIRONMENTVIEWERPLUGIN_H_*/
