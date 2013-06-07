/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_PLUGIN_H_
#define PLUGINS_PLUGIN_H_

#include <string>

#include "QtIncludes.h"

#include "core/base/KeyValueConfiguration.h"
#include "core/base/Mutex.h"

#include "ContainerObserver.h"

namespace plugins {

    using namespace std;

    /**
     * This class is the main class for all plugins.
     */
    class PlugIn {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            PlugIn(const PlugIn &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            PlugIn& operator=(const PlugIn &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param name Name of this plugin.
             * @param kvc KeyValueConfiguration for this plugin.
             * @param prnt Pointer to the containing super window.
             */
            PlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget* prnt);

            virtual ~PlugIn();

            /**
             * This method must be overridden in subclasses to setup
             * this plugin, i.e. construct all necessary GUI elements.
             */
            virtual void setupPlugin() = 0;

            /**
             * This method must be overridden in subclasses to stop
             * this plugin right before destruction.
             */
            virtual void stopPlugin() = 0;

            /**
             * This method returns the widget on which all content is drawn.
             *
             * @return Widget on which all content is drawn.
             */
            virtual QWidget* getQWidget() const = 0;

            /**
             * This method returns the container observer or NULL.
             *
             * @return ContainerObserver to be used.
             */
            monitor::ContainerObserver* getContainerObserver() const;

            /**
             * This method sets the container observer to be used.
             *
             * @param containerObserver ContainerObserver to be used.
             */
            void setContainerObserver(monitor::ContainerObserver *containerObserver);

            /**
             * This method returns the name of this plugin.
             *
             * @return Name of this plugin.
             */
            const string getName() const;

            /**
             * This method sets the description of the plugin.
             *
             * @param description Description.
             */
            void setDescription(const string &description);

            /**
             * This method returns a simple description for the
             * plugin.
             *
             * @return Description of this plugin.
             */
            const string getDescription() const;

            /**
             * This method returns the parental widget.
             *
             * @return Parental widget.
             */
            QWidget* getParentQWidget();

            /**
             * This method returns the configuration for this widget.
             *
             * @return KeyValueConfiguration.
             */
            const core::base::KeyValueConfiguration getKeyValueConfiguration() const;

        private:
            QWidget *m_parent;
            const string m_name;
            string m_description;
            const core::base::KeyValueConfiguration &m_kvc;
            mutable core::base::Mutex m_containerObserverMutex;
            monitor::ContainerObserver *m_containerObserver;
    };

} // plugins

#endif /*PLUGINS_PLUGIN_H_*/
