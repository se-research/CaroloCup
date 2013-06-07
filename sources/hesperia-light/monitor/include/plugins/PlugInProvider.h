/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_PLUGINPROVIDER_H_
#define PLUGINS_PLUGINPROVIDER_H_

#include <string>
#include <vector>
#include <map>

#include "QtIncludes.h"

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/SharedPointer.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/base/Mutex.h"
#include "core/exceptions/Exceptions.h"
#include "hesperia/base/DataStoreManager.h"

#include "plugins/PlugIn.h"
#include "plugins/MasterPlugIn.h"

namespace plugins {

    using namespace std;

    /**
     * This class provides all available plugins.
     */
    class PlugInProvider : public QObject {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            PlugInProvider(const PlugInProvider &);
            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            PlugInProvider& operator=(const PlugInProvider &);

        private:
            /**
             * Constructor.
             *
             * @param kvc KeyValueConfiguration for this GL-based widget.
             * @param dsm DataStoreManager to be used for adding DataStores.
             * @param prnt Pointer to the container super window.
             */
            PlugInProvider(const core::base::KeyValueConfiguration &kvc, hesperia::base::DataStoreManager &dsm, QWidget *prnt);

        public:
            virtual ~PlugInProvider();

            /**
             * This method returns a static instance for this factory.
             *
             * @param kvc KeyValueConfiguration for this widget.
             * @param dsm DataStoreManager to be used for adding DataStores.
             * @param prnt Pointer to the container super window.
             * @return Instance of this factory.
             */
            static PlugInProvider& getInstance(const core::base::KeyValueConfiguration &kvc, hesperia::base::DataStoreManager &dsm, QWidget *prnt);

            /**
             * This method returns the list of available plugins.
             *
             * @return List of available plugins.
             */
            const vector<string> getListOfAvailablePlugIns() const;

            /**
             * This method returns the list of available master plugins.
             *
             * @return List of available master plugins.
             */
            const vector<string> getListOfAvailableMasterPlugIns() const;

            /**
             * This method returns the description for the given Plugin.
             *
             * @param pluginName Name of the Plugin.
             * @return Description.
             */
            string getDescriptionForPlugin(const string &pluginName);

            /**
             * This method returns the plugin for the given name.
             *
             * @param name Name of the plugin.
             * @return Plugin.
             */
            core::SharedPointer<PlugIn> getPlugIn(const string &name);

            /**
             * This method returns the master plugin for the given name.
             *
             * @param name Name of the master plugin.
             * @return master plugin.
             */
            core::SharedPointer<MasterPlugIn> getMasterPlugIn(const string &name) const;

            /**
             * This method stops the current master plugin.
             *
             */
            void stopMasterPlugIn();

        private:
            static core::base::Mutex m_singletonMutex;
            static PlugInProvider* m_singleton;

            vector<string> m_listOfAvailablePlugIns;
            vector<string> m_listOfAvailableMasterPlugIns;
            map<string,string> m_listOfDescriptions;
            core::SharedPointer<MasterPlugIn> m_ActiveMasterPlugIn;
            core::base::KeyValueConfiguration m_kvc;
            hesperia::base::DataStoreManager &m_dataStoreManager;
            QWidget *m_parent;

            /**
             * This method creates a new instance for the given name.
             *
             * @param name Name of the plugin.
             * @return New instance.
             */
            core::SharedPointer<PlugIn> createPlugIn(const string &name);

            /**
             * This method creates a new instance for the given name.
             *
             * @param name Name of the master plugin.
             * @return New instance.
             */
            core::SharedPointer<MasterPlugIn> createMasterPlugIn(const string &name) const;
    };

} // plugins

#endif /*PLUGINS_PLUGINPROVIDER_H_*/
