/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"
#include "core/base/Lock.h"

#include "QtIncludes.h"

#include "plugins/PlugInProvider.h"
#include "plugins/MasterPlugIn.h"

#include "plugins/configurationviewer/ConfigurationViewerPlugIn.h"
#include "plugins/environmentviewer/EnvironmentViewerPlugIn.h"
#include "plugins/modulestatisticsviewer/ModuleStatisticsViewerPlugIn.h"
#include "plugins/objxviewer/OBJXViewerPlugIn.h"
#include "plugins/packetlogviewer/PacketLogViewerPlugIn.h"
#include "plugins/scnxviewer/SCNXViewerPlugIn.h"
#include "plugins/sharedimageviewer/SharedImageViewerPlugIn.h"
#include "plugins/surroundingsviewer/SurroundingsViewerPlugIn.h"
#include "plugins/timeControl/TimeControlPlugIn.h"

namespace plugins {

    using namespace std;
    using namespace core::base;
    using namespace hesperia::base;

    // Initialize singleton instance.
    Mutex PlugInProvider::m_singletonMutex;
    PlugInProvider* PlugInProvider::m_singleton = NULL;

    PlugInProvider::PlugInProvider(const KeyValueConfiguration &kvc, DataStoreManager &dsm, QWidget *prnt) :
        QObject(prnt),
        m_listOfAvailablePlugIns(),
        m_listOfAvailableMasterPlugIns(),
        m_listOfDescriptions(),
        m_ActiveMasterPlugIn(),
        m_kvc(kvc),
        m_dataStoreManager(dsm),
        m_parent(prnt)
     {
        // TODO: Read from file system as .so-files.
        m_listOfAvailablePlugIns.push_back("ConfigurationViewer");
        m_listOfAvailablePlugIns.push_back("EnvironmentViewer");
        m_listOfAvailablePlugIns.push_back("ModuleStatisticsViewer");
        m_listOfAvailablePlugIns.push_back("OBJXViewer");
        m_listOfAvailablePlugIns.push_back("PacketLogViewer");
        m_listOfAvailablePlugIns.push_back("SCNXViewer");
        m_listOfAvailablePlugIns.push_back("SharedImageViewer");
        m_listOfAvailablePlugIns.push_back("SurroundingsViewer");

        // TODO: Schoener waere es, wenn das PlugIn diese  Informationen liefert. Leider existiert zu diesem Zeit noch keine Instanz...
        m_listOfDescriptions["ConfigurationViewer"] = tr("This plugin shows the current configuration.").toStdString();
        m_listOfDescriptions["EnvironmentViewer"] = tr("This plugin shows the entire environment in 3D.").toStdString();
        m_listOfDescriptions["ModuleStatisticsViewer"] = tr("This plugin shows module statistics.").toStdString();
        m_listOfDescriptions["OBJXViewer"] = tr("This plugin shows .objx files.").toStdString();
        m_listOfDescriptions["PacketLogViewer"] = tr("This plugin shows all distributed containers.").toStdString();
        m_listOfDescriptions["SCNXViewer"] = tr("This plugin shows SCNX-Archives.").toStdString();
        m_listOfDescriptions["SharedImageViewer"] = tr("This plugin shows shared images.").toStdString();
        m_listOfDescriptions["SurroundingsViewer"] = tr("EXPERIMENTAL (!!) plugin for viewing either 3D or 2D.").toStdString();

        m_listOfAvailableMasterPlugIns.push_back("TimeControl");
    }

    PlugInProvider::~PlugInProvider() {
        PlugInProvider::m_singleton = NULL;
    }

    PlugInProvider& PlugInProvider::getInstance(const KeyValueConfiguration &kvc, DataStoreManager &dsm, QWidget *prnt) {
        {
            Lock l(PlugInProvider::m_singletonMutex);
            if (PlugInProvider::m_singleton == NULL) {
                PlugInProvider::m_singleton = new PlugInProvider(kvc, dsm, prnt);
            }
        }

        return (*PlugInProvider::m_singleton);
    }

    const vector<string> PlugInProvider::getListOfAvailablePlugIns() const {
        return m_listOfAvailablePlugIns;
    }

    const vector<string> PlugInProvider::getListOfAvailableMasterPlugIns() const {
       return m_listOfAvailableMasterPlugIns;
    }

    string PlugInProvider::getDescriptionForPlugin(const string &pluginName) {
      return m_listOfDescriptions[pluginName];
    }

    core::SharedPointer<PlugIn> PlugInProvider::createPlugIn(const string &name) {
        core::SharedPointer<PlugIn> plugIn;

        if (name == "ConfigurationViewer") {
            cerr << "Creating ConfigurationViewer" << endl;
            plugIn = core::SharedPointer<PlugIn>(new configurationviewer::ConfigurationViewerPlugIn("ConfigurationViewer", m_kvc, m_parent));
        } else if (name == "OBJXViewer") {
            cerr << "Creating OBJXViewer" << endl;
            plugIn = core::SharedPointer<PlugIn>(new objxviewer::OBJXViewerPlugIn("OBJXViewer", m_kvc, m_parent));
        } else if (name == "PacketLogViewer") {
            cerr << "Creating PacketLogViewer" << endl;
            plugIn = core::SharedPointer<PlugIn>(new packetlogviewer::PacketLogViewerPlugIn("PacketLogViewer", m_kvc, m_parent));
        } else if (name == "SCNXViewer") {
            cerr << "Creating SCNXViewer" << endl;
            plugIn = core::SharedPointer<PlugIn>(new scnxviewer::SCNXViewerPlugIn("SCNXViewer", m_kvc, m_parent));
        } else if (name == "ModuleStatisticsViewer") {
            cerr << "Creating ModuleStatisticsViewer" << endl;
            plugIn = core::SharedPointer<PlugIn>(new modulestatisticsviewer::ModuleStatisticsViewerPlugIn("ModuleStatisticsViewer", m_kvc, m_parent));
        } else if (name == "SharedImageViewer") {
            cerr << "Creating SharedImageViewer" << endl;
            plugIn = core::SharedPointer<PlugIn>(new sharedimageviewer::SharedImageViewerPlugIn("SharedImageViewer", m_kvc, m_parent));
        } else if (name == "EnvironmentViewer") {
            cerr << "Creating EnvironmentViewer" << endl;
            plugIn = core::SharedPointer<PlugIn>(new environmentviewer::EnvironmentViewerPlugIn("EnvironmentViewer", m_kvc, m_parent));
        } else if (name == "SurroundingsViewer") {
            cerr << "Creating SurroundingsViewer" << endl;
            plugIn = core::SharedPointer<PlugIn>(new surroundingsviewer::SurroundingsViewerPlugIn("SurroundingsViewer", m_kvc, m_parent));
        }

        return plugIn;
    }

    core::SharedPointer<MasterPlugIn> PlugInProvider::createMasterPlugIn(const string &name) const {
        core::SharedPointer<MasterPlugIn> masterPlugIn;

        if (name == "TimeControl") {
            cerr << "Creating TimeControl" << endl;
            masterPlugIn = core::SharedPointer<MasterPlugIn>(new timeControl::TimeControlPlugIn("TimeControl", m_kvc, m_dataStoreManager, m_parent));
        }

        return masterPlugIn;
    }

    core::SharedPointer<PlugIn> PlugInProvider::getPlugIn(const string &name) {
        core::SharedPointer<PlugIn> plugIn;

        // Check if the plugin exists.
        vector<string>::const_iterator it = m_listOfAvailablePlugIns.begin();
        while (it != m_listOfAvailablePlugIns.end()) {
            if ((*it) == name) {
                break;
            }
            it++;
        }

        if (it != m_listOfAvailablePlugIns.end()) {
            plugIn = createPlugIn(*it);
        }

        return plugIn;
    }


    core::SharedPointer<MasterPlugIn> PlugInProvider::getMasterPlugIn(const string &name) const {
        core::SharedPointer<MasterPlugIn> masterPlugIn;

        if (!m_ActiveMasterPlugIn.isValid()) {
        // Check if the plugin exists.
        vector<string>::const_iterator it = m_listOfAvailableMasterPlugIns.begin();
        while (it != m_listOfAvailableMasterPlugIns.end()) {
            if ((*it) == name) {
                break;
            }
            it++;
        }

        if (it != m_listOfAvailableMasterPlugIns.end()) {
            masterPlugIn = createMasterPlugIn(*it);
        }

        return masterPlugIn;
        } else {
        	HESPERIA_CORE_THROW_EXCEPTION(MasterPlugInMultiInstanceException, "An instance of a master plugin is already active!");
        }

    }

    void PlugInProvider::stopMasterPlugIn(){
    	if (m_ActiveMasterPlugIn.isValid()) {
    		m_ActiveMasterPlugIn->stopPlugin();
    		m_ActiveMasterPlugIn.release();
    	}
    }

} // plugins
