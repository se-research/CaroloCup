/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/macros.h"
#include "core/base/Lock.h"

#include "QtIncludes.h"

#include "plugins/PlugInProvider.h"

#include "plugins/configurationviewer/ConfigurationViewerPlugIn.h"
#include "plugins/controller/ControllerPlugIn.h"
#include "plugins/cutter/CutterPlugIn.h"
#include "plugins/forcecontrolviewer/ForceControlViewerPlugIn.h"
#include "plugins/iruscharts/IrUsChartsPlugIn.h"
#include "plugins/irusmap/IrUsMapPlugIn.h"
#include "plugins/sharedimageviewer/SharedImageViewerPlugIn.h"
#include "plugins/spy/SpyPlugIn.h"

namespace cockpit {

    namespace plugins {

        using namespace std;
        using namespace core::base;
        using namespace core::io;

        // Initialize singleton instance.
        Mutex PlugInProvider::m_singletonMutex;
        PlugInProvider* PlugInProvider::m_singleton = NULL;

        PlugInProvider::PlugInProvider(const KeyValueConfiguration &kvc, DataStoreManager &dsm, ContainerConference &conf, QWidget *prnt) :
            QObject(prnt),
            m_listOfAvailablePlugIns(),
            m_listOfDescriptions(),
            m_kvc(kvc),
            m_dataStoreManager(dsm),
            m_conference(conf),
            m_parent(prnt) {
            // TODO: Read available plugins from .so-files.
            m_listOfAvailablePlugIns.push_back("ConfigurationViewer");
            m_listOfAvailablePlugIns.push_back("Controller");
            m_listOfAvailablePlugIns.push_back("Cutter");
#ifndef PANDABOARD
            m_listOfAvailablePlugIns.push_back("ForceControlViewer");
            m_listOfAvailablePlugIns.push_back("IrUsCharts");
#endif
            m_listOfAvailablePlugIns.push_back("IrUsMap");
            m_listOfAvailablePlugIns.push_back("SharedImageViewer");
            m_listOfAvailablePlugIns.push_back("Spy");

            m_listOfDescriptions["ConfigurationViewer"] = tr("This plugin displays the current configuration.").toStdString();
            m_listOfDescriptions["Controller"] = tr("This plugin allows the control of the vehicle by the arrow keys.").toStdString();
            m_listOfDescriptions["Cutter"] = tr("This plugin allows to filter & cut recordings.").toStdString();
#ifndef PANDABOARD
            m_listOfDescriptions["ForceControlViewer"] = tr("This plugin displays the values of ForceControl over time.").toStdString();
            m_listOfDescriptions["IrUsCharts"] = tr("This plugin displays the values of SensorBoardData over time.").toStdString();
#endif
            m_listOfDescriptions["IrUsMap"] = tr("This plugin displays the current irus readings.").toStdString();
            m_listOfDescriptions["SharedImageViewer"] = tr("This plugin displays shared images.").toStdString();
            m_listOfDescriptions["Spy"] = tr("This plugin displays all distributed containers.").toStdString();
        }

        PlugInProvider::~PlugInProvider() {
            PlugInProvider::m_singleton = NULL;
        }

        PlugInProvider& PlugInProvider::getInstance(const KeyValueConfiguration &kvc, DataStoreManager &dsm, ContainerConference &conf, QWidget *prnt) {
            {
                Lock l(PlugInProvider::m_singletonMutex);
                if (PlugInProvider::m_singleton == NULL) {
                    PlugInProvider::m_singleton = new PlugInProvider(kvc, dsm, conf, prnt);
                }
            }

            return (*PlugInProvider::m_singleton);
        }

        const vector<string> PlugInProvider::getListOfAvailablePlugIns() const {
            return m_listOfAvailablePlugIns;
        }

        string PlugInProvider::getDescriptionForPlugin(const string &pluginName) {
          return m_listOfDescriptions[pluginName];
        }

        core::SharedPointer<PlugIn> PlugInProvider::createPlugIn(const string &name) {
            core::SharedPointer<PlugIn> plugIn;

            if (name == "ConfigurationViewer") {
                cerr << "Creating ConfigurationViewer" << endl;
                plugIn = core::SharedPointer<PlugIn>(new configurationviewer::ConfigurationViewerPlugIn("ConfigurationViewer", m_kvc, m_parent));
            } else if (name == "Controller") {
                cerr << "Creating Controller" << endl;
                plugIn = core::SharedPointer<PlugIn>((PlugIn*)(new controller::ControllerPlugIn("Controller", m_kvc, m_conference, m_parent)));
            } else if (name == "Cutter") {
                cerr << "Creating Cutter" << endl;
                plugIn = core::SharedPointer<PlugIn>(new cutter::CutterPlugIn("Cutter", m_kvc, m_parent));
            }
#ifndef PANDABOARD
            else if (name == "ForceControlViewer") {
                cerr << "Creating ForceControlViewer" << endl;
                plugIn = core::SharedPointer<PlugIn>(new forcecontrolviewer::ForceControlViewerPlugIn("ForceControlViewer", m_kvc, m_parent));
            }
            else if (name == "IrUsCharts") {
                cerr << "Creating IrUsCharts" << endl;
                plugIn = core::SharedPointer<PlugIn>(new iruscharts::IrUsChartsPlugIn("IrUsCharts", m_kvc, m_parent));
            }
#endif
            else if (name == "IrUsMap") {
                cerr << "Creating IrUsMap" << endl;
                plugIn = core::SharedPointer<PlugIn>(new irusmap::IrUsMapPlugIn("IrUsMap", m_kvc, m_parent));
            } else if (name == "SharedImageViewer") {
                cerr << "Creating SharedImageViewer" << endl;
                plugIn = core::SharedPointer<PlugIn>(new sharedimageviewer::SharedImageViewerPlugIn("SharedImageViewer", m_kvc, m_parent));
            } else if (name == "Spy") {
                cerr << "Creating Spy" << endl;
                plugIn = core::SharedPointer<PlugIn>(new spy::SpyPlugIn("Spy", m_kvc, m_parent));
            }

            return plugIn;
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
    }
}

