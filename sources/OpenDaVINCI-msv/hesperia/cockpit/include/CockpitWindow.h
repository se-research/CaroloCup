/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPITWINDOW_H_
#define COCKPITWINDOW_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <vector>

#include "QtIncludes.h"

#include "core/base/KeyValueConfiguration.h"
#include "core/base/DataStoreManager.h"
#include "core/io/ContainerConference.h"

#include "FIFOMultiplexer.h"
#include "plugins/PlugIn.h"
#include "plugins/PlugInProvider.h"

namespace cockpit {

    using namespace std;

    class CockpitWindow: public QMainWindow {

            Q_OBJECT

        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            CockpitWindow(const CockpitWindow &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            CockpitWindow& operator=(const CockpitWindow &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param kvc KeyValueConfiguration.
             * @param dsm DataStoreManager to be used for adding DataStores.
             * @param conf Container conference to send data to.
             */
            CockpitWindow(const core::base::KeyValueConfiguration &kvc, core::base::DataStoreManager &dsm, core::io::ContainerConference &conf);

            virtual ~CockpitWindow();

        public slots:
            void close();
            void maximizeActiveSubWindow();
            void minimizeActiveSubWindow();
            void resetActiveSubWindow();
            void showPlugIn(QListWidgetItem *item);

        private:
            void constructLayout();

        private:
            core::base::KeyValueConfiguration m_kvc;
            core::base::DataStoreManager &m_dataStoreManager;
            FIFOMultiplexer *m_multiplexer;
            cockpit::plugins::PlugInProvider &m_plugInProvider;
            vector<core::SharedPointer<cockpit::plugins::PlugIn> > m_listOfPlugIns;

            QMdiArea *m_cockpitArea;
            QMenu *m_fileMenu;
            QMenu *m_windowMenu;
            QListWidget *m_availablePlugInsList;
    };
}

#endif /* COCKPITWINDOW_H_ */
