/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MONITORWINDOW_H_
#define MONITORWINDOW_H_


#include <vector>

#include "QtIncludes.h"

#include "core/SharedPointer.h"
#include "core/base/KeyValueConfiguration.h"
#include "hesperia/base/DataStoreManager.h"

#include "FIFOMultiplexer.h"
#include "plugins/PlugIn.h"
#include "plugins/PlugInProvider.h"

namespace monitor {

    using namespace std;

    class MonitorWindow: public QMainWindow {
            Q_OBJECT

        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            MonitorWindow(const MonitorWindow &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            MonitorWindow& operator=(const MonitorWindow &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param kvc KeyValueConfiguration.
             * @param dsm DataStoreManager to be used for adding DataStores.
             * @param prnt Pointer to the containing super window.
             */
            MonitorWindow(const core::base::KeyValueConfiguration &kvc, hesperia::base::DataStoreManager &dsm);

            virtual ~MonitorWindow();

        public slots:
            void maximizeActiveSubWindow();
            void minimizeActiveSubWindow();
            void resetActiveSubWindow();

            void showPlugIn(QListWidgetItem *item);

            void close();


        private:
            core::base::KeyValueConfiguration m_kvc;
            hesperia::base::DataStoreManager &m_dataStoreManager;
            FIFOMultiplexer* m_multiplexer;
            plugins::PlugInProvider &m_plugInProvider;
            vector<core::SharedPointer<plugins::PlugIn> > m_listOfPlugIns;

            void constructLayout();

            QMdiArea* m_monitorArea;
            QMenu* m_fileMenu;
            QMenu* m_windowMenu;
            QListWidget* m_availablePlugInsList;
    };
}

#endif /* MONITORWINDOW_H_ */
