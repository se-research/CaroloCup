/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "QtIncludes.h"

#include "core/base/KeyValueConfiguration.h"

#include "MonitorWindow.h"
#include "Monitor.h"

namespace monitor {

    using namespace std;
    using namespace core::base;
    using namespace core::data;

    Monitor::Monitor(int32_t &argc, char **argv) :
            ConferenceClientModule(argc, argv, "Monitor"),
            m_monitorApp(argc, argv) {}

    Monitor::~Monitor() {}

    void Monitor::setUp() {}

    void Monitor::tearDown() {}

    void Monitor::wait() {
        AbstractModule::wait();
    }

    ModuleState::MODULE_EXITCODE Monitor::body() {
        KeyValueConfiguration kvc = getKeyValueConfiguration();

        // Get apropriate translator.
        QTranslator qtTranslator;
        std::cerr << QLocale::system().name().toStdString() << std::endl;
        qtTranslator.load("monitor_" + QLocale::system().name());
        m_monitorApp.installTranslator(&qtTranslator);

        // Construct main application and providing ourselves as datastoremanager.
        MonitorWindow mainWindow(kvc, (*this));
        mainWindow.show();
        m_monitorApp.exec();

        return ModuleState::OKAY;
    }

} // monitor
