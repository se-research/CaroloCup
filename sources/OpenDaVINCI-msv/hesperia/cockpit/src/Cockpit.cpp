/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "Cockpit.h"
#include "CockpitWindow.h"

namespace cockpit {

    using namespace std;
    using namespace core::base;

    Cockpit::Cockpit(int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "Cockpit"),
        m_cockpitApp(argc, argv)
	{}

    Cockpit::~Cockpit() {}

    void Cockpit::setUp() {
        // This method will be call automatically _before_ running body().
    }

    void Cockpit::tearDown() {
        // This method will be call automatically _after_ return from body().
    }

    ModuleState::MODULE_EXITCODE Cockpit::body() {
        // Get apropriate translator.
        QTranslator qtTranslator;
        std::cerr << QLocale::system().name().toStdString() << std::endl;
        qtTranslator.load("cockpit_" + QLocale::system().name());
        m_cockpitApp.installTranslator(&qtTranslator);

        // Create main cockpit.
        CockpitWindow  mainWindow(getKeyValueConfiguration(), (*this), getConference());
        mainWindow.resize(800, 600);

        // Center window.
        QSize size;
        size = mainWindow.size();

        QDesktopWidget *d = QApplication::desktop();
        int ws = d->width();   // returns screen width
        int h = d->height();  // returns screen height
        int mw = size.width();
        int mh = size.height();
        int cw = (ws/2) - (mw/2);
        int ch = (h/2) - (mh/2);
        mainWindow.move(cw,ch);

        mainWindow.setWindowTitle("OpenDaVINCI Cockpit");

        mainWindow.show();
        m_cockpitApp.exec();

    	return ModuleState::OKAY;
    }

} // cockpit

