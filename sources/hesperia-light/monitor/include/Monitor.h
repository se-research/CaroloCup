/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MONITOR_MONITOR_H_
#define MONITOR_MONITOR_H_

#include "QtIncludes.h"

#include "hesperia/base/ConferenceClientModule.h"

namespace monitor {

    using namespace std;

    /**
     * This is the main class for a monitor.
     */
    class Monitor : public hesperia::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            Monitor(const Monitor &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            Monitor& operator=(const Monitor &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Monitor(int32_t &argc, char **argv);

            virtual ~Monitor();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            QApplication m_monitorApp;

            virtual void wait();
            virtual void setUp();
            virtual void tearDown();
    };

} // monitor

#endif /*MONITOR_MONITOR_H_*/
