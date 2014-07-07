/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_H_
#define COCKPIT_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "QtIncludes.h"

#include "core/base/ConferenceClientModule.h"

namespace cockpit {

    using namespace std;

    /**
     * This class demonstrates how to send data as a Container.
     */
    class Cockpit : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Cockpit(const Cockpit &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Cockpit& operator=(const Cockpit &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Cockpit(int32_t &argc, char **argv);

            virtual ~Cockpit();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();

            virtual void tearDown();

        private:
            QApplication m_cockpitApp;
    };

} // cockpit

#endif /*COCKPIT_H_*/
