/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PROXY_H_
#define PROXY_H_

#include <map>

#include "core/base/ConferenceClientModule.h"
#include "core/data/Container.h"
#include "tools/recorder/Recorder.h"



#include "Camera.h"

namespace msv {

    using namespace std;

    /**
     * This class wraps the software/hardware interface board.
     */
    class Proxy : public core::base::ConferenceClientModule, public core::wrapper::StringListener, public core::wrapper::ConnectionListener{
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Proxy(const Proxy &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Proxy& operator=(const Proxy &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Proxy(const int32_t &argc, char **argv);

            virtual ~Proxy();

            virtual void handleConnectionError();

            virtual void nextString(const string &s);

            string encodeNetstring(const string &d);

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();

            virtual void tearDown();


        private:
            tools::recorder::Recorder *m_recorder;
            Camera *m_camera;
    };

} // msv

#endif /*PROXY_H_*/
