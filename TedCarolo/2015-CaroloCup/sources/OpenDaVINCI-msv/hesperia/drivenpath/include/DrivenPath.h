/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#ifndef DRIVENPATH_H_
#define DRIVENPATH_H_

#include "core/base/ConferenceClientModule.h"

namespace measurements {

    using namespace std;

    /**
     * This class encapsulates the a measurement module to measure the driven vs. the optimal path.
     */
    class DrivenPath : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            DrivenPath(const DrivenPath &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            DrivenPath& operator=(const DrivenPath &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            DrivenPath(const int32_t &argc, char **argv);

            virtual ~DrivenPath();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();

            virtual void tearDown();
    };

} // measurements

#endif /*DRIVENPATH_H_*/
