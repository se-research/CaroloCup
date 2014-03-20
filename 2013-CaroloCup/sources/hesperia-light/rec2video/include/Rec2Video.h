/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef REC2VIDEO_H_
#define REC2VIDEO_H_

#include "core/base/Condition.h"
#include "hesperia/base/ConferenceClientModule.h"

#include "Renderer.h"

namespace rec2video {

    using namespace std;

    /**
     * This class converts a given previously recorded data file
     * to a video.
     */
    class Rec2Video : public hesperia::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Rec2Video(const Rec2Video &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Rec2Video& operator=(const Rec2Video &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Rec2Video(const int32_t &argc, char **argv);

            virtual ~Rec2Video();

            virtual void setUp();

            virtual void tearDown();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            core::base::Condition m_conditionRenderer;
            bool m_doRendering;
            Renderer *m_renderer;
    };

} // rec2video

#endif /*REC2VIDEO_H_*/
