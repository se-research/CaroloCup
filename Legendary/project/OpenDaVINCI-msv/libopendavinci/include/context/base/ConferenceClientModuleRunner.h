/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_CONFERENCECLIENTMODULERUNNER_H_
#define CONTEXT_BASE_CONFERENCECLIENTMODULERUNNER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/Mutex.h"
#include "core/base/Service.h"
#include "core/wrapper/Time.h"
#include "core/base/ConferenceClientModule.h"
#include "context/base/BlockableContainerListener.h"
#include "context/base/RunModuleBreakpoint.h"
#include "context/base/Runner.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class encapsulates a given ConferenceClientModule into an
         * independent thread which is controlled by the given breakpoint
         * created by this class.
         */
        class OPENDAVINCI_API ConferenceClientModuleRunner : public Runner, public core::base::Service {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ConferenceClientModuleRunner(const ConferenceClientModuleRunner&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ConferenceClientModuleRunner& operator=(const ConferenceClientModuleRunner&);

            public:
                /**
                 * Constructor.
                 *
                 * @param ccm ConferenceClientModule which should be executed.
                 */
                ConferenceClientModuleRunner(core::base::ConferenceClientModule &ccm);

                virtual ~ConferenceClientModuleRunner();

                /**
                 * This method actually performs a step (i.e. executes
                 * exactly one cycle between to consecutive getModuleState()-
                 * calls).
                 *
                 * @param t Time.
                 */
                virtual void step(const core::wrapper::Time &t);

                virtual bool hasFinished() const;

            protected:
                virtual void beforeStop();

                virtual void run();

            private:
                virtual float getFrequency() const;

            private:
                bool m_conferenceClientModuleStarted;

                mutable core::base::Mutex m_conferenceClientModuleFinishedMutex;
                bool m_conferenceClientModuleFinished;

                core::base::ConferenceClientModule &m_conferenceClientModule;
                BlockableContainerListener &m_blockableContainerListener;
                RunModuleBreakpoint m_runModuleBreakpoint;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_CONFERENCECLIENTMODULERUNNER_H_*/
