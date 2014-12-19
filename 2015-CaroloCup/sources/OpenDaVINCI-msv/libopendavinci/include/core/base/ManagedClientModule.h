/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_BASE_MANAGEDCLIENTMODULE_H_
#define OPENDAVINCI_BASE_MANAGEDCLIENTMODULE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Breakpoint.h"
#include "core/base/ClientModule.h"
#include "core/data/TimeStamp.h"
#include "core/exceptions/Exceptions.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This class manages the local module:
         *  - unsupervised distributed execution
         *  - supervised distributed execution
         *
         * @See ConferenceClientModule
         */
        class OPENDAVINCI_API ManagedClientModule : public core::base::ClientModule, public core::base::Breakpoint
        {
            private:
                friend class ConferenceClientModule;

                /**
                 * Private constructor to not allow any other subclasses than ConferenceClientModule to instantiate an object.
                 *
                 * @param argc Number of command line arguments.
                 * @param argv Command line arguments.
                 * @param name Name of this module. This parameter is necessary for identifying the corresponding parts in the configuration.
                 * @throw InvalidArgumentException if the signal handler could not be registered.
                 */
                ManagedClientModule(const int32_t &argc, char **argv, const string &name) throw (core::exceptions::InvalidArgumentException);

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ManagedClientModule(const ManagedClientModule&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ManagedClientModule& operator=(const ManagedClientModule&);

            public:
                virtual ~ManagedClientModule();

            protected:
                /**
                 * This method is called right before the body is executed.
                 */
                virtual void setUp() = 0;

                /**
                 * This method is called after returning from body.
                 */
                virtual void tearDown() = 0;

                /**
                 * This method contains the real module body.
                 *
                 * @return The exit code of the real body.
                 */
                virtual core::base::ModuleState::MODULE_EXITCODE body() = 0;

                virtual core::base::ModuleState::MODULE_EXITCODE runModuleImplementation();

                virtual void reached();

            private:
                virtual void wait();

                virtual void DMCPconnectionLost();

                core::base::ModuleState::MODULE_EXITCODE runModuleImplementation_ManagedLevel_None();
                void wait_ManagedLevel_None();

                core::base::ModuleState::MODULE_EXITCODE runModuleImplementation_ManagedLevel_Pulse();
                void wait_ManagedLevel_Pulse();
                void reached_ManagedLevel_Pulse();

                core::base::ModuleState::MODULE_EXITCODE runModuleImplementation_ManagedLevel_Pulse_Shift();
                void wait_ManagedLevel_Pulse_Shift();
                void reached_ManagedLevel_Pulse_Shift();

                /**
                 * This method is used to log the time consumption (load)
                 * for this module into a profiling file.
                 */
                void logProfilingData(const core::data::TimeStamp &current, const core::data::TimeStamp &lastCycle, const float &freq, const long &lastWaitTime, const long &timeConsumptionCurrent, const long &nominalDuration, const long &waitingTimeCurrent, const int32_t &cycleCounter);

            private:
                core::data::TimeStamp m_lastCycle;
                long m_lastWaitTime;
                int32_t m_cycleCounter;
                ofstream *m_profilingFile;

                bool m_firstCallToBreakpoint_ManagedLevel_Pulse;
        };

    }
} // core::base

#endif /*OPENDAVINCI_BASE_MANAGEDCLIENTMODULE_H_*/
