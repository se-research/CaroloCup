/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/data/RuntimeStatistic.h"
#include "core/data/TimeStamp.h"
#include "core/exceptions/Exceptions.h"

#include "core/base/ManagedClientModule.h"

#include "context/base/RuntimeControl.h"

namespace core {
    namespace base {

        using namespace std;

        using namespace core;
        using namespace core::base;
        using namespace core::data;
        using namespace core::exceptions;

        ManagedClientModule::ManagedClientModule(const int32_t &argc, char **argv, const string &name) throw (InvalidArgumentException) :
            ClientModule(argc, argv, name),
            m_lastCycle(),
            m_lastWaitTime(0),
            m_cycleCounter(0),
            m_profilingFile(NULL),
            m_firstCallToBreakpoint_ManagedLevel_Pulse(true),
            m_time(),
            m_controlledTimeFactory(NULL) {}

        ManagedClientModule::~ManagedClientModule() {
            if (m_profilingFile != NULL) {
                m_profilingFile->flush();
                m_profilingFile->close();
            }
            OPENDAVINCI_CORE_DELETE_POINTER(m_profilingFile);
        }

        void ManagedClientModule::DMCPconnectionLost() {}

        void ManagedClientModule::wait() {
            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_NONE) {
                wait_ManagedLevel_None();
            }

            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_PULSE) {
                wait_ManagedLevel_Pulse();
            }

            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_PULSE_SHIFT) {
                wait_ManagedLevel_Pulse_Shift();
            }

            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_PULSE_TIME) {
                wait_ManagedLevel_Pulse_Time();
            }
        }

        ModuleState::MODULE_EXITCODE ManagedClientModule::runModuleImplementation() {
            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_NONE) {
                return runModuleImplementation_ManagedLevel_None();
            }

            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_PULSE) {
                return runModuleImplementation_ManagedLevel_Pulse();
            }

            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_PULSE_SHIFT) {
                return runModuleImplementation_ManagedLevel_Pulse_Shift();
            }

            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_PULSE_TIME) {
                return runModuleImplementation_ManagedLevel_Pulse_Time();
            }

            return ModuleState::OKAY;
        }

        void ManagedClientModule::reached() {
            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_PULSE) {
                reached_ManagedLevel_Pulse();
            }

            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_PULSE_SHIFT) {
                reached_ManagedLevel_Pulse_Shift();
            }

            if (getServerInformation().getManagedLevel() == core::dmcp::ServerInformation::ML_PULSE_TIME) {
                reached_ManagedLevel_Pulse_Time();
            }
        }

        void ManagedClientModule::logProfilingData(const TimeStamp &current, const TimeStamp &lastCycle, const float &freq, const long &lastWaitTime, const long &timeConsumptionCurrent, const long &nominalDuration, const long &waitingTimeCurrent, const int32_t &cycleCounter) {
            if (m_profilingFile == NULL) {
                stringstream sstr;
                sstr << getName() << "_" << TimeStamp().getYYYYMMDD_HHMMSS() << ".profiling.csv";
                m_profilingFile = new ofstream();
                m_profilingFile->open(sstr.str().c_str(), ios::out | ios::app);

                (*m_profilingFile) <<
                    "timestamp_current_cycle" << ";" <<
                    "timestamp_last_cycle" << ";" <<
                    "freq" << ";" <<
                    "last_wait_time" << ";" <<
                    "time_consumption_current" << ";" <<
                    "nominal_duration" << ";" <<
                    "waiting_time_current" << ";" <<
                    "percentage_load_current_slice" << ";" <<
                    "percentage_waiting_current_slice" << ";" <<
                    "cycle_counter" << endl;
            }

            if (m_profilingFile != NULL) {
                (*m_profilingFile) <<
                    current.toMicroseconds() << ";" <<
                    lastCycle.toMicroseconds() << ";" <<
                    freq << ";" <<
                    lastWaitTime << ";" <<
                    timeConsumptionCurrent << ";" <<
                    nominalDuration << ";" <<
                    waitingTimeCurrent << ";" <<
                    (100.0-(waitingTimeCurrent*100.0/((float)nominalDuration))) << ";" <<
                    (waitingTimeCurrent*100.0/((float)nominalDuration)) << ";" <<
                    cycleCounter << endl;
                m_profilingFile->flush();
            }
        }

        ///////////////////////////////////////////////////////////////////////
        // Implementation for managed level none.
        ///////////////////////////////////////////////////////////////////////

        core::base::ModuleState::MODULE_EXITCODE ManagedClientModule::runModuleImplementation_ManagedLevel_None() {
            ModuleState::MODULE_EXITCODE retVal = ModuleState::OKAY;

            try {
                // Setup the module itself.
                setUp();

                setModuleState(ModuleState::RUNNING);
                if (getDMCPClient().isValid()) {
                    getDMCPClient()->sendModuleState(ModuleState::RUNNING);
                }

                // Execute the module's body.
                retVal = body();

                setModuleState(ModuleState::NOT_RUNNING);
                if (getDMCPClient().isValid()) {
                    getDMCPClient()->sendModuleState(ModuleState::NOT_RUNNING);
                }

                // Clean up.
                tearDown();
            } catch (std::exception &e) {
                // Try to catch any exception derived from std::exception and print32_t out reason.
                clog << e.what() << endl;
                retVal = ModuleState::EXCEPTION_CAUGHT;
            } catch (std::string &str) {
                clog << "string exception caught in ClientModule::run()" << endl;
                clog << str << endl;
                retVal = ModuleState::SERIOUS_ERROR;
            } catch (...) {
                // Try to catch anything else print32_t generic error.
                clog << "Unknown exception caught in ClientModule::run()" << endl;
                retVal = ModuleState::SERIOUS_ERROR;
            }

            return retVal;
        }

        uint32_t ManagedClientModule::getWaitingTimeAndUpdateRuntimeStatistics() {
            // Update liveliness.
            m_cycleCounter++;

            // Get "now".
            TimeStamp current;

            const float FREQ = getFrequency();
            const long TIME_CONSUMPTION_OF_CURRENT_SLICE = (current.toMicroseconds() - m_lastCycle.toMicroseconds()) - m_lastWaitTime;

            const long ONE_SECOND_IN_MICROSECONDS = 1000 * 1000 * 1;
            const long NOMINAL_DURATION_OF_ONE_SLICE = static_cast<long>((1.0f/FREQ) * ONE_SECOND_IN_MICROSECONDS);
            const long WAITING_TIME_OF_CURRENT_SLICE = NOMINAL_DURATION_OF_ONE_SLICE - TIME_CONSUMPTION_OF_CURRENT_SLICE;

            // Inform supercomponent about statistical runtime data.
            bool sendStatistics = false;
            if (FREQ < 1) {
                sendStatistics = true;

                // Avoid overflows.
                m_cycleCounter = (m_cycleCounter % 1000);
            }
            else {
				const int32_t CYCLES_PER_SECOND = static_cast<int32_t>(fabs(floor(FREQ + 0.5)));
                if ( (m_cycleCounter % CYCLES_PER_SECOND) == 0 ) {
                    sendStatistics = true;
                    m_cycleCounter = 0;
                }
            }

            // Send RuntimeStatistic to supercomponent.
            if (sendStatistics && getDMCPClient().isValid()) {
                RuntimeStatistic rts;
                rts.setSliceConsumption((float)TIME_CONSUMPTION_OF_CURRENT_SLICE/(float)NOMINAL_DURATION_OF_ONE_SLICE);
                getDMCPClient()->sendStatistics(rts);
            }

            // Check whether we need to save profiling data.
            if (isProfiling()) {
                logProfilingData(current, m_lastCycle, FREQ, m_lastWaitTime, TIME_CONSUMPTION_OF_CURRENT_SLICE, NOMINAL_DURATION_OF_ONE_SLICE, WAITING_TIME_OF_CURRENT_SLICE, m_cycleCounter);
            }

            // Store "now" to m_lastCycle for usage in next cycle.
            m_lastCycle = current;

            // Save the time to be waited.
            if (WAITING_TIME_OF_CURRENT_SLICE > 0) {
                m_lastWaitTime = WAITING_TIME_OF_CURRENT_SLICE;
            }
            else {
                m_lastWaitTime = 0;
            }

            // Return the time to wait.
            return WAITING_TIME_OF_CURRENT_SLICE;
        }

        void ManagedClientModule::wait_ManagedLevel_None() {
            const long WAITING_TIME_OF_CURRENT_SLICE = getWaitingTimeAndUpdateRuntimeStatistics();

            // Enforce waiting to consume the rest of the time slice but ensure that there is no overflow.
            const long ONE_SECOND_IN_MICROSECONDS = 1000 * 1000 * 1;
            if ( (WAITING_TIME_OF_CURRENT_SLICE > 0) && (WAITING_TIME_OF_CURRENT_SLICE < ONE_SECOND_IN_MICROSECONDS) ) {
                Thread::usleep(WAITING_TIME_OF_CURRENT_SLICE);
            }

            if (isVerbose()) {
                clog << "Starting next cycle at " << TimeStamp().toString() << endl;
            }
        }

        ///////////////////////////////////////////////////////////////////////
        // Implementation for managed level pulse.
        ///////////////////////////////////////////////////////////////////////

        void ManagedClientModule::reached_ManagedLevel_Pulse() {
            if (m_firstCallToBreakpoint_ManagedLevel_Pulse) {
                // Align the further execution of a module's body until this realtime clock matches with supercomponent's realtime clock for the required waiting slice.
                setBreakpoint(NULL);
                m_firstCallToBreakpoint_ManagedLevel_Pulse = false;

                // Align the component ten times.
                uint32_t aligner = 10;
                while (aligner-- > 0) {
                    const core::data::dmcp::PulseMessage pm = getDMCPClient()->getPulseMessage();

                    const long ONE_SECOND_IN_MICROSECONDS = 1000 * 1000 * 1;
                    const long ADJUSTMENT_TIME_TO_VIRTUAL_ONE_SECOND = (ONE_SECOND_IN_MICROSECONDS - pm.getCumulatedTimeSlice());

                    const TimeStamp module_now;
                    const long ADJUSTMENT_TIME_TO_ALIGN_REALTIME = (module_now.toMicroseconds() - pm.getRealtimeFromSupercomponent().toMicroseconds());

                    const long ADJUSTMENT_TIME = ADJUSTMENT_TIME_TO_VIRTUAL_ONE_SECOND - ADJUSTMENT_TIME_TO_ALIGN_REALTIME;

                    if (ADJUSTMENT_TIME > 0) {
                        Thread::usleep(ADJUSTMENT_TIME);

                        clog << "(ManagedClientModule) Adjust execution to next timeslice that is aligned with supercomponent: " << ADJUSTMENT_TIME << " microseconds." << endl;
                    }
                }
            }
        }

        core::base::ModuleState::MODULE_EXITCODE ManagedClientModule::runModuleImplementation_ManagedLevel_Pulse() {
            // For the very first call to getModuleState(), delay the sliced execution to the next timeslice.
            m_firstCallToBreakpoint_ManagedLevel_Pulse = true;
            setBreakpoint(this);            

            ModuleState::MODULE_EXITCODE retVal = ModuleState::OKAY;

            try {
                // Setup the module itself.
                setUp();

                setModuleState(ModuleState::RUNNING);
                if (getDMCPClient().isValid()) {
                    getDMCPClient()->sendModuleState(ModuleState::RUNNING);
                }

                // Execute the module's body.
                retVal = body();

                setModuleState(ModuleState::NOT_RUNNING);
                if (getDMCPClient().isValid()) {
                    getDMCPClient()->sendModuleState(ModuleState::NOT_RUNNING);
                }

                // Clean up.
                tearDown();
            } catch (std::exception &e) {
                // Try to catch any exception derived from std::exception and print32_t out reason.
                clog << e.what() << endl;
                retVal = ModuleState::EXCEPTION_CAUGHT;
            } catch (std::string &str) {
                clog << "string exception caught in ClientModule::run()" << endl;
                clog << str << endl;
                retVal = ModuleState::SERIOUS_ERROR;
            } catch (...) {
                // Try to catch anything else print32_t generic error.
                clog << "Unknown exception caught in ClientModule::run()" << endl;
                retVal = ModuleState::SERIOUS_ERROR;
            }

            return retVal;
        }

        void ManagedClientModule::wait_ManagedLevel_Pulse() {
            // No specific implementation for waiting in mode managed_level == pulse.
            wait_ManagedLevel_None();
        }

        ///////////////////////////////////////////////////////////////////////
        // Implementation for managed level pulse shift.
        ///////////////////////////////////////////////////////////////////////

        void ManagedClientModule::reached_ManagedLevel_Pulse_Shift() {
            // No specific implementation for waiting in mode managed_level == pulse_shift.
            reached_ManagedLevel_Pulse();
        }

        core::base::ModuleState::MODULE_EXITCODE ManagedClientModule::runModuleImplementation_ManagedLevel_Pulse_Shift() {
            // No specific implementation for waiting in mode managed_level == pulse_shift 
            return runModuleImplementation_ManagedLevel_Pulse();
        }

        void ManagedClientModule::wait_ManagedLevel_Pulse_Shift() {
            // No specific implementation for waiting in mode managed_level == pulse_shift.
            wait_ManagedLevel_None();
        }

        ///////////////////////////////////////////////////////////////////////
        // Implementation for managed level pulse time (i.e. the execution of
        // this module is suspended unless we receive the next pulse.
        ///////////////////////////////////////////////////////////////////////

        void ManagedClientModule::reached_ManagedLevel_Pulse_Time() {
            // Get next PulseMessage. This call blocks until the next PulseMessage has been received.
            const core::data::dmcp::PulseMessage pm = getDMCPClient()->getPulseMessage();

            // Check whether our clock was already initialized (i.e. getSeconds() > 0).
            if (m_time.now().getSeconds() < 1) {
                // Set seconds of our virtual clock to the clock from supercomponents.
                m_time = context::base::Clock(pm.getRealtimeFromSupercomponent().getSeconds(), 0);
            }

            // Increment the virtual time by the nominal value of the time slice
            // provided by from supercomponent. As PulseMessage provides this
            // value in microseconds, we need to convert to milliseconds.
            const uint32_t INCREMENT_IN_MS = pm.getNominalTimeSlice()/1000.0;
            m_time.increment(INCREMENT_IN_MS);

            // Set the new system time in the TimeFactory.
            m_controlledTimeFactory->setTime(m_time.now());

            // Update the RuntimeStatistics which will be done by the following method.
            getWaitingTimeAndUpdateRuntimeStatistics();

            if (isVerbose()) {
                clog << "Starting next cycle at " << TimeStamp().toString() << endl;
            }
        }

        core::base::ModuleState::MODULE_EXITCODE ManagedClientModule::runModuleImplementation_ManagedLevel_Pulse_Time() {
            // In the controlled pulse_time mode, the time pulses are provided
            // centrally from supercomponent and each component is incrementing
            // the local time based on these pulses.
            //
            // Therefore, the local TimeFactory that wraps the local system
            // time needs to be disabled and exchanged by a controllable one.

            // 1) Disable the existing TimeFactory.
            context::base::RuntimeControl::DisableTimeFactory dtf;
            dtf.disable();

            // 2) Create a controllable TimeFactory.
            m_controlledTimeFactory = new context::base::ControlledTimeFactory();

            // 3) Initialize the new TimeFactory with the new actual time.
            m_controlledTimeFactory->setTime(m_time.now());

            // 4) Run the module implementation as usual. As the
            //    module will be exited after the following call
            //    has returned, we don't need to change back the
            //    time.
            return runModuleImplementation_ManagedLevel_Pulse();
        }

        void ManagedClientModule::wait_ManagedLevel_Pulse_Time() {
            // As the managed level ML_PULSE_TIME uses a breakpoint, the wait() method
            // will not be called at all. Thus, all profiling information needs to be
            // handled in reached_ManagedLevel_Pulse_Time().
        }

    }
} // core::base
