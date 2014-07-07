/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/AbstractModule.h"
#include "core/base/CommandLineParser.h"
#include "core/base/Lock.h"
#include "core/base/Thread.h"
#include "core/wrapper/DisposalService.h"

namespace core {
    namespace base {

        using namespace std;
        using namespace exceptions;

        // Initialization of the static attribute.
        vector<AbstractModule*> AbstractModule::m_listOfModules;

        void signalHandler(int32_t signal) {
            clog << "Module caught signal number " << signal << "." << endl;

            vector<AbstractModule*>::iterator it = AbstractModule::getListOfModules().begin();
            while (it != AbstractModule::getListOfModules().end()) {
                AbstractModule *m = *it++;
                if (m != NULL) {
                    m->setModuleState(ModuleState::NOT_RUNNING);
                }
            }
        }

        void finalize() {
            if (AbstractModule::getListOfModules().size() == 0) {
                clog << "Calling disposal service..." << endl;
                wrapper::DisposalService &ds = wrapper::DisposalService::getInstance();
                ds.cleanUpFinally();
            }
        }

        AbstractModule::AbstractModule() :
                m_moduleStateMutex(),
                m_moduleState(ModuleState::NOT_RUNNING) {
            m_listOfModules.push_back(this);

            atexit(finalize);

            if (::signal(SIGINT, &signalHandler) == SIG_ERR) {
                OPENDAVINCI_CORE_THROW_EXCEPTION(InvalidArgumentException, "Failed to register signal SIGINT.");
            }
        }

        AbstractModule::~AbstractModule() {
            vector<AbstractModule*>::iterator it = AbstractModule::getListOfModules().begin();
            while (it != AbstractModule::getListOfModules().end()) {
                AbstractModule *m = *it;
                if (m == this) {
                    break;
                }
                it++;
            }

            if (it != AbstractModule::getListOfModules().end()) {
                AbstractModule::getListOfModules().erase(it);
            }
        }

        vector<AbstractModule*>& AbstractModule::getListOfModules() {
            return m_listOfModules;
        }

        void AbstractModule::setModuleState(const ModuleState::MODULE_STATE &s) {
            Lock l(m_moduleStateMutex);
            m_moduleState = s;
        }

        void AbstractModule::wait() {
            Thread::usleep(25);
        }

        void AbstractModule::calledGetModuleState() {
            wait();
        }

        ModuleState::MODULE_STATE AbstractModule::getModuleState() {
            calledGetModuleState();

            Lock l(m_moduleStateMutex);
            return m_moduleState;
        }

    }
} // core::base
