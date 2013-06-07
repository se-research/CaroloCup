/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_MASTERMODULE_H_
#define HESPERIA_CORE_BASE_MASTERMODULE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/AbstractCIDModule.h"
#include "core/exceptions/Exceptions.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This class is the base for every master module. A master module
         * masters the configuration for every module as well as the the
         * overall state machines. For using this class one has to override
         * the method "virtual int32_t body();":
         *
         * @code
         * class MyModule: public MasterModule
         * {
         *     public:
         *         MyModule(const int32_t argc, char **argv) :
         *           MasterModule(argc, argv)
         *         {}
         *
         *        int32_t body() {
         *            // Do something.
         *            return OKAY;
         *        }
         * };
         *
         * // Regular main method.
         * int32_t main(int32_t argc, char **argv) {
         *     MyModule myModule(argc, argv);
         *
         *     return myModule.runModule();
         * }
         * @endcode
         */
        class HESPERIA_API MasterModule : public AbstractCIDModule {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                MasterModule(const MasterModule& );

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                MasterModule& operator=(const MasterModule& );

            public:
                /**
                 * Constructor.
                 *
                 * @param argc Number of command line arguments.
                 * @param argv Command line arguments.
                 * @throw InvalidArgumentException if the signal handler could not be registered.
                 */
                MasterModule(const int32_t &argc, char **argv) throw (exceptions::InvalidArgumentException);

                virtual ~MasterModule();

                virtual ModuleState::MODULE_EXITCODE runModule();

            protected:
                virtual ModuleState::MODULE_EXITCODE body() = 0;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_MASTERMODULE_H_*/
