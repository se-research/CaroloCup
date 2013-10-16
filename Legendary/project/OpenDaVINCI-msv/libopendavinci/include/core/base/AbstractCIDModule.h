/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_ABSTRACTCIDMODULE_H_
#define OPENDAVINCI_CORE_BASE_ABSTRACTCIDMODULE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/InterruptibleModule.h"
#include "core/base/Periodic.h"
#include "core/exceptions/Exceptions.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This class is the abstract base for every module consisting to
         * one specific CID. For concrete classes, use either ClientModule or
         * MasterModule. ClientModules use DMCP client requests for getting
         * configuration data. MasterModules however must provide DMCP services.
         */
        class OPENDAVINCI_API AbstractCIDModule : public InterruptibleModule, public Periodic {
            protected:
                /**
                 * Constructor for any module.
                 *
                 * @param argc Number of command line arguments.
                 * @param argv Command line arguments.
                 * @throw InvalidArgumentException if the signal handler could not be registered or if --id was found but the identifier contains white spaces.
                 */
                AbstractCIDModule(const int32_t &argc, char **argv) throw (exceptions::InvalidArgumentException);

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                AbstractCIDModule(const AbstractCIDModule&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                AbstractCIDModule& operator=(const AbstractCIDModule&);

            public:
                virtual ~AbstractCIDModule();

                virtual float getFrequency() const;

                /**
                 * This method returns this module's identifier as
                 * specified in the command line.
                 *
                 * @return This module's identifier.
                 */
                const string getIdentifier() const;

                /**
                 * This method returns the module's multicast group
                 * as specified in the command line
                 */
                const string getMultiCastGroup() const;

                /**
                 * This method returns the CID.
                 *
                 * @return CID.
                 */
                uint32_t getCID() const;

                /**
                 * This method returns true, if --verbose is enabled.
                 *
                 * @return true if verbose.
                 */
                inline bool isVerbose() const {
                    return m_verbose;
                }

            private:
                float m_frequency;
                string m_identifier;
                string m_mulitcastGroup;
                uint32_t m_CID;
                bool m_verbose;

                /**
                 * This method tries to parse the identifier.
                 *
                 * @param argc Number of command line arguments.
                 * @param argv Command line arguments.
                 * @throw InvalidArgumentException if --id was found but the identifier contains white spaces.
                 */
                void parseCommandLine(const int32_t &argc, char **argv) throw (exceptions::InvalidArgumentException);
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_ABSTRACTCIDMODULE_H_*/
