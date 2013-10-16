/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_IO_CONTAINERCONFERENCEFACTORY_H_
#define OPENDAVINCI_CORE_IO_CONTAINERCONFERENCEFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Mutex.h"
#include "core/io/ContainerConference.h"

namespace core {
    namespace io {

        using namespace std;

        /**
         * This class provides ContainerConferences.
         */
        class OPENDAVINCI_API ContainerConferenceFactory {
            public:
                enum {
                    MULTICAST_PORT = 12175 // Mariposa Rd, Victorville.
                };

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ContainerConferenceFactory(const ContainerConferenceFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ContainerConferenceFactory& operator=(const ContainerConferenceFactory &);

            protected:
                /**
                 * The default constructor is protected to allow subclasses
                 * (e.g. in testcases).
                 */
                ContainerConferenceFactory();

            public:
                virtual ~ContainerConferenceFactory();

                /**
                 * This method returns a static instance for this factory.
                 *
                 * @return Instance of this factory.
                 */
                static ContainerConferenceFactory& getInstance();

                /**
                 * This method returns a new ContainerConference.
                 *
                 * @param address Use address for joining.
                 * @param port Use port for joining.  If omitted, MULTICAST_PORT will be used.
                 * @return ContainerConference or NULL.
                 */
                virtual ContainerConference* getContainerConference(const string &address, const uint32_t &port = ContainerConferenceFactory::MULTICAST_PORT);

            protected:
                /**
                 * This method sets the singleton pointer.
                 *
                 * @param singleton Singleton to be used.
                 */
                static void setSingleton(ContainerConferenceFactory* singleton);

            private:
                static base::Mutex m_singletonMutex;
                static ContainerConferenceFactory* m_singleton;
        };

    }
} // core::io

#endif /*OPENDAVINCI_CORE_IO_CONTAINERCONFERENCEFACTORY_H_*/
