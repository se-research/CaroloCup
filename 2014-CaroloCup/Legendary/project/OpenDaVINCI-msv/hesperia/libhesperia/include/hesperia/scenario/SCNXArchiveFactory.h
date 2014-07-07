/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_SCENARIO_SCNXARCHIVEFACTORY_H_
#define HESPERIA_SCENARIO_SCNXARCHIVEFACTORY_H_

#include <map>
#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/Mutex.h"
#include "core/exceptions/Exceptions.h"
#include "core/io/URL.h"
#include "core/wrapper/StringComparator.h"

#include "hesperia/scenario/SCNXArchive.h"

namespace hesperia {
    namespace scenario {

        using namespace std;

        /**
         * This class produces an instance for accessing the contents
         * as well as the parsed scenario data structure of an
         * SCNXArchive.
         */
        class OPENDAVINCI_API SCNXArchiveFactory {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SCNXArchiveFactory(const SCNXArchiveFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SCNXArchiveFactory& operator=(const SCNXArchiveFactory &);

            private:
                SCNXArchiveFactory();

            public:
                virtual ~SCNXArchiveFactory();

                /**
                 * This method returns a static instance for this factory.
                 *
                 * @return Instance of this factory.
                 */
                static SCNXArchiveFactory& getInstance();

                /**
                 * This method returns the SCNXArchive data structure.
                 *
                 * @param u URL describing the source of the SCNX archive file.
                 * @return SCNXArchive.
                 * @throws InvalidArgumentException if the URL could not be used to create the data structure.
                 */
                SCNXArchive& getSCNXArchive(const core::io::URL &u) throw (core::exceptions::InvalidArgumentException);

            private:
                static core::base::Mutex m_singletonMutex;
                static SCNXArchiveFactory* m_singleton;

                map<string, SCNXArchive*, core::wrapper::StringComparator> m_mapOfSCNXArchives;
        };

    }
} // hesperia::scenario

#endif /*HESPERIA_SCENARIO_SCNXARCHIVEFACTORY_H_*/
