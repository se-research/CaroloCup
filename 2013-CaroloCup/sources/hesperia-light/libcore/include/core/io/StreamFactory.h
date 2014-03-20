/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_IO_STREAMFACTORY_H_
#define HESPERIA_CORE_IO_STREAMFACTORY_H_

#include <iostream>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/Mutex.h"
#include "core/exceptions/Exceptions.h"
#include "core/io/URL.h"

namespace core {
    namespace io {

        using namespace std;

        /**
         * This class provides input and output streams.
         */
        class HESPERIA_API StreamFactory {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                StreamFactory(const StreamFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                StreamFactory& operator=(const StreamFactory &);

            private:
                StreamFactory();

            public:
                virtual ~StreamFactory();

                /**
                 * This method returns a static instance for this factory.
                 *
                 * @return Instance of this factory.
                 */
                static StreamFactory& getInstance();

                /**
                 * This method returns a new input stream based on a given URL.
                 *
                 * @param url URL for the input stream
                 * @return input stream based on the given URL.
                 * @throws InvalidArgumentException in case of an invalid URL.
                 */
                istream& getInputStream(const URL &url) throw (core::exceptions::InvalidArgumentException);

                /**
                 * This method returns a new output stream based on a given URL.
                 *
                 * @param url URL for the output stream
                 * @return output stream based on the given URL.
                 * @throws InvalidArgumentException in case of an invalid URL.
                 */
                ostream& getOutputStream(const URL &url) throw (core::exceptions::InvalidArgumentException);

            private:
                static base::Mutex m_singletonMutex;
                static StreamFactory* m_singleton;

                vector<istream*> m_listOfInputStreams;
                vector<ostream*> m_listOfOutputStreams;
        };

    }
} // core::io

#endif /*HESPERIA_CORE_IO_STREAMFACTORY_H_*/
