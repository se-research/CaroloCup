/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_TIMEFACTORY_H_
#define HESPERIA_CORE_WRAPPER_TIMEFACTORY_H_

#include "core/wrapper/Disposable.h"
#include "core/wrapper/Mutex.h"
#include "core/wrapper/Time.h"

namespace core {
    namespace wrapper {

        /**
         * Abstract factory for creating wrapped times (i.e.
         * time data structures based on Boost, POSIX, ...).
         *
            * It can be used as follows:
            *
            * @code
            * Time *t = TimeFactory::getInstance().now();
            *
            * ...
            *
            * if (t != NULL) {
            *     delete t;
            * }
            *
            * @endcode
         */
        class TimeFactory : public Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                TimeFactory(const TimeFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                TimeFactory& operator=(const TimeFactory &);

            protected:
                TimeFactory();

            public:
                virtual ~TimeFactory();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static TimeFactory& getInstance();

                /**
                 * This method returns the wrapped time.
                 *
                 * @return time based on the type of instance this factory is.
                 */
                virtual Time* now() = 0;

            protected:
                /**
                 * This method sets the singleton pointer.
                 *
                 * @param singleton Singleton to be used.
                 */
                static void setSingleton(TimeFactory* singleton);

            private:
                static Mutex *m_singletonMutex;
                static TimeFactory *m_singleton;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_TIMEFACTORY_H_*/
