/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_CONDITIONFACTORY_H_
#define HESPERIA_CORE_WRAPPER_CONDITIONFACTORY_H_

#include "core/wrapper/Condition.h"
#include "core/wrapper/Disposable.h"
#include "core/wrapper/Mutex.h"

namespace core {
    namespace wrapper {

        /**
         * Abstract factory for creating conditions using different
         * implementations (i.e. Boost or POSIX).
         *
         * It can be used as follows:
         *
         * @code
         * Condition *c = NULL;
         *
         * try {
         *     c = ConditionFactory::getInstance().createCondition();
         * }
         * catch(string &s) {
         *    clog << "Failed: " << s << endl;
         * }
         *
         * if (c != NULL) {
         *     c->waitOnSignal();
         * }
         *
         * ...
         * // Another thread:
         *
         * if (c != NULL) {
         *     c->wakeAll();
         * }
         *
         * ...
         * // Original thread:
         * if (c != NULL) {
         *     delete c;
         * }
         *
         * @endcode
         */
        class ConditionFactory : public Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ConditionFactory(const ConditionFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ConditionFactory& operator=(const ConditionFactory &);

            protected:
                ConditionFactory();

            public:
                virtual ~ConditionFactory();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static ConditionFactory& getInstance();

                /**
                 * This method returns the condition.
                 *
                 * @return  condition based on the type of instance this factory is.
                 */
                virtual Condition* createCondition() = 0;

            private:
                static Mutex *m_singletonMutex;
                static ConditionFactory *m_singleton;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_CONDITIONFACTORY_H_*/
