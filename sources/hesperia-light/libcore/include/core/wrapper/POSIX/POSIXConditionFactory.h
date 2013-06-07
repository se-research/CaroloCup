/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXCONDITIONFACTORY_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXCONDITIONFACTORY_H_

#include "core/wrapper/ConditionFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            /**
             * This class is a concrete derivative for the abstract
             * factory ConditionFactory.
             *
             * @See ConditionFactory
             */
            class POSIXConditionFactory : public ConditionFactory {
                protected:
                    friend class ConditionFactory;

                    POSIXConditionFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXConditionFactory(const POSIXConditionFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXConditionFactory& operator=(const POSIXConditionFactory &);

                public:
                    virtual ~POSIXConditionFactory();

                    virtual Condition* createCondition();
            };

        }
    }
} // core::wrapper::POSIX

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXCONDITIONFACTORY_H_*/
