/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXTIMEFACTORY_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXTIMEFACTORY_H_

#include "core/wrapper/TimeFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            /**
             * This class is a concrete derivative for the abstract
             * factory TimeFactory using POSIX.
             *
             * @See TimeFactory
             */
            class POSIXTimeFactory : public TimeFactory {
                protected:
                    friend class TimeFactory;

                    POSIXTimeFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXTimeFactory(const POSIXTimeFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXTimeFactory& operator=(const POSIXTimeFactory &);

                public:
                    virtual ~POSIXTimeFactory();

                    virtual Time* now();
            };

        }
    }
} // core::wrapper::POSIX

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXTIMEFACTORY_H_*/
