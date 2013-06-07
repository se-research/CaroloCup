/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXTIME_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXTIME_H_

#include "core/wrapper/Time.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            // Forward declaration to prevent circular dependencies.
            class POSIXTimeFactory;

            /**
             * This class implements the time using POSIX.
             *
             * @See Time
             */
            class POSIXTime : public Time {
                private:
                    friend class POSIXTimeFactory;

                    POSIXTime();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXTime(const POSIXTime &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXTime& operator=(const POSIXTime &);

                public:
                    virtual ~POSIXTime();

                    virtual int32_t getSeconds() const;

                    virtual int32_t getPartialMicroseconds() const;

                private:
                    int32_t m_seconds;
                    int32_t m_partialMicroseconds;
            };

        }
    }
} // core::wrapper::POSIX

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXTIME_H_*/
