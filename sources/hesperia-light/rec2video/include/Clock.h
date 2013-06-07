/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CLOCK_H_
#define CLOCK_H_

#include <stdint.h>

namespace rec2video {

    using namespace std;

    /**
     * This class represents a rudimentary clock.
     */
    class Clock {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Clock(const Clock &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Clock& operator=(const Clock &/*obj*/);

        public:
            Clock();

            virtual ~Clock();

            /**
             * This method returns the time since start in milliseconds.
             *
             * @return Milliseconds since start.
             */
            int32_t getMillisecondsSinceStart() const;

            /**
             * This method returns the fractional microseconds.
             *
             * @return Fractional microseconds.
             */
            int32_t getFractionalMicroseconds() const;

            /**
             * This method increments the clock by 1 microsecond.
             */
            void incrementByOneMicrosecond();

        private:
            uint32_t m_millisecondsSinceStart;
            uint32_t m_fractionalMicroseconds;
    };

} // rec2video

#endif /*CLOCK_H_*/
