/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef POINTSENSOR_H_
#define POINTSENSOR_H_

#include <stdint.h>

#include <string>

namespace msv {

    using namespace std;

    /**
     * This class encapsulates a point providing sensor to realize the mapping between UDP_Server from Benjamin and our code.
     */
    class PointSensor {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            PointSensor(const PointSensor &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            PointSensor& operator=(const PointSensor &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param id ID.
             * @param name Name.
             * @param address Address of the sensor according to UDP_Server's data.
             * @param clampDistance Any distances greater than this will be mapped to -1.
             */
            PointSensor(const uint16_t &id, const string &name, const uint16_t &address, const double &clampDistance);

            virtual ~PointSensor();

            const string getName() const;

            uint16_t getID() const;

            uint16_t getAddress() const;

            double getClampDistance() const;

            const string toString() const;

        private:
            uint16_t m_id;
            string m_name;
            uint16_t m_address;
            double m_clampDistance;
    };

} // msv

#endif /*POINTSENSOR_H_*/
