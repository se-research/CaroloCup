/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#ifndef TSDPOINTSENSOR_H_
#define TSDPOINTSENSOR_H_

#include <string>
#include <map>

#include "core/data/environment/Point3.h"
#include "hesperia/data/environment/Polygon.h"

namespace sll {

    using namespace std;

    /**
     * This class encapsulates a 360 degree point providing sensor using polygon data from an SCNX file.
     */
    class TSDPointSensor {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            TSDPointSensor(const TSDPointSensor &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            TSDPointSensor& operator=(const TSDPointSensor &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param id ID.
             * @param name Name.
             * @param translation Translation of the sensor.
             * @param rotZ Rotation of the sensor.
             * @param minDistance Range per beam.
             * @param maxDistance Range per beam.
             * @param clampDistance Maximum distance for the sensor; any distance greater than this will be set to -1.
             */
            TSDPointSensor(const uint16_t &id, const string &name, const core::data::environment::Point3 &translation, const double &rotZ, const double &minDistance, const double &maxDistance, const double &clampDistance);

            virtual ~TSDPointSensor();

            /**
             * This method updates the FOV.
             *
             * @param translation Global translation of the vehicle that is carrying this sensor.
             * @param rotation Global rotation of the vehicle that is carrying this sensor.
             */
            void update(const core::data::environment::Point3 &translation, const core::data::environment::Point3 &rotation);

            /**
             * This methods calculates the distances.
             *
             * @param mapOfPolygons Map of polygons to iterate through.
             * @param distances to the closest lines or -1.
             */
            void getDistances(map<uint32_t, hesperia::data::environment::Polygon> &mapOfPolygons, map<uint32_t, double> &distancesPerBeam);
            void getDistancesOld(map<uint32_t, hesperia::data::environment::Polygon> &mapOfPolygons, map<uint32_t, double> &distancesPerBeam);

            const string getName() const;

            uint16_t getID() const;

            const string toString() const;

        private:
            uint16_t m_id;
            string m_name;
            core::data::environment::Point3 m_translation;
            double m_rotZ;
            double m_minDistance;
            double m_maxDistance;
            double m_clampDistance;

            double m_totalRotation;

            core::data::environment::Point3 m_sensorPosition;
    };

} // sll

#endif /*TSDPOINTSENSOR_H_*/
