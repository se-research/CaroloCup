/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_IRUSMAP_POINTSENSOR_H_
#define COCKPIT_PLUGINS_IRUSMAP_POINTSENSOR_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <string>
#include <map>

#include "core/data/environment/Point3.h"

#include "QtIncludes.h"

#include "SensorBoardData.h"

namespace cockpit {

    namespace plugins {

        namespace irusmap {

            using namespace std;

            /**
             * This class encapsulates a point providing sensor to be drawn by IrUsMap.
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
                     * @param translation Translation of the sensor.
                     * @param rotZ Rotation in DEG around the Z-axis. +: counterclockwise, -:clockwise, 0 = 12am, -90 = 3pm, ...
                     * @param angleFOV Field of view angle in DEG.
                     * @param distanceFOV Range of the FOV.
                     * @param clampDistance Maximum distance for the sensor; any distance greater than this will be set to -1.
                     */
                    PointSensor(const uint16_t &id, const string &name, const core::data::environment::Point3 &translation, const double &rotZ, const double &angleFOV, const double &distanceFOV, const double &clampDistance);

                    virtual ~PointSensor();

                    void drawFOV(QPainter &painter) const;

                    void drawMatchingDistances(QPainter &painter, const msv::SensorBoardData &sbd) const;

                    const string getName() const;

                    uint16_t getID() const;

                    const string toString() const;

                    core::data::environment::Point3 getTranslation() const;

                    core::data::environment::Point3 getDescPoint() const;

                private:
                    uint16_t m_id;
                    string m_name;
                    core::data::environment::Point3 m_translation;
                    double m_rotZ;
                    double m_angleFOV;
                    double m_distanceFOV;
                    double m_clampDistance;
            };
 
       }
    }
}

#endif /*COCKPIT_PLUGINS_IRUSMAP_POINTSENSOR_H_*/
