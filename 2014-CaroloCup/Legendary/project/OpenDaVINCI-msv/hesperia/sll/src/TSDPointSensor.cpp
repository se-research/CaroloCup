/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include <cmath>
#include <strstream>

#include "core/data/Constants.h"
#include "hesperia/data/environment/Line.h"

#include "TSDPointSensor.h"

namespace sll {

    using namespace std;
    using namespace core::data;
    using namespace core::data::environment;
    using namespace hesperia::data::environment;

    TSDPointSensor::TSDPointSensor(const uint16_t &id, const string &name, const core::data::environment::Point3 &translation, const double &rotZ, const double &minDistance, const double &maxDistance, const double &clampDistance) :
        m_id(id),
        m_name(name),
        m_translation(translation),
        m_rotZ(rotZ),
        m_minDistance(minDistance),
        m_maxDistance(maxDistance),
        m_clampDistance(clampDistance),
        m_totalRotation(0),
        m_sensorPosition()
    {}

    TSDPointSensor::~TSDPointSensor() {}

    void TSDPointSensor::update(const Point3 &translationGlobal, const Point3 &rotationGlobal) {
        // 1. Rotate the translation of the sensor w.r.t. the global rotation of the vehicle.
        m_sensorPosition = m_translation;
        m_sensorPosition.rotateZ(rotationGlobal.getAngleXY());
        m_sensorPosition += translationGlobal;

        m_totalRotation = rotationGlobal.getAngleXY() + m_rotZ*Constants::DEG2RAD;
    }

    void TSDPointSensor::getDistances(map<uint32_t, hesperia::data::environment::Polygon> &mapOfPolygons, map<uint32_t, double> &distancesPerBeam) {
        // Per polygon:
        // Construct another polygon with two points: (sensor position, 0..359 degree * max distance).
        // Intersect the current polygon with each of the contruct polygons.
        // Return the minimal distance per beam.

        for(uint32_t i = 0; i < 360; i++) {
            const double ANGLE_FOV = 0.5; // 0.5 degree.

            Point3 beamEndLeft(m_maxDistance, 0, 0);
            beamEndLeft.rotateZ(m_totalRotation + i*Constants::DEG2RAD);
            beamEndLeft += m_sensorPosition;

            Point3 beamEndRight(m_maxDistance, 0, 0);
            beamEndRight.rotateZ(m_totalRotation + (i+ANGLE_FOV)*Constants::DEG2RAD);
            beamEndRight += m_sensorPosition;

            Polygon FOV;
            FOV.add(m_sensorPosition);
            FOV.add(beamEndLeft);
            FOV.add(beamEndRight);
            FOV.add(m_sensorPosition);

            double distanceToSensor = -1;
            Point3 nearest;

            map<uint32_t, hesperia::data::environment::Polygon>::const_iterator it = mapOfPolygons.begin();
            while (it != mapOfPolygons.end()) {
                Polygon p = it->second;

                // Get overlapping parts of polygon...
                Polygon contour = FOV.intersectIgnoreZ(p);

                if (contour.getSize() > 0) {
                    // Get nearest point from contour.
                    const vector<Point3> listOfPoints = contour.getVertices();
                    vector<Point3>::const_iterator jt = listOfPoints.begin();

                    if (distanceToSensor < 0) {
                        nearest = (*jt);
                        distanceToSensor = (nearest - m_sensorPosition).lengthXY();
                    }

                    while (jt != listOfPoints.end()) {
                        Point3 pt = (*jt++);
                        double d = (pt - m_sensorPosition).lengthXY();
                        if (d < distanceToSensor) {
                            nearest = pt;
                            distanceToSensor = d;
                        }
                    }
                }

                it++;
            }

            if (distanceToSensor > 0) {
                // Calculate angle deviation.
                double angleDelta = ((nearest - m_sensorPosition).getAngleXY() - (beamEndLeft - m_sensorPosition).getAngleXY());

                if ( (fabs(angleDelta) < 1e-1) && (distanceToSensor < m_maxDistance) ) {
                    // Found distance is appropriate.
                }
                else {
                    distanceToSensor = -1;
                }   
            }

            if (distanceToSensor > m_clampDistance) {
                distanceToSensor = -1;
            }

            if (distanceToSensor < m_minDistance) {
                distanceToSensor = -1;
            }

            // Update distance map.
            if ((distancesPerBeam[i] > distanceToSensor) || (distancesPerBeam[i] < 0)) {
                distancesPerBeam[i] = distanceToSensor;
            }

        }
    } 

    const string TSDPointSensor::getName() const {
        return m_name;    
    }

    uint16_t TSDPointSensor::getID() const {
        return m_id;    
    }

    const string TSDPointSensor::toString() const {
        strstream sstr;
        sstr << m_name << "(" << m_id << ")" << ": " << m_translation.toString() << ", rot: " << m_rotZ << ", minDistance: " << m_minDistance << ", maxDistance: " << m_maxDistance << ", clampDistance: " << m_clampDistance;
        return sstr.str();
    }  

} // sll

