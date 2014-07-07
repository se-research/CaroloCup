/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include <cmath>
#include <strstream>

#include "core/data/Constants.h"

#include "PointSensor.h"

namespace irus {

    using namespace std;
    using namespace core::data;
    using namespace core::data::environment;
    using namespace hesperia::data::environment;

    PointSensor::PointSensor(const uint16_t &id, const string &name, const core::data::environment::Point3 &translation, const double &rotZ, const double &angleFOV, const double &distanceFOV, const double &clampDistance, const bool &showFOV) :
        m_id(id),
        m_name(name),
        m_translation(translation),
        m_rotZ(rotZ),
        m_angleFOV(angleFOV),
        m_distanceFOV(distanceFOV),
        m_clampDistance(clampDistance),
        m_showFOV(showFOV),
        m_totalRotation(0),
        m_FOV(),
        m_sensorPosition()
    {}

    PointSensor::~PointSensor() {}

    Polygon PointSensor::updateFOV(const Point3 &translationGlobal, const Point3 &rotationGlobal) {
        // 1. Rotate the translation of the sensor w.r.t. the global rotation of the vehicle.
        m_sensorPosition = m_translation;
        m_sensorPosition.rotateZ(rotationGlobal.getAngleXY());
        m_sensorPosition += translationGlobal;

        m_totalRotation = rotationGlobal.getAngleXY() + m_rotZ*Constants::DEG2RAD;

        // 2. Construct the FOV in the origin w.r.t. the global rotation of the vehicle.
        Point3 leftBoundaryFOV(m_distanceFOV, 0, 0);
        leftBoundaryFOV.rotateZ(m_totalRotation + (m_angleFOV/2.0)*Constants::DEG2RAD);

        Point3 rightBoundaryFOV(m_distanceFOV, 0, 0);
        rightBoundaryFOV.rotateZ(m_totalRotation - (m_angleFOV/2.0)*Constants::DEG2RAD);

        // 3. Translate the FOV to the sensor's position.
        leftBoundaryFOV += m_sensorPosition;
        rightBoundaryFOV += m_sensorPosition;

        // Iterate through all available polygons and intersect FOV-polygon with polygon.
        Polygon FOV;
        FOV.add(m_sensorPosition);
        FOV.add(leftBoundaryFOV);
        FOV.add(rightBoundaryFOV);
        FOV.add(m_sensorPosition);
        m_FOV = FOV;

        return m_FOV;
    }

    double PointSensor::getDistance(map<uint32_t, hesperia::data::environment::Polygon> &mapOfPolygons) {
        Point3 nearest;
        double distanceToSensor = -1;

        map<uint32_t, hesperia::data::environment::Polygon>::const_iterator it = mapOfPolygons.begin();
        while (it != mapOfPolygons.end()) {
            Polygon p = it->second;

            // Get overlapping parts of polygon...
            Polygon contour = m_FOV.intersectIgnoreZ(p);

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
            double angleDelta = (nearest - m_sensorPosition).getAngleXY() - m_totalRotation;

            // Normalize angle to interval -PI ... PI.
            while (angleDelta < -Constants::PI) {
                angleDelta += 2.0*Constants::PI;
            }
            while (angleDelta > Constants::PI) {
                angleDelta -= 2.0*Constants::PI;
            }

            if ( (fabs(angleDelta) < m_angleFOV) && (distanceToSensor < m_distanceFOV) ) {
                // Found distance.
                //cerr << "Object's distance: " << distanceToSensor << ", Pos: " << nearest.toString() << ", angleDelta: " << angleDelta << endl;
            }
            else {
                distanceToSensor = -1;
            }   
        }

        if (distanceToSensor > m_clampDistance) {
            distanceToSensor = -1;
        }

        return distanceToSensor;
    } 

    const string PointSensor::getName() const {
        return m_name;    
    }

    uint16_t PointSensor::getID() const {
        return m_id;    
    }

    bool PointSensor::hasShowFOV() const {
        return m_showFOV;
    }

    const string PointSensor::toString() const {
        strstream sstr;
        sstr << m_name << "(" << m_id << ")" << ": " << m_translation.toString() << ", rot: " << m_rotZ << ", angle: " << m_angleFOV << ", range: " << m_distanceFOV << ", clampDistance: " << m_clampDistance <<  ", showFOV: " << m_showFOV;
        return sstr.str();
    }  

} // irus

