/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <cmath>
#include <strstream>

#include "core/data/Constants.h"

#include "plugins/irusmap/PointSensor.h"

namespace cockpit {

    namespace plugins {

        namespace irusmap {

            using namespace std;
            using namespace core::data;
            using namespace core::data::environment;

            PointSensor::PointSensor(const uint16_t &id, const string &name, const core::data::environment::Point3 &translation, const double &rotZ, const double &angleFOV, const double &distanceFOV, const double &clampDistance) :
                m_id(id),
                m_name(name),
                m_translation(translation),
                m_rotZ(rotZ),
                m_angleFOV(angleFOV),
                m_distanceFOV(distanceFOV),
                m_clampDistance(clampDistance)
            {}

            PointSensor::~PointSensor() {}

            void PointSensor::drawFOV(QPainter &painter) const {
                Point3 A = m_translation;

                double totalRot = m_rotZ * Constants::DEG2RAD;

                Point3 B(m_distanceFOV, 0, 0);
                B.rotateZ(totalRot + (m_angleFOV/2.0) * Constants::DEG2RAD);
                B += m_translation;

                Point3 C(m_distanceFOV, 0, 0);
                C.rotateZ(totalRot - (m_angleFOV/2.0) * Constants::DEG2RAD);
                C += m_translation;

                painter.drawLine(A.getX() * 1000, A.getY() * 1000, B.getX() * 1000, B.getY() * 1000);
                painter.drawLine(A.getX() * 1000, A.getY() * 1000, C.getX() * 1000, C.getY() * 1000);

                double stepSize = 2.0; // 2° to rad.
                uint32_t steps = (unsigned int) round( m_angleFOV/stepSize );

                Point3 old = C;
                for(uint32_t i = 0; i < steps; i++) {
                    // Calculate the skeleton approximation.                
                    Point3 p;
                    p.setX(m_distanceFOV * cos(totalRot - (m_angleFOV/2.0) * Constants::DEG2RAD + i * stepSize * Constants::DEG2RAD));
                    p.setY(m_distanceFOV * sin(totalRot - (m_angleFOV/2.0) * Constants::DEG2RAD + i * stepSize * Constants::DEG2RAD));

                    p += m_translation;

                    painter.drawLine(old.getX() * 1000, old.getY() * 1000, p.getX() * 1000, p.getY() * 1000);

                    old = p;
                }

                painter.drawLine(old.getX() * 1000, old.getY() * 1000, B.getX() * 1000, B.getY() * 1000);
            }

            void PointSensor::drawMatchingDistances(QPainter &painter, const msv::SensorBoardData &sbd) const {
                double d = sbd.getDistance(m_id);
                if (d > 0) {
                    double totalRot = m_rotZ * Constants::DEG2RAD;

                    Point3 measurementPoint(d, 0, 0);
                    measurementPoint.rotateZ(totalRot);
                    measurementPoint += m_translation;

                    int width = 200;
                    int height = 200;

                    painter.fillRect(measurementPoint.getX() * 1000, measurementPoint.getY() * 1000, width, height, QBrush(Qt::red));
                }
            }

            Point3 PointSensor::getDescPoint() const {
                Point3 p(1, 0, 0);
                p.rotateZ(m_rotZ * Constants::DEG2RAD);
                p += m_translation;

                return p;
            }


            Point3 PointSensor::getTranslation() const {
                return m_translation;
            }

            const string PointSensor::getName() const {
                return m_name;    
            }

            uint16_t PointSensor::getID() const {
                return m_id;    
            }

            const string PointSensor::toString() const {
                strstream sstr;
                sstr << m_name << "(" << m_id << ")" << ": " << m_translation.toString() << ", rot: " << m_rotZ << ", angle: " << m_angleFOV << ", range: " << m_distanceFOV << ", clampDistance: " << m_clampDistance;
                return sstr.str();
            }

        }
    }
}

