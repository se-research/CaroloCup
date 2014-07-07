/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#ifndef PANDABOARD

#include "plugins/forcecontrolviewer/ForceControlData.h"

namespace cockpit {

    namespace plugins {

        namespace forcecontrolviewer {

            using namespace std;
            using namespace core::data::control;

            ForceControlData::ForceControlData(deque<ForceControl> &data, const FORCECONTROLDATA_INTERFACE &dataSelection) :
                QwtData(),
                m_data(data),
                m_selectedData(dataSelection) {}

            ForceControlData::~ForceControlData() {}

            QwtData* ForceControlData::copy() const {
                return new ForceControlData(m_data, m_selectedData);
            }

            size_t ForceControlData::size() const {
                return m_data.size();
            }

            double ForceControlData::x(size_t i) const {
                return (double)i;
            }

            double ForceControlData::y(size_t i) const {
                double value = 0;
                switch (m_selectedData) {
                    case ACCELERATION_FORCE:
                        value = m_data.at(i).getAccelerationForce();
                    break;
                    case BRAKE_FORCE:
                        value = m_data.at(i).getBrakeForce();
                    break;
                    case STEERING_FORCE:
                        value = m_data.at(i).getSteeringForce();
                    break;
                }
                return value;
            }

        }
    }
}

#endif
