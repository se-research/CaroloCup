/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <strstream>

#include "PointSensor.h"

namespace msv {

    using namespace std;

    PointSensor::PointSensor(const uint16_t &id, const string &name, const uint16_t &address, const double &clampDistance) :
        m_id(id),
        m_name(name),
        m_address(address),
        m_clampDistance(clampDistance)
    {}

    PointSensor::~PointSensor() {}

    const string PointSensor::getName() const {
        return m_name;    
    }

    uint16_t PointSensor::getID() const {
        return m_id;    
    }

    uint16_t PointSensor::getAddress() const {
        return m_address;    
    }

    double PointSensor::getClampDistance() const {
        return m_clampDistance;
    }

    const string PointSensor::toString() const {
        strstream sstr;
        sstr << m_name << "(" << m_id << ")" << ": " << m_address << ", clampDistance: " << m_clampDistance;
        return sstr.str();
    }  

} // msv

