//
// Created by parallels on 9/30/15.
//

#include "LineDetector.h"

LineDetector::LineDetector(Config m_config) {
    m_config.XTimesYMin = 2;
    m_config.XTimesYMax = 20;
    m_config.maxY = 235;
    m_config.maxArea = 4;
}