/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#ifndef PANDABOARD

#include "plugins/iruscharts/IrUsChartData.h"

namespace cockpit {

    namespace plugins {

        namespace iruscharts {

            using namespace std;

            IrUsChartData::IrUsChartData(deque<msv::SensorBoardData> &data, const uint32_t &dataSelection) :
                QwtData(),
                m_data(data),
                m_dataSelection(dataSelection) {}

            IrUsChartData::~IrUsChartData() {}

            QwtData* IrUsChartData::copy() const {
                return new IrUsChartData(m_data, m_dataSelection);
            }

            size_t IrUsChartData::size() const {
                return m_data.size();
            }

            double IrUsChartData::x(size_t i) const {
                return (double)i;
            }

            double IrUsChartData::y(size_t i) const {
                double value = m_data.at(i).getDistance(m_dataSelection);
                return value;
            }

        }
    }
}

#endif
