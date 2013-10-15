/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_IRUSCHARTS_IRUSCHARTDATA_H_
#define COCKPIT_PLUGINS_IRUSCHARTS_IRUSCHARTDATA_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#ifndef PANDABOARD

#include <deque>

#include "QtIncludes.h"

#include "SensorBoardData.h"

namespace cockpit {

    namespace plugins {

      namespace iruscharts {

            using namespace std;

            /**
             * This class is the container of accessing previous SensorBoardData.
             */
            class IrUsChartData : public QwtData {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    IrUsChartData(const IrUsChartData &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    IrUsChartData& operator=(const IrUsChartData &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param data Reference to the deque of received SensorBoardData.
                     * @param dataSelection Select the type of data that is returned.
                     */
                    IrUsChartData(deque<msv::SensorBoardData> &data, const uint32_t &dataSelection);

                    virtual ~IrUsChartData();

                    virtual QwtData *copy() const;

                    virtual size_t size() const;

                    virtual double x(size_t i) const;

                    virtual double y(size_t i) const;

                private:
                    deque<msv::SensorBoardData> &m_data;
                    const uint32_t m_dataSelection;
            };
        }
    }
}

#endif

#endif /*COCKPIT_PLUGINS_IRUSCHARTS_IRUSCHARTDATA_H_*/

