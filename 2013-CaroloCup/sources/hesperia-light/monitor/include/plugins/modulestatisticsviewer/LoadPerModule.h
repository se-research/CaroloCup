/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MONITOR_PLUGINS_MODULESTATISTICSVIEWER_LOADPERMODULE_H_
#define MONITOR_PLUGINS_MODULESTATISTICSVIEWER_LOADPERMODULE_H_

#include <deque>
#include <vector>
#include <cstring>

#include "QtIncludes.h"

#include "core/base/Mutex.h"
#include "core/data/RuntimeStatistic.h"
#include "hesperia/data/dmcp/ModuleDescriptor.h"

namespace plugins {
    namespace modulestatisticsviewer {

        using namespace std;

        /**
         * This class collects all data per module.
         */
        class LoadPerModule {
            private:
                enum {
                    MAX_ENTRIES_TO_QUEUE = 100,
                };

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                LoadPerModule(const LoadPerModule &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                LoadPerModule& operator=(const LoadPerModule &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param plot Parental plot.
                 * @param md Module's descriptor.
                 * @param color Curve's color.
                 */
                LoadPerModule( const hesperia::data::dmcp::ModuleDescriptor &md, const QColor &color);

                virtual ~LoadPerModule();

                /**
                 * This method adds new runtime statistics.
                 *
                 * @param rts RunTimeStatistics to be added.
                 */
                void addRuntimeStatistics(const core::data::RuntimeStatistic &rts);

                QwtPlotCurve* getCurve();

                const string getModuleName() const;

            private:
                hesperia::data::dmcp::ModuleDescriptor m_moduleDescriptor;
                QwtPlotCurve *m_curve;
                core::base::Mutex m_loadMutex;
                deque<core::data::RuntimeStatistic> m_load;
                vector<double> m_data;
                vector<double> m_reference;
                double m_referenceOffset;
        };

    }
} // plugins::modulestatisticsviewer

#endif /*MONITOR_PLUGINS_MODULESTATISTICSVIEWER_LOADPERMODULE_H_*/
