/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef COCKPIT_PLUGINS_MODULESTATISTICSVIEWER_LOADPERMODULE_H_
#define COCKPIT_PLUGINS_MODULESTATISTICSVIEWER_LOADPERMODULE_H_

#include <deque>
#include <vector>
#include <cstring>

#include "QtIncludes.h"

#include "core/base/Mutex.h"
#include "core/data/RuntimeStatistic.h"
#include "core/data/dmcp/ModuleDescriptor.h"

namespace cockpit {
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
                    LoadPerModule( const core::data::dmcp::ModuleDescriptor &md, const QColor &color);

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
                    core::data::dmcp::ModuleDescriptor m_moduleDescriptor;
                    QwtPlotCurve *m_curve;
                    core::base::Mutex m_loadMutex;
                    deque<core::data::RuntimeStatistic> m_load;
                    vector<double> m_data;
                    vector<double> m_reference;
                    double m_referenceOffset;
            };

        }
    }
} // cockpit::plugins::modulestatisticsviewer

#endif /*COCKPIT_PLUGINS_MODULESTATISTICSVIEWER_LOADPERMODULE_H_*/
