/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef LOADPLOT_H_
#define LOADPLOT_H_

#include "core/SharedPointer.h"

#include <vector>

#include "QtIncludes.h"
#include "LoadPerModule.h"

namespace cockpit {
    namespace plugins {
        namespace modulestatisticsviewer {

            using namespace std;

            class LoadPlot: public QwtPlot {

                Q_OBJECT

                private:
	                /**
	                 * "Forbidden" copy constructor. Goal: The compiler should warn
	                 * already at compile time for unwanted bugs caused by any misuse
	                 * of the copy constructor.
	                 */
	                LoadPlot(const LoadPlot &/*obj*/);

	                /**
	                 * "Forbidden" assignment operator. Goal: The compiler should warn
	                 * already at compile time for unwanted bugs caused by any misuse
	                 * of the assignment operator.
	                 */
	                LoadPlot& operator=(const LoadPlot &/*obj*/);

                public:
	                LoadPlot(QWidget* prnt = 0);

	                ~LoadPlot();

	                void addLoadPerModule(core::SharedPointer<LoadPerModule> lpm);

                private:
	                QwtLegend* m_legend;
	                vector<core::SharedPointer<LoadPerModule> > m_toAdd;
	                QTimer* m_toAttachVisitor;

                private slots:
	                void attachQueuedLPM();

            };
        }
    }
}

#endif /* LOADPLOT_H_ */
