/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef TIMECONTROLPLUGIN_H_
#define TIMECONTROLPLUGIN_H_

#include "core/base/KeyValueConfiguration.h"
#include "hesperia/base/DataStoreManager.h"

#include "QtIncludes.h"

#include "plugins/PlugIn.h"
#include "plugins/MasterPlugIn.h"
#include "BufferedFIFOMultiplexer.h"
#include "plugins/timeControl/TimeControlWidget.h"
#include "core/base/Mutex.h"

namespace plugins {
namespace timeControl {

using namespace std;

class TimeControlPlugIn: public QObject, public MasterPlugIn {

	Q_OBJECT

private:
	/**
	 * "Forbidden" copy constructor. Goal: The compiler should warn
	 * already at compile time for unwanted bugs caused by any misuse
	 * of the copy constructor.
	 */
	TimeControlPlugIn(const TimeControlPlugIn &/*obj*/);

	/**
	 * "Forbidden" assignment operator. Goal: The compiler should warn
	 * already at compile time for unwanted bugs caused by any misuse
	 * of the assignment operator.
	 */
	TimeControlPlugIn& operator=(const TimeControlPlugIn &/*obj*/);

public:

	/**	                 * Constructor.
	 *
	 * @param name Name of this plugin.
	 * @param kvc KeyValueConfiguration for this widget.
	 * @param prnt Pointer to the container super window.
	 */
	TimeControlPlugIn(const string &name,
			const core::base::KeyValueConfiguration &kvc,
			hesperia::base::DataStoreManager &dsm, QWidget *prnt);

	virtual ~TimeControlPlugIn();

	virtual QWidget* getQWidget() const;

	virtual void setupPlugin();

	virtual void stopPlugin();

private:
	TimeControlWidget *m_TimeControlWidget;
	monitor::BufferedFIFOMultiplexer* m_multiplexer;


};
}

}

#endif /* TIMECONTROLPLUGIN_H_ */
