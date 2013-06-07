/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef ActiveBuffer_H_
#define ActiveBuffer_H_

#include "QtIncludes.h"

#include "core/base/Service.h"
#include "core/base/Condition.h"
#include "ContainerObserver.h"
#include "core/base/BufferedFIFOQueue.h"
#include "core/base/Mutex.h"
#include "hesperia/base/DataStoreManager.h"

namespace monitor {

  using namespace core::base;

class ActiveBuffer: public QObject, public core::base::Service, public core::base::BufferedFIFOQueue {

	Q_OBJECT

private:
	/**
	 * "Forbidden" copy constructor. Goal: The compiler should warn
	 * already at compile time for unwanted bugs caused by any misuse
	 * of the copy constructor.
	 *
	 * @param obj Reference to an object of this class.
	 */
	ActiveBuffer(const ActiveBuffer &/*obj*/);

	/**
	 * "Forbidden" assignment operator. Goal: The compiler should warn
	 * already at compile time for unwanted bugs caused by any misuse
	 * of the assignment operator.
	 *
	 * @param obj Reference to an object of this class.
	 * @return Reference to this instance.
	 */
	ActiveBuffer& operator=(const ActiveBuffer &/*obj*/);

public:
	ActiveBuffer(int32_t size, hesperia::base::DataStoreManager &dsm, core::base::Mutex &fifoMutex);

	virtual ~ActiveBuffer();

	void pauseRequest();

	void setPlay();

signals:
	void bufferFillChanged(const int32_t &fillRate);
	void shifted(const int32_t &items);

private:
    hesperia::base::DataStoreManager &m_dsm;
	core::base::Mutex &m_fifoMutex;
	bool m_paused;
	Condition m_bufferPausedCondition;

	virtual void beforeStop();
	virtual void run();
};
}

#endif /* ActiveBuffer_H_ */
