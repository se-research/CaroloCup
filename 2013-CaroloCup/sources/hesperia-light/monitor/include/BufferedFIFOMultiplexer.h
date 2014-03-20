/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef BUFFEREDFIFOMULTIPLEXER_H_
#define BUFFEREDFIFOMULTIPLEXER_H_

#include <vector>

#include "FIFOMultiplexer.h"
#include "QtIncludes.h"

#include "core/base/Mutex.h"
#include "core/data/Container.h"
#include "core/base/BufferedFIFOQueue.h"
#include "hesperia/base/DataStoreManager.h"

#include "ActiveBuffer.h"

namespace monitor {

using namespace std;

class BufferedFIFOMultiplexer : public QObject, public FIFOMultiplexer  {

	Q_OBJECT

private:
	/**
	 * "Forbidden" copy constructor. Goal: The compiler should warn
	 * already at compile time for unwanted bugs caused by any misuse
	 * of the copy constructor.
	 *
	 * @param obj Reference to an object of this class.
	 */
	BufferedFIFOMultiplexer(const BufferedFIFOMultiplexer &/*obj*/);

	/**
	 * "Forbidden" assignment operator. Goal: The compiler should warn
	 * already at compile time for unwanted bugs caused by any misuse
	 * of the assignment operator.
	 *
	 * @param obj Reference to an object of this class.
	 * @return Reference to this instance.
	 */
	BufferedFIFOMultiplexer& operator=(const BufferedFIFOMultiplexer &/*obj*/);

public:
	/**
	 * Constructor.
	 *
	 * @param dsm DataStoreManager to be used for registering own FIFO.
	 */
	BufferedFIFOMultiplexer(hesperia::base::DataStoreManager &dsm);

	virtual ~BufferedFIFOMultiplexer();

	const ActiveBuffer* getBuffer() const;

	float getSpeed() const;

signals:
	void bufferFillChanged(const int32_t &fill);
	void positionChanged(const int32_t &position);
	void speedChanged(const float &speed);
	void recChanged(bool);

public slots:
	void setSpeed(const float &speed);
	void pause();
	void playFwd();
	void playRev();
	void forward();
	void reverse();
	void stepForward();
	void stepReverse();
	void saveToDisk();
	void loadFromDisk();

	void bufferNearlyFull();
	void bufferShifted(const int32_t &items);

	void setPosition(const int32_t &position);
	void setRecordingState(bool state);

	void setMinimum(const int32_t &minimum);
	void setMaximum(const int32_t &maximum);


private:
    hesperia::base::DataStoreManager &m_dataStoreManager;
	mutable core::base::Mutex m_fifoMutex;
	ActiveBuffer m_fifo;
    float m_speed;
    mutable core::base::Mutex m_positionMutex;
    uint32_t m_bufferPosition;

    uint32_t m_minBorder;
    uint32_t m_maxBorder;

    virtual void run();

    virtual void beforeStop();
};
}

#endif /* BUFFEREDFIFOMULTIPLEXER_H_ */
