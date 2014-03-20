/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "ActiveBuffer.h"

#include "core/base/BufferedFIFOQueue.h"
#include "core/base/Mutex.h"
#include "core/base/Lock.h"

namespace monitor
{

  using namespace core::base;
  using namespace hesperia::base;

  ActiveBuffer::ActiveBuffer(int32_t size, DataStoreManager &dsm, Mutex &fifoMutex) :
    BufferedFIFOQueue(size), m_dsm(dsm), m_fifoMutex(fifoMutex), m_paused(),
        m_bufferPausedCondition()
  {
  }

  ActiveBuffer::~ActiveBuffer()
  {
  }

  void
  ActiveBuffer::beforeStop()
  {
    // Awake our thread.
    {
    Lock l(m_bufferPausedCondition);
    m_bufferPausedCondition.wakeAll();
    }
    wakeAll();
  }

  void
  ActiveBuffer::pauseRequest()
  {
    m_paused = true;

  }

  void
  ActiveBuffer::setPlay()
  {
    m_paused = false;
    Lock l(m_bufferPausedCondition);
    m_bufferPausedCondition.wakeAll();
  }

  void
  ActiveBuffer::run()
  {
    serviceReady();

    while (isRunning())
      {
        {
          Lock l(m_bufferPausedCondition);
          if (m_paused) {
            m_bufferPausedCondition.waitOnSignal();
          }
        }

        wait();
        if (isFull())
          {
              {
                Lock l(m_fifoMutex);
                leave();
                emit shifted(1);
              }
          }
        emit bufferFillChanged(static_cast<int32_t> (getSize()));
      }
  }

}
