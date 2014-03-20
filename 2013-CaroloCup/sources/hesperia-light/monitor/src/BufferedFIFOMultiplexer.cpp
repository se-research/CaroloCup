/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cmath>
#include <iostream>

#include "core/base/Lock.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/io/StreamFactory.h"
#include "core/io/URL.h"

#include "BufferedFIFOMultiplexer.h"
#include "FIFOMultiplexer.h"
#include "ActiveBuffer.h"
#include "QtIncludes.h"

namespace monitor
{

  using namespace std;
  using namespace core::base;
  using namespace core::data;
  using namespace core::io;
  using namespace hesperia::base;

  BufferedFIFOMultiplexer::BufferedFIFOMultiplexer(DataStoreManager &dsm) :
    FIFOMultiplexer(dsm), m_dataStoreManager(dsm), m_fifoMutex(), m_fifo(10000,
        dsm, m_fifoMutex), m_speed(1.0), m_positionMutex(),
        m_bufferPosition(0), m_minBorder(0), m_maxBorder(0)
  {
    qRegisterMetaType<int32_t> ("int32_t");
    connect(&m_fifo, SIGNAL(bufferFillChanged(int32_t)),
        SIGNAL(bufferFillChanged(int32_t)));
    connect(&m_fifo, SIGNAL(shifted(const int32_t)),
        SLOT(bufferShifted(const int32_t)));
  }

  BufferedFIFOMultiplexer::~BufferedFIFOMultiplexer()
  {
  }

  void
  BufferedFIFOMultiplexer::beforeStop()
  {
    m_fifo.stop();
  }

  void
  BufferedFIFOMultiplexer::run()
  {
    // Register FIFO for receiving new data.
    m_dataStoreManager.addDataStoreFor(m_fifo);
    emit
    recChanged(true);
    m_fifo.start();
    long waitTime = 0;

    serviceReady();
    while (isRunning())
      {
        while ((m_fifo.getSize() > m_bufferPosition + 1 && m_speed > 0.0f)
            || (m_bufferPosition > 1 && m_speed < 0.0f))
          {
            //std::cout << m_fifo.getSize() << " " << m_bufferPosition << std::endl;
            Container c;
              {
                Lock l(m_fifoMutex);
                c = m_fifo.getElementAt(m_bufferPosition);
              }
            distributeContainer(c);
              {
                Lock l(m_positionMutex);
                m_speed > 0 ? m_bufferPosition++ : m_bufferPosition--;
                emit positionChanged(static_cast<int32_t> (m_bufferPosition));
              }
            waitTime
                = m_fifo.getElementAt(m_bufferPosition).getReceivedTimeStamp().toMicroseconds()
                    - m_fifo.getElementAt(m_bufferPosition - 1).getReceivedTimeStamp().toMicroseconds();
            waitTime /= fabs(m_speed);
            //std::cout << "Waiting " << waitTime << " microseconds for # " << m_bufferPosition << std::endl;
            Thread::usleep(waitTime);

            //break out of loop
            if (!isRunning())
              break;
          }
      }
  }

  void
  BufferedFIFOMultiplexer::bufferShifted(const int32_t &items)
  {
      {
        Lock l(m_positionMutex);
        m_bufferPosition -= items;
        emit positionChanged(static_cast<int32_t> (m_bufferPosition));
      }
  }

  const ActiveBuffer*
  BufferedFIFOMultiplexer::getBuffer() const
  {
    return &m_fifo;
  }

  float
  BufferedFIFOMultiplexer::getSpeed() const
  {
    return m_speed;
  }

  void
  BufferedFIFOMultiplexer::setSpeed(const float &speed)
  {
    m_speed = speed;
    emit speedChanged(m_speed);
  }

  void
  BufferedFIFOMultiplexer::pause()
  {
    setSpeed(0.0f);
    m_fifo.pauseRequest();
    emit recChanged(false);
  }

  void
  BufferedFIFOMultiplexer::playFwd()
  {
    setSpeed(1.0f);
  }

  void
  BufferedFIFOMultiplexer::playRev()
  {
    setSpeed(-1.0f);
  }

  void
  BufferedFIFOMultiplexer::forward()
  {
    if (m_speed < 1.0f && m_speed >= -1.0f)
      {
        setSpeed(round((m_speed + 0.1f) * 10) / 10);
      }
    else
      {
        setSpeed(round((m_speed + 0.5f) * 10) / 10);
      }
  }

  void
  BufferedFIFOMultiplexer::reverse()
  {
    if (m_speed > -1.0f && m_speed <= 1.0f)
      {
        setSpeed(round((m_speed - 0.1f) * 10) / 10);
      }
    else
      {
        setSpeed(round((m_speed - 0.5f) * 10) / 10);
      }
  }

  void
  BufferedFIFOMultiplexer::stepForward()
  {
    if (m_fifo.getSize() > m_bufferPosition + 1)
      {
          {
            Lock l(m_positionMutex);
            m_bufferPosition++;
            emit positionChanged(static_cast<int32_t> (m_bufferPosition));
          }

        Container c;
          {
            Lock l(m_fifoMutex);
            c = m_fifo.getElementAt(m_bufferPosition);
          }
        distributeContainer(c);

        //std::cout << "Step forward to " << m_bufferPosition << std::endl;
      }
  }

  void
  BufferedFIFOMultiplexer::stepReverse()
  {
    if (m_bufferPosition > 0)
      {
          {
            Lock l(m_positionMutex);
            m_bufferPosition--;
            emit positionChanged(static_cast<int32_t> (m_bufferPosition));
          }

        Container c;
          {
            Lock l(m_fifoMutex);
            c = m_fifo.getElementAt(m_bufferPosition);
          }
        distributeContainer(c);

        //std::cout << "Step reverse to " << m_bufferPosition << std::endl;
      }
  }

  void
  BufferedFIFOMultiplexer::saveToDisk()
  {
    QFileDialog* saveDialog = new QFileDialog();
    saveDialog->setFileMode(QFileDialog::AnyFile);
    saveDialog->setWindowTitle(tr("Save Buffer.."));
    saveDialog->setFilter(tr("Recorder File (*.rec)"));
    QString filename;
    bool saveInterval = false;
    QMessageBox::StandardButton useBorders;
    if (m_minBorder != m_maxBorder)
      {
        useBorders = QMessageBox::question(NULL,
            tr("Save Inteval?"), tr("Save only selected interval? \n "
              "Say no to save the complete buffer. "
              "\n Abort to do not save anything."), QMessageBox::Yes
                | QMessageBox::No | QMessageBox::Abort, QMessageBox::Abort);
        saveInterval = (useBorders == QMessageBox::Yes);
      } else {
        useBorders = QMessageBox::question(NULL,
                    tr("Interval empty."), tr("Selected buffer interval is empty. \n "
                      "Save complete buffer? "), QMessageBox::Yes
                        | QMessageBox::Abort, QMessageBox::Abort);
      }
    if (useBorders != QMessageBox::Abort)
      {
        if (saveDialog->exec())
          {
            filename = saveDialog->selectedFiles()[0];
            if (filename[filename.size() - 4] != '.')
              {
                QMessageBox::StandardButton res = QMessageBox::question(NULL,
                    tr("Recorder File"), tr(
                        "File has no or wrong extension. \n Append '.rec'?"),
                    QMessageBox::Abort | QMessageBox::No | QMessageBox::Yes,
                    QMessageBox::Abort);
                if (res == QMessageBox::Yes)
                  {
                    filename.append(".rec");
                  }
                if (res == QMessageBox::Abort)
                  {
                    filename.clear();
                  }
              }
          }

        if (!filename.isEmpty())
          {
            URL url("file://" + filename.toStdString());
            ostream &out = StreamFactory::getInstance().getOutputStream(url);

              {
                Lock l(m_fifoMutex);

                if (!m_fifo.isEmpty())
                  {
                    uint32_t first = 0;
                    uint32_t last = m_fifo.getSize();
                    if (saveInterval) {
                      first = m_minBorder;
                      last = m_maxBorder;
                    }
                    for (uint32_t i = first; i < last; i++)
                      {
                        Container c = m_fifo.getElementAt(i);
                        out << c;
                      }
                    out.flush();
                    stringstream count;
                    count << (last - first);
                    QString message(count.str().c_str());
                    std::cout << "m_minBorder: " << m_minBorder << "(" << first << ")" << std::endl;
                    std::cout << "m_maxBorder: " << m_maxBorder << "(" << last << ")"<< std::endl;
                    std::cout << "Saved " << (last-first) << " container." << std::endl;
                    message.append(tr(" container successfully saved!"));
                    QMessageBox::information(NULL,tr("Save successful."), message,QMessageBox::Ok, QMessageBox::Ok);
                  }
              }
          }
      }
  }

  void
  BufferedFIFOMultiplexer::loadFromDisk()
  {
    //Stop Recording
    pause();

    QFileDialog* openDialog = new QFileDialog();
    openDialog->setFileMode(QFileDialog::ExistingFile);
    openDialog->setWindowTitle(tr("Load Buffer.."));
    openDialog->setFilter(tr("Recorder File (*.rec)"));
    QString filename;
    if (openDialog->exec())
      {
        filename = openDialog->selectedFiles()[0];
      }

    if (!filename.isEmpty())
      {
        URL url("file://" + filename.toStdString());
        istream &in = StreamFactory::getInstance().getInputStream(url);

          {
            Lock l(m_fifoMutex);

            m_fifo.clear();
          }
        int32_t count = 0;
        while (!in.eof())
          {
            Container c;
            in >> c;
            count++;

              {
                Lock l(m_fifoMutex);

                m_fifo.add(c);
              }
          }
        emit bufferFillChanged(static_cast<int32_t> (m_fifo.getSize()));
        {
          Lock l(m_positionMutex);
          m_bufferPosition = 0;
          emit positionChanged(static_cast<int32_t> (m_bufferPosition));
        }
        stringstream cs;
        cs << count;
        QString message(cs.str().c_str());
        message.append(tr(" container successfully Loaded!"));
        QMessageBox::information(NULL,tr("Load successful."), message,QMessageBox::Ok, QMessageBox::Ok);
      }
  }

  void
  BufferedFIFOMultiplexer::bufferNearlyFull()
  {
    if (QMessageBox::question(
        NULL,
        tr("Buffer nearly full!"),
        tr(
            "The Container-Buffer is nearly full and set to pause. Further data will be lost. \n  Do you want to reactivate data-buffering? \n (Answering this question with yes will shift the buffer-data. You might lost older data.)"),
        QMessageBox::No | QMessageBox::Yes, QMessageBox::No)
        == QMessageBox::Yes)
      {
        m_fifo.setPlay();
      }
  }

  void
  BufferedFIFOMultiplexer::setPosition(const int32_t &position)
  {
    m_bufferPosition = position;
  }

  void
  BufferedFIFOMultiplexer::setRecordingState(bool state)
  {
    if (state && !m_fifo.isEmpty()
        && QMessageBox::question(
            NULL,
            tr("Restart Recording?"),
            tr(
                "The Container-Buffer will be cleared. All data will be lost. \n Restart recording?"),
            QMessageBox::No | QMessageBox::Yes, QMessageBox::No)
            == QMessageBox::Yes)
      {
          {
            Lock l(m_positionMutex);
            m_bufferPosition = 0;
            emit positionChanged(static_cast<int32_t> (m_bufferPosition));
          }
          {
            Lock l(m_fifoMutex);

            m_fifo.clear();
          }emit
        bufferFillChanged(static_cast<int32_t> (m_fifo.getSize()));
        m_fifo.setPlay();
        emit recChanged(true);
      }
    else
      {
        m_fifo.pauseRequest();
        emit recChanged(false);
      }
  }

  void
  BufferedFIFOMultiplexer::setMinimum(const int32_t &minimum)
  {
    m_minBorder = minimum;
  }

  void
  BufferedFIFOMultiplexer::setMaximum(const int32_t & maximum)
  {
    m_maxBorder = maximum;
  }

}

