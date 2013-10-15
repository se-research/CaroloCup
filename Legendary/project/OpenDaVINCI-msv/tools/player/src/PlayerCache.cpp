/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/base/Thread.h"

#include "PlayerCache.h"

namespace player {

    using namespace std;
    using namespace core::base;
    using namespace core::data;

    PlayerCache::PlayerCache(const uint32_t size, const bool &autoRewind, istream &in) :
            m_cacheSize(size),
            m_autoRewind(autoRewind),
            m_in(in),
            m_queueMutex(),
            m_queue() {
        m_cacheSize = (m_cacheSize < 3) ? 3 : m_cacheSize;
        m_queue.clear();
    }

    PlayerCache::~PlayerCache() {
        Lock l(m_queueMutex);
        m_queue.clear();
    }

    Container PlayerCache::getNextContainer() {
        Container c;

        if (getNumberOfEntries() > 0) {
            Lock l(m_queueMutex);
            c = m_queue.front();
            m_queue.pop_front();
        }

        return c;
    }

    uint32_t PlayerCache::getNumberOfEntries() const {
        uint32_t size = 0;

        {
            Lock l(m_queueMutex);
            size = m_queue.size();
        }

        return size;
    }

    void PlayerCache::clearQueueRewindInputStream() {
        {
            Lock l(m_queueMutex);
            m_queue.clear();

            // Start from beginning.
            m_in.clear();

            // Seek to the beginning of the input stream.
            m_in.seekg(ios::beg);
        }
    }

    void PlayerCache::beforeStop() {}

    void PlayerCache::run() {
        bool dataAvailable = true;

        // Clear any error states of the stream.
        m_in.clear();

        // Seek to the beginning of the input stream.
        m_in.seekg(ios::beg);

        serviceReady();
        Container data;
        while (isRunning() && dataAvailable) {
            while (getNumberOfEntries() < m_cacheSize) {
                if (m_in.good()) {
                    m_in >> data;
                    {
                        Lock l(m_queueMutex);
                        m_queue.push_back(data);
                    }
                } else {
                    if (m_autoRewind) {
                        // Clear any error states of the stream.
                        m_in.clear();

                        // Seek to the beginning of the input stream.
                        m_in.seekg(ios::beg);
                        break;
                    } else {
                        dataAvailable = false;
                        break;
                    }
                }
            }

            Thread::usleep(250);
        }
        cerr << "PlayerCache: No more data to cache." << endl;
    }

} // player
