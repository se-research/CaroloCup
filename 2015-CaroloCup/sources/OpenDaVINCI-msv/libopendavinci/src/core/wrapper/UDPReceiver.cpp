/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/UDPReceiver.h"

namespace core {
    namespace wrapper {

        using namespace std;

        UDPReceiver::UDPReceiver() :
            m_stringPipeline(),
            m_packetListenerMutex(NULL),
            m_packetListener(NULL) {

            m_packetListenerMutex = MutexFactory::createMutex();
            if (m_packetListenerMutex == NULL) {
                stringstream s;
                s << "Error while creating mutex at " << __FILE__ << ": " << __LINE__;
                throw s.str();
            }

            m_stringPipeline.start();
        }

        UDPReceiver::~UDPReceiver() {
            m_stringPipeline.stop();
        }

        void UDPReceiver::setPacketListener(PacketListener *pl) {
            m_packetListenerMutex->lock();
            {
                m_packetListener = pl;
            }
            m_packetListenerMutex->unlock();
        }

        void UDPReceiver::nextPacket(const Packet &p) {
            m_packetListenerMutex->lock();
            {
                // Pass packet either to packet listner or to string listener.
                if (m_packetListener != NULL) {
                    m_packetListener->nextPacket(p);
                }
                else {
                    m_stringPipeline.nextString(p.getData());
                }
            }
            m_packetListenerMutex->unlock();
        }

        void UDPReceiver::setStringListener(StringListener *sl) {
            m_stringPipeline.setStringListener(sl);
        }

    }
} // core::wrapper
